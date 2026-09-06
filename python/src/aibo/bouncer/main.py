from __future__ import annotations

from collections.abc import AsyncIterator
from contextlib import asynccontextmanager
from pathlib import Path
from typing import Any, cast

from fastapi import APIRouter, FastAPI, HTTPException, Request, WebSocket
from fastapi.responses import JSONResponse
from pydantic import BaseModel, ConfigDict, Field, model_validator
from starlette.websockets import WebSocketDisconnect

from aibo.bouncer.codex import AppServerError, CodexAppServer
from aibo.common.constants import Env
from aibo.common.images import read_image
from aibo.common.log import configure_logging
from aibo.common.websocket import forward_events
from aibo.core.titles import TITLE_INSTRUCTIONS, title_prompt


class StartThread(BaseModel):
    model_config = ConfigDict(protected_namespaces=())
    cwd: str = Field(min_length=1)
    instructions: str | None = None
    model: str | None = None
    model_reasoning_effort: str | None = None


class StartTurn(BaseModel):
    model_config = ConfigDict(protected_namespaces=())
    text: str = ""
    attachments: list[str] = Field(default_factory=list)
    cwd: str | None = None
    context: str | None = None
    model: str | None = None
    model_reasoning_effort: str | None = None

    @model_validator(mode="after")
    def has_content(self) -> "StartTurn":
        if not self.text.strip() and not self.attachments:
            raise ValueError("Provide text or an attachment")
        return self


class ClientResponse(BaseModel):
    request_id: int | str
    result: dict[str, Any] | None = None
    error: dict[str, Any] | None = None


class SetGoal(BaseModel):
    objective: str | None = Field(default=None, min_length=1)


class GenerateTitle(BaseModel):
    model_config = ConfigDict(protected_namespaces=())
    text: str = Field(min_length=1)
    model: str | None = None
    model_reasoning_effort: str | None = None


def get_codex(request: Request) -> CodexAppServer:
    return cast(CodexAppServer, request.app.state.codex)


router = APIRouter()


@router.get("/health")
async def health(request: Request) -> dict[str, str]:
    if not get_codex(request).ready:
        raise HTTPException(status_code=503, detail="Codex app-server is unavailable")
    return {"status": "ok"}


@router.get("/account/rate-limits")
async def rate_limits(request: Request) -> dict[str, Any]:
    return await get_codex(request).rate_limits()


@router.post("/threads")
async def start_thread(request: Request, body: StartThread) -> dict[str, Any]:
    params: dict[str, Any] = {"cwd": body.cwd}
    if body.instructions:
        params["baseInstructions"] = body.instructions
    if body.model:
        params["model"] = body.model
    if body.model_reasoning_effort is not None:
        params["config"] = {"model_reasoning_effort": body.model_reasoning_effort}
    return await get_codex(request).call("thread/start", params)


@router.post("/threads/{thread_id}/resume")
async def resume_thread(request: Request, thread_id: str) -> dict[str, Any]:
    codex = get_codex(request)
    response = await codex.call("thread/resume", {"threadId": thread_id})
    response.update(await codex.call("thread/goal/get", {"threadId": thread_id}))
    return response


@router.post("/threads/{thread_id}/turns")
async def start_turn(
    request: Request, thread_id: str, body: StartTurn
) -> dict[str, Any]:
    for index, path in enumerate(body.attachments):
        try:
            if not Path(path).is_absolute():
                raise ValueError("attachment path must be absolute")
            read_image(Path(path))
        except (OSError, ValueError) as error:
            raise HTTPException(
                status_code=422,
                detail=f"attachment {index} must be a readable PNG, JPEG, GIF or WebP image",
            ) from error
    inputs: list[dict[str, str]] = []
    if body.context:
        inputs.append(
            {"type": "text", "text": "# Aibo workspace context\n\n" + body.context}
        )
    inputs.append({"type": "text", "text": body.text})
    inputs.extend({"type": "localImage", "path": path} for path in body.attachments)
    params: dict[str, Any] = {"threadId": thread_id, "input": inputs}
    if body.cwd is not None:
        params["cwd"] = body.cwd
    if "model" in body.model_fields_set:
        # Null resets a previously overridden thread to the current Codex setup.
        params["model"] = body.model or await get_codex(request).default_model(body.cwd)
    if "model_reasoning_effort" in body.model_fields_set:
        params["effort"] = body.model_reasoning_effort or await get_codex(
            request
        ).default_effort(body.cwd, params.get("model"))
    return await get_codex(request).call("turn/start", params)


@router.get("/threads/{thread_id}/goal")
async def get_goal(request: Request, thread_id: str) -> dict[str, Any]:
    return await get_codex(request).call("thread/goal/get", {"threadId": thread_id})


@router.put("/threads/{thread_id}/goal")
async def set_goal(request: Request, thread_id: str, body: SetGoal) -> dict[str, Any]:
    if body.objective is None:
        await get_codex(request).call("thread/goal/clear", {"threadId": thread_id})
        return {"goal": None}
    return await get_codex(request).call(
        "thread/goal/set",
        {"threadId": thread_id, "objective": body.objective, "status": "active"},
    )


@router.post("/titles")
async def generate_title(request: Request, body: GenerateTitle) -> dict[str, str]:
    title = await get_codex(request).generate_text(
        instructions=TITLE_INSTRUCTIONS,
        text=title_prompt(body.text),
        model=body.model,
        model_reasoning_effort=body.model_reasoning_effort,
    )
    return {"title": title}


@router.post("/threads/{thread_id}/turns/{turn_id}/interrupt")
async def interrupt_turn(
    request: Request, thread_id: str, turn_id: str
) -> dict[str, Any]:
    return await get_codex(request).call(
        "turn/interrupt", {"threadId": thread_id, "turnId": turn_id}
    )


@router.post("/responses")
async def respond(request: Request, body: ClientResponse) -> dict[str, str]:
    try:
        await get_codex(request).respond(
            body.request_id, result=body.result, error=body.error
        )
    except ValueError as error:
        raise HTTPException(status_code=422, detail=str(error)) from error
    return {"status": "ok"}


@router.websocket("/events")
async def events(websocket: WebSocket) -> None:
    await websocket.accept()
    codex: CodexAppServer = websocket.app.state.codex
    try:
        async with codex.events() as queue:
            await forward_events(websocket, queue)
    except WebSocketDisconnect:
        return


def create_app(codex: CodexAppServer | None = None) -> FastAPI:
    env = Env.get()
    configure_logging("bouncer", env)
    codex_server = codex or CodexAppServer(env.CODEX_COMMAND)

    @asynccontextmanager
    async def lifespan(app: FastAPI) -> AsyncIterator[None]:
        app.state.codex = codex_server
        try:
            await codex_server.start()
        except AppServerError:
            await codex_server.shutdown()
            raise
        yield
        await codex_server.shutdown()

    app = FastAPI(title="aibo-codex-bouncer", lifespan=lifespan)
    app.state.codex = codex_server
    app.include_router(router)

    @app.exception_handler(AppServerError)
    async def app_server_error(request: Request, error: AppServerError) -> JSONResponse:
        return JSONResponse(status_code=502, content={"detail": str(error)})

    return app
