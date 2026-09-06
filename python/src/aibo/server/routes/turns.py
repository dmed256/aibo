from pathlib import Path
from typing import Any, cast
from uuid import UUID

from fastapi import APIRouter, Depends, HTTPException, Request, status
from sqlalchemy.ext.asyncio import AsyncSession

from aibo.bouncer.client import BouncerClient, BouncerError
from aibo.core import workspace
from aibo.core.settings import get_models
from aibo.core.titles import needs_title
from aibo.db.client import get_db
from aibo.db.models import ChatModel
from aibo.server import history, schemas
from aibo.server.delegation import source_content
from aibo.server.dependencies import get_bouncer
from aibo.server.routes.attachments import validate_paths
from aibo.server.routes.common import not_found, publish
from aibo.server.titles import TitleGenerator

router = APIRouter()


async def set_native_goal(
    bouncer: BouncerClient, thread_id: str, objective: str | None
) -> dict[str, Any] | None:
    try:
        return cast(
            dict[str, Any] | None,
            (await bouncer.set_goal(thread_id, objective))["goal"],
        )
    except BouncerError as error:
        if error.status_code != 404:
            raise
        # Rolling backend upgrades must not interrupt the bouncer's active turns.
        # Keep the requested objective, but never present it as a running goal.
        return {"objective": objective, "status": "unavailable"} if objective else None


async def resume_chat(bouncer: BouncerClient, chat: ChatModel) -> None:
    if chat.codex_thread_id:
        try:
            resumed = await bouncer.resume_thread(chat.codex_thread_id)
            if "goal" in resumed:
                chat.goal = resumed["goal"]
        except BouncerError as error:
            never_started = not any(
                item.kind == workspace.MessageKind.ASSISTANT
                or item.data.get("event") in ("turn_started", "turn/completed")
                for item in chat.messages
            )
            if "no rollout found for thread id" not in str(error) or not never_started:
                raise
            chat.codex_thread_id = None
            chat.goal = None


@router.put("/chats/{chat_id}/goal", response_model=schemas.Chat)
async def set_chat_goal(
    chat_id: UUID,
    body: schemas.SetGoal,
    request: Request,
    db: AsyncSession = Depends(get_db),
    bouncer: BouncerClient = Depends(get_bouncer),
) -> schemas.Chat:
    chat = await workspace.get_chat(db, chat_id)
    if not chat:
        raise not_found("chat")
    last_user = next(
        (
            item
            for item in reversed(chat.messages)
            if item.kind == workspace.MessageKind.USER
        ),
        None,
    )
    objective = workspace.goal_objective(chat, last_user) if last_user else ""
    if body.enabled and not objective.strip():
        raise HTTPException(
            status_code=422, detail="Send a text message before setting a goal"
        )
    try:
        await resume_chat(bouncer, chat)
        if chat.codex_thread_id:
            chat.goal = await set_native_goal(
                bouncer, chat.codex_thread_id, objective if body.enabled else None
            )
        else:
            chat.goal = (
                {"objective": objective, "status": "pending"} if body.enabled else None
            )
    except BouncerError as error:
        raise HTTPException(status_code=502, detail=str(error)) from error
    chat.goal_enabled = body.enabled
    await db.commit()
    response = schemas.Chat.from_model(chat)
    publish(request, {"kind": "chat_updated", "chat": response.model_dump(mode="json")})
    return response


@router.post("/chats/{chat_id}/submit", response_model=schemas.StartedTurn)
async def submit_message(
    chat_id: UUID,
    body: schemas.SubmitMessage,
    request: Request,
    compact: bool = False,
    db: AsyncSession = Depends(get_db),
    bouncer: BouncerClient = Depends(get_bouncer),
) -> schemas.StartedTurn:
    chat = await workspace.get_chat(db, chat_id)
    if not chat:
        raise not_found("chat")
    message = next(
        (
            message
            for message in chat.messages
            if message.id == body.message_id
            and message.kind == workspace.MessageKind.USER
        ),
        None,
    )
    if body.message_id and not message:
        raise not_found("user message")
    if (
        body.m_context is not None or body.source_message_id
    ) and chat.kind != workspace.ChatKind.BOT:
        raise HTTPException(
            status_code=422, detail="delegation is only valid for bot chats"
        )
    text = message.content if message else body.text
    attachments = message.data.get("attachments", []) if message else body.attachments
    if body.source_message_id:
        text, attachments = await source_content(db, body.source_message_id)
    user_message = text
    if not isinstance(attachments, list) or not all(
        isinstance(path, str) for path in attachments
    ):
        raise HTTPException(
            status_code=422, detail="message attachments must be a list of paths"
        )
    if body.m_context is not None:
        text = workspace.delegation_message(text, body.m_context)
    cwd = (
        str(Path.home())
        if chat.kind == workspace.ChatKind.MANAGER
        else (chat.location.path if chat.location else None)
    )
    if not cwd or not Path(cwd).is_dir():
        notice = (
            "Blocked: the selected working directory does not exist."
            if chat.location
            else "Blocked: select a working directory before sending."
        )
        if (
            chat.status != workspace.ChatStatus.BLOCKED
            or workspace.chat_notice(chat)[0] != notice
        ):
            await workspace.set_status(db, chat, workspace.ChatStatus.BLOCKED)
            await workspace.append_message(
                db,
                chat,
                kind=workspace.MessageKind.EVENT,
                content=notice,
                data={"event": "location_unavailable"},
            )
            await workspace.create_notification(db, chat=chat, body=notice)
            await db.commit()
            publish(
                request,
                {
                    "kind": "chat_updated",
                    "chat": schemas.Chat.from_model(chat).model_dump(mode="json"),
                },
            )
        raise HTTPException(
            status_code=status.HTTP_422_UNPROCESSABLE_ENTITY,
            detail=notice,
        )
    validate_paths(attachments)

    context = await workspace.refresh_context(db, chat)
    models = await get_models(db)
    model = (
        models.manager_model
        if chat.kind == workspace.ChatKind.MANAGER
        else models.bot_model
    )
    effort = (
        models.manager_model_reasoning_effort
        if chat.kind == workspace.ChatKind.MANAGER
        else models.bot_model_reasoning_effort
    )
    if not message:
        message = await workspace.append_message(
            db,
            chat,
            kind=workspace.MessageKind.USER,
            content=text,
            data={
                "attachments": attachments,
                "user_message": user_message,
                **(
                    {"source_message_id": str(body.source_message_id)}
                    if body.source_message_id
                    else {}
                ),
            },
        )
    if body.goal is not None:
        chat.goal_enabled = body.goal
    elif body.m_context is not None:
        chat.goal_enabled = True
    await workspace.set_status(db, chat, workspace.ChatStatus.QUEUED)
    await db.commit()

    try:
        await resume_chat(bouncer, chat)
        if not chat.codex_thread_id:
            response = await bouncer.start_thread(
                cwd=cwd,
                instructions=chat.messages[0].content,
                model=model,
            )
            chat.codex_thread_id = response["thread"]["id"]
            await db.commit()
        thread_id = chat.codex_thread_id
        if not thread_id:
            raise KeyError("thread.id")
        objective = workspace.goal_objective(chat, message)
        if chat.goal_enabled and objective.strip():
            # Steering an active goal keeps its objective and accumulated usage.
            if (
                not chat.goal
                or chat.goal.get("status")
                in ("complete", "paused", "blocked", "unavailable", "pending")
                or body.goal is True
                or body.m_context is not None
            ):
                if (
                    chat.goal
                    and chat.goal.get("status")
                    in ("paused", "blocked", "unavailable", "pending")
                    and body.goal is not True
                    and body.m_context is None
                ):
                    objective = str(chat.goal["objective"])
                chat.goal = await set_native_goal(bouncer, thread_id, objective)
                await db.commit()
        elif not chat.goal_enabled and chat.goal:
            await set_native_goal(bouncer, thread_id, None)
            chat.goal = None
            await db.commit()
        response = await bouncer.start_turn(
            thread_id=thread_id,
            text=text,
            attachments=attachments,
            cwd=cwd,
            context=context,
            model=model,
            model_reasoning_effort=effort,
        )
        turn_id = str(response["turn"]["id"])
        chat.active_turn_id = turn_id
        await workspace.set_status(db, chat, workspace.ChatStatus.RUNNING)
        await workspace.append_message(
            db,
            chat,
            kind=workspace.MessageKind.EVENT,
            content="Codex turn started",
            data={"event": "turn_started", "turn_id": chat.active_turn_id},
        )
        await db.commit()
    except (BouncerError, KeyError) as error:
        await workspace.set_status(db, chat, workspace.ChatStatus.ERROR)
        notice = f"The turn could not continue: {str(error).rstrip('.')}."
        await workspace.append_message(
            db,
            chat,
            kind=workspace.MessageKind.ERROR,
            content=notice,
            data={"event": "turn_start_failed"},
        )
        await workspace.create_notification(db, chat=chat, body=notice)
        await db.commit()
        publish(
            request,
            {
                "kind": "chat_updated",
                "chat": schemas.Chat.from_model(chat).model_dump(mode="json"),
            },
        )
        raise HTTPException(
            status_code=status.HTTP_502_BAD_GATEWAY, detail=str(error)
        ) from error
    result = schemas.StartedTurn(
        chat=(
            await history.history(db, chat_id)
            if compact
            else schemas.Chat.from_model(chat)
        ),
        turn_id=turn_id,
    )
    publish(
        request,
        {
            "kind": "chat_updated",
            "chat": schemas.ChatSummary.from_model(chat).model_dump(mode="json"),
        },
    )
    titles: TitleGenerator | None = getattr(request.app.state, "titles", None)
    if titles and needs_title(chat.title):
        first_user = next(
            (item for item in chat.messages if item.kind == workspace.MessageKind.USER),
            None,
        )
        if first_user:
            original = first_user.data.get("user_message")
            titles.schedule(
                chat.id, original if isinstance(original, str) else first_user.content
            )
    return result
