import asyncio
from collections.abc import AsyncIterator
from contextlib import asynccontextmanager, suppress
from pathlib import Path
from typing import Awaitable, Callable

from fastapi import FastAPI, HTTPException, Request, Response
from fastapi.responses import FileResponse
from fastapi.staticfiles import StaticFiles

from aibo.bouncer.client import BouncerClient
from aibo.common.constants import WEB_DIR, Env
from aibo.common.log import configure_logging
from aibo.common.runtime_files import ensure_runtime_docs
from aibo.db import migrate_db
from aibo.server.events import BouncerEvents, EventHub
from aibo.server.routes.workspace_routes import router
from aibo.server.titles import TitleGenerator


def create_app() -> FastAPI:
    env = Env.get()
    configure_logging("server", env)
    ensure_runtime_docs(env)
    migrate_db()

    hub = EventHub()
    bouncer = BouncerEvents(env.BOUNCER_URL, hub)
    titles = TitleGenerator(BouncerClient(env.BOUNCER_URL, timeout=150), hub)

    @asynccontextmanager
    async def lifespan(app: FastAPI) -> AsyncIterator[None]:
        bouncer_task = asyncio.create_task(bouncer.run())
        yield
        bouncer_task.cancel()
        with suppress(asyncio.CancelledError):
            await bouncer_task
        await titles.close()

    app = FastAPI(title="aibo-server", lifespan=lifespan)
    app.state.event_hub = hub
    app.state.titles = titles
    app.include_router(router)

    @app.middleware("http")
    async def web_security_headers(
        request: Request, call_next: Callable[[Request], Awaitable[Response]]
    ) -> Response:
        response = await call_next(request)
        if request.url.path.startswith("/assets/") or (
            response.headers.get("content-type", "").startswith("text/html")
            and request.url.path not in ("/docs", "/redoc")
        ):
            response.headers["Content-Security-Policy"] = (
                "default-src 'self'; connect-src 'self' ws: wss:; "
                "img-src 'self' data:; style-src 'self'; script-src 'self'"
            )
            response.headers["X-Content-Type-Options"] = "nosniff"
            response.headers["Referrer-Policy"] = "no-referrer"
        return response

    @app.get("/tutorial", include_in_schema=False, response_class=FileResponse)
    @app.get("/", include_in_schema=False, response_class=FileResponse)
    async def web_app() -> FileResponse:
        return FileResponse(Path(WEB_DIR) / "index.html")

    app.mount(
        "/assets", StaticFiles(directory=Path(WEB_DIR) / "assets"), name="web-assets"
    )

    @app.get("/{path:path}", include_in_schema=False, response_class=FileResponse)
    async def web_route(path: str) -> FileResponse:
        if path.split("/", 1)[0] in ("api", "assets"):
            raise HTTPException(status_code=404, detail="Not found")
        return FileResponse(Path(WEB_DIR) / "index.html")

    return app
