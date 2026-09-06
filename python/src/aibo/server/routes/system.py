from typing import Any

import sqlalchemy as sa
from fastapi import APIRouter, Depends, HTTPException, WebSocket, status
from sqlalchemy.ext.asyncio import AsyncSession
from starlette.websockets import WebSocketDisconnect

from aibo.bouncer.client import BouncerClient
from aibo.common.websocket import forward_events
from aibo.db.client import get_db
from aibo.server.dependencies import get_bouncer
from aibo.server.events import EventHub

router = APIRouter()


def compact_event(event: dict[str, Any]) -> dict[str, Any]:
    """Emacs reconciles persisted state; never send unused protocol bodies."""
    result = {
        key: value for key, value in event.items() if key not in {"params", "message"}
    }
    if isinstance(result.get("chat"), dict):
        result["chat"] = {
            key: value for key, value in result["chat"].items() if key != "messages"
        }
    return result


@router.websocket("/events")
async def events(websocket: WebSocket) -> None:
    await websocket.accept()
    hub: EventHub = websocket.app.state.event_hub
    try:
        async with hub.subscribe() as queue:
            await forward_events(
                websocket,
                queue,
                compact_event
                if websocket.query_params.get("compact") == "true"
                else None,
            )
    except WebSocketDisconnect:
        return


@router.get("/health")
async def health(
    db: AsyncSession = Depends(get_db),
    bouncer: BouncerClient = Depends(get_bouncer),
) -> dict[str, str]:
    await db.execute(sa.text("SELECT 1"))
    if not await bouncer.health():
        raise HTTPException(
            status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
            detail="bouncer unavailable",
        )
    return {"status": "ok"}
