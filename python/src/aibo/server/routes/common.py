from pathlib import Path

from fastapi import HTTPException, Request, status

from aibo.server.events import EventHub


def not_found(resource: str) -> HTTPException:
    return HTTPException(
        status_code=status.HTTP_404_NOT_FOUND, detail=f"{resource} not found"
    )


def normalized_path(path: str) -> str:
    return str(Path(path).expanduser().resolve(strict=False))


def publish(request: Request, event: dict[str, object]) -> None:
    hub: EventHub | None = getattr(request.app.state, "event_hub", None)
    if hub:
        hub.publish(event)
