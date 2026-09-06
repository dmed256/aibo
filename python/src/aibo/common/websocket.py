import asyncio
from collections.abc import Callable, Mapping
from typing import Any

from fastapi import WebSocket


async def forward_events(
    websocket: WebSocket,
    queue: asyncio.Queue[dict[str, Any]],
    transform: Callable[[dict[str, Any]], dict[str, Any]] | None = None,
) -> None:
    while True:
        event_task = asyncio.create_task(queue.get())
        receive_task = asyncio.create_task(websocket.receive())
        done, pending = await asyncio.wait(
            {event_task, receive_task}, return_when=asyncio.FIRST_COMPLETED
        )
        for task in pending:
            task.cancel()
        await asyncio.gather(*pending, return_exceptions=True)

        if receive_task in done:
            message: Mapping[str, Any] = receive_task.result()
            if message.get("type") == "websocket.disconnect":
                return
        if event_task in done:
            event = event_task.result()
            await websocket.send_json(transform(event) if transform else event)
