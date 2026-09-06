import asyncio
from typing import Any, cast

from fastapi import WebSocket

from aibo.common.websocket import forward_events


class FakeWebSocket:
    def __init__(self) -> None:
        self.disconnected = asyncio.Event()
        self.sent = asyncio.Event()
        self.messages: list[dict[str, Any]] = []

    async def receive(self) -> dict[str, str]:
        await self.disconnected.wait()
        return {"type": "websocket.disconnect"}

    async def send_json(self, message: dict[str, Any]) -> None:
        self.messages.append(message)
        self.sent.set()


async def test_event_forwarding_drains_after_idle_disconnect() -> None:
    websocket = FakeWebSocket()
    queue: asyncio.Queue[dict[str, Any]] = asyncio.Queue()
    task = asyncio.create_task(forward_events(cast(WebSocket, websocket), queue))

    await queue.put({"kind": "changed"})
    await asyncio.wait_for(websocket.sent.wait(), 0.1)
    websocket.disconnected.set()
    await asyncio.wait_for(task, 0.1)

    assert websocket.messages == [{"kind": "changed"}]
