import asyncio
import resource
import shlex
import sys
from pathlib import Path
from typing import Any
from unittest.mock import AsyncMock, Mock

import pytest
from httpx import ASGITransport, AsyncClient
from PIL import Image

from aibo.bouncer.codex import CodexAppServer, raise_file_limit, read_lines
from aibo.bouncer.main import create_app


@pytest.mark.parametrize(
    "soft,hard,expected",
    [(256, resource.RLIM_INFINITY, 8192), (256, 1024, 1024), (16384, 32768, None)],
)
def test_file_limit(
    monkeypatch: pytest.MonkeyPatch, soft: int, hard: int, expected: int | None
) -> None:
    monkeypatch.setattr(resource, "getrlimit", lambda _: (soft, hard))
    setter = Mock()
    monkeypatch.setattr(resource, "setrlimit", setter)
    raise_file_limit()
    if expected is None:
        setter.assert_not_called()
    else:
        setter.assert_called_once_with(resource.RLIMIT_NOFILE, (expected, hard))


async def test_model_override_and_reset(monkeypatch: pytest.MonkeyPatch) -> None:
    codex = CodexAppServer("unused")

    async def call(method: str, params: dict[str, Any], **_: Any) -> dict[str, Any]:
        if method == "config/read":
            assert params["cwd"] == "/tmp/aibo"
            return {
                "config": {
                    "model": "configured-model",
                    "model_reasoning_effort": "medium",
                }
            }
        return {"turn": {"id": "turn-1"}}

    calls = AsyncMock(side_effect=call)
    monkeypatch.setattr(codex, "call", calls)
    app = create_app(codex)
    async with AsyncClient(
        transport=ASGITransport(app=app), base_url="http://test"
    ) as client:
        for model in ("chosen-model", None):
            response = await client.post(
                "/threads/thread-1/turns",
                json={
                    "text": "Continue",
                    "cwd": "/tmp/aibo",
                    "model": model,
                    "model_reasoning_effort": "high" if model else None,
                },
            )
            assert response.status_code == 200
            assert calls.call_args.args[1]["model"] == (model or "configured-model")
            assert calls.call_args.args[1]["effort"] == ("high" if model else "medium")
    assert [call.args[0] for call in calls.call_args_list] == [
        "turn/start",
        "config/read",
        "config/read",
        "turn/start",
    ]


async def test_effort_uses_model_default(monkeypatch: pytest.MonkeyPatch) -> None:
    codex = CodexAppServer("unused")
    calls = AsyncMock(
        side_effect=[
            {"config": {"model": "configured", "model_reasoning_effort": None}},
            {
                "data": [
                    {
                        "model": "configured",
                        "defaultReasoningEffort": "medium",
                        "isDefault": True,
                    },
                    {"model": "chosen", "defaultReasoningEffort": "low"},
                ]
            },
        ]
    )
    monkeypatch.setattr(codex, "call", calls)
    assert await codex.default_effort("/tmp", "chosen") == "low"


async def test_title_uses_final_event(monkeypatch: pytest.MonkeyPatch) -> None:
    codex = CodexAppServer("unused")

    async def call(method: str, params: dict[str, Any], **_: Any) -> dict[str, Any]:
        if method == "thread/start":
            assert params["ephemeral"] is True
            assert params["model"] == "gpt-5.6-luna"
            assert params["config"] == {"model_reasoning_effort": "low"}
            return {"thread": {"id": "title-thread"}}
        if method == "turn/start":
            assert "3-6 word title" in params["input"][0]["text"]
            assert "Do not include hashtags" in params["input"][0]["text"]
            assert "User request" in params["input"][0]["text"]
            # Events may arrive before the turn/start RPC returns.
            for thread, phase, text in [
                ("other", "final_answer", "Wrong"),
                ("title-thread", "commentary", "Thinking"),
                ("title-thread", "final_answer", "Useful title #aibo #ui"),
            ]:
                codex._publish_event(
                    {
                        "method": "item/completed",
                        "params": {
                            "threadId": thread,
                            "turnId": "turn-1",
                            "item": {
                                "id": "item-1",
                                "type": "agentMessage",
                                "phase": phase,
                                "text": text,
                            },
                        },
                    }
                )
            codex._publish_event(
                {
                    "method": "turn/completed",
                    "params": {
                        "threadId": "title-thread",
                        "turn": {"id": "turn-1", "status": "completed"},
                    },
                }
            )
            return {"turn": {"id": "turn-1"}}
        return {}

    calls = AsyncMock(side_effect=call)
    monkeypatch.setattr(codex, "call", calls)
    app = create_app(codex)
    async with AsyncClient(
        transport=ASGITransport(app=app), base_url="http://test"
    ) as client:
        response = await asyncio.wait_for(
            client.post(
                "/titles",
                json={
                    "text": "User request",
                    "model": "gpt-5.6-luna",
                    "model_reasoning_effort": "low",
                },
            ),
            2,
        )
    assert response.json() == {"title": "Useful title #aibo #ui"}
    assert calls.call_args.args == (
        "thread/unsubscribe",
        {"threadId": "title-thread"},
    )
    assert not codex._subscribers


class FakeCodex:
    ready = True

    def __init__(self) -> None:
        self.calls: list[tuple[str, dict[str, Any]]] = []
        self.responses: list[tuple[int | str, dict[str, Any] | None]] = []

    async def start(self) -> None:
        pass

    async def shutdown(self) -> None:
        pass

    async def call(self, method: str, params: dict[str, Any]) -> dict[str, Any]:
        self.calls.append((method, params))
        return {"method": method}

    async def respond(
        self,
        request_id: int | str,
        *,
        result: dict[str, Any] | None = None,
        error: dict[str, Any] | None = None,
    ) -> None:
        if (result is None) == (error is None):
            raise ValueError("provide exactly one of result or error")
        self.responses.append((request_id, result if result is not None else error))


async def test_reject_non_image(tmp_path: Path) -> None:
    path = tmp_path / "image.png"
    path.write_text("SYNTHETIC NOT AN IMAGE")
    codex = FakeCodex()
    app = create_app(codex)  # type: ignore[arg-type]
    async with AsyncClient(
        transport=ASGITransport(app=app), base_url="http://test"
    ) as client:
        response = await client.post(
            "/threads/thread-1/turns", json={"attachments": [str(path)]}
        )
        assert response.status_code == 422
    assert not codex.calls


async def test_thread_and_turn_protocol_mapping(tmp_path: Path) -> None:
    path = tmp_path / "image.png"
    Image.new("RGB", (2, 2)).save(path)
    codex = FakeCodex()
    app = create_app(codex)  # type: ignore[arg-type]
    async with AsyncClient(
        transport=ASGITransport(app=app),
        base_url="http://test",
    ) as client:
        assert (await client.get("/health")).status_code == 200
        await client.post(
            "/threads",
            json={"cwd": "/tmp/aibo", "instructions": "Be concise"},
        )
        await client.post(
            "/threads/thread-1/turns",
            json={"text": "Fix it", "attachments": [str(path)]},
        )
        await client.post("/threads/thread-1/turns/turn-1/interrupt")
        await client.post(
            "/threads/thread-1/turns",
            json={"text": "Continue", "cwd": "/tmp/other", "context": "Project: aibo"},
        )
        response = await client.post("/responses", json={"request_id": 7, "result": {}})

    assert response.status_code == 200
    assert codex.calls == [
        (
            "thread/start",
            {"cwd": "/tmp/aibo", "baseInstructions": "Be concise"},
        ),
        (
            "turn/start",
            {
                "threadId": "thread-1",
                "input": [
                    {"type": "text", "text": "Fix it"},
                    {"type": "localImage", "path": str(path)},
                ],
            },
        ),
        (
            "turn/interrupt",
            {"threadId": "thread-1", "turnId": "turn-1"},
        ),
        (
            "turn/start",
            {
                "threadId": "thread-1",
                "input": [
                    {
                        "type": "text",
                        "text": "# Aibo workspace context\n\nProject: aibo",
                    },
                    {"type": "text", "text": "Continue"},
                ],
                "cwd": "/tmp/other",
            },
        ),
    ]
    assert codex.responses == [(7, {})]


async def test_app_server_stream_and_shutdown(tmp_path: Path) -> None:
    server = tmp_path / "fake_codex.py"
    server.write_text(
        """import json
import sys

for line in sys.stdin:
    message = json.loads(line)
    method = message.get("method")
    if "id" not in message:
        continue
    if method == "initialize":
        result = {"userAgent": "fake"}
    elif method == "thread/start":
        result = {"thread": {"id": "thread-1"}}
    elif method == "turn/start":
        result = {"turn": {"id": "turn-1", "status": "inProgress"}}
    elif method == "thread/resume":
        result = {"thread": {"id": "thread-1", "history": "猫" * 100000}}
    elif method == "config/read":
        files = [open(__file__) for _ in range(300)]
        result = {"openFiles": len(files)}
        for file in files:
            file.close()
    else:
        result = {}
    print(json.dumps({"id": message["id"], "result": result}), flush=True)
    if method == "turn/start":
        print(json.dumps({"method": "turn/started", "params": {
            "threadId": "thread-1",
            "turn": {"id": "turn-1", "status": "inProgress"}
        }}), flush=True)
        print("diagnostic " * 10000, file=sys.stderr, flush=True)
        print(json.dumps({"method": "item/completed", "params": {
            "threadId": "thread-1", "turnId": "turn-1",
            "item": {"id": "tool-1", "type": "commandExecution", "aggregatedOutput": "猫" * 100000}
        }}), flush=True)
        for delta in ("Fi", "xed"):
            print(json.dumps({"method": "item/agentMessage/delta", "params": {
                "threadId": "thread-1", "turnId": "turn-1", "itemId": "item-1", "delta": delta
            }}), flush=True)
        print(json.dumps({"method": "item/completed", "params": {
            "threadId": "thread-1", "turnId": "turn-1",
            "item": {"id": "item-1", "type": "agentMessage", "text": "Fixed"}
        }}), flush=True)
        print(json.dumps({"method": "turn/completed", "params": {
            "threadId": "thread-1",
            "turn": {"id": "turn-1", "status": "completed"}
        }}), flush=True)
"""
    )
    codex = CodexAppServer(shlex.join([sys.executable, str(server)]))
    limits = resource.getrlimit(resource.RLIMIT_NOFILE)
    try:
        resource.setrlimit(resource.RLIMIT_NOFILE, (256, limits[1]))
        await codex.start()
    finally:
        resource.setrlimit(resource.RLIMIT_NOFILE, limits)
    try:
        assert (await codex.call("config/read"))["openFiles"] == 300
        async with codex.events() as events:
            thread = await codex.call("thread/start", {"cwd": "/tmp"})
            turn = await codex.call(
                "turn/start",
                {"threadId": "thread-1", "input": [{"type": "text", "text": "go"}]},
            )
            started = await asyncio.wait_for(events.get(), 2)
            tool = await asyncio.wait_for(events.get(), 2)
            item = await asyncio.wait_for(events.get(), 2)
            completed = await asyncio.wait_for(events.get(), 2)
            resumed = await codex.call(
                "thread/resume", {"threadId": "thread-1"}, timeout=2
            )
            assert resumed["thread"]["history"] == "猫" * 100000
            assert tool["params"]["item"]["aggregatedOutput"] == "猫" * 100000
            assert codex.ready

        assert thread["thread"]["id"] == "thread-1"
        assert turn["turn"]["id"] == "turn-1"
        assert started["method"] == "turn/started"
        assert item["params"]["item"]["text"] == "Fixed"
        assert completed["params"]["turn"]["status"] == "completed"
    finally:
        await codex.shutdown()
    assert not codex.ready


async def test_subscribers_receive_complete_items() -> None:
    codex = CodexAppServer("unused")
    params = {"threadId": "thread", "turnId": "turn", "itemId": "item"}
    async with codex.events() as first, codex.events() as second:
        for method in ("item/agentMessage/delta", "item/reasoning/textDelta"):
            codex._publish_event(
                {"method": method, "params": {**params, "delta": "猫 café"}}
            )
        assert first.empty() and second.empty()
        completed = {
            "method": "item/completed",
            "params": {
                "threadId": "thread",
                "turnId": "turn",
                "item": {"id": "item", "type": "agentMessage", "text": "猫 café"},
            },
        }
        codex._publish_event(completed)
        assert first.get_nowait() == completed
        assert second.get_nowait() == completed
        assert first.empty() and second.empty()


async def test_fragmented_lines() -> None:
    stream = asyncio.StreamReader(limit=16)
    expected = ["猫" * 100000, "", "second", "last without newline"]
    encoded = "\n".join(expected).encode()

    async def feed() -> None:
        # Force boundaries inside UTF-8 and on either side of a newline.
        for start in range(0, len(encoded), 701):
            stream.feed_data(encoded[start : start + 701])
            await asyncio.sleep(0)
        stream.feed_eof()

    feeder = asyncio.create_task(feed())
    try:
        assert [line.decode() async for line in read_lines(stream)] == expected
    finally:
        await feeder


async def test_goal_protocol(monkeypatch: pytest.MonkeyPatch) -> None:
    codex = CodexAppServer("unused")
    calls = AsyncMock(
        return_value={"goal": {"objective": "Fix the UI", "status": "active"}}
    )
    monkeypatch.setattr(codex, "call", calls)
    async with AsyncClient(
        transport=ASGITransport(app=create_app(codex)), base_url="http://test"
    ) as client:
        response = await client.put(
            "/threads/t1/goal", json={"objective": "Fix the UI"}
        )
        assert response.status_code == 200
        calls.assert_awaited_with(
            "thread/goal/set",
            {"threadId": "t1", "objective": "Fix the UI", "status": "active"},
        )
        response = await client.put("/threads/t1/goal", json={"objective": None})
        assert response.json() == {"goal": None}
        calls.assert_awaited_with("thread/goal/clear", {"threadId": "t1"})
