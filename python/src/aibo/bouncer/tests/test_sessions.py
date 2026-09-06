import asyncio
from typing import Any
from unittest.mock import AsyncMock

import pytest

from aibo.bouncer.codex import AppServerError, CodexAppServer


@pytest.mark.parametrize(
    "status,goal,release",
    [
        ("idle", None, True),
        ("active", None, False),
        ("idle", "active", False),
        ("idle", "complete", True),
        ("idle", "blocked", True),
        ("unknown", None, False),
    ],
)
async def test_idle_cleanup(
    monkeypatch: pytest.MonkeyPatch, status: str, goal: str | None, release: bool
) -> None:
    codex = CodexAppServer("unused")
    codex._thread_activity = {"thread": 0}

    async def call(method: str, *_: Any) -> dict[str, Any]:
        if method == "thread/read":
            return {"thread": {"status": {"type": status}}}
        if method == "thread/goal/get":
            return {"goal": {"status": goal} if goal else None}
        return {"status": "unsubscribed"}

    rpc = AsyncMock(side_effect=call)
    monkeypatch.setattr(codex, "_call", rpc)
    await codex.release_idle_threads()
    assert ("thread" not in codex._thread_activity) == release
    assert any(c.args[0] == "thread/unsubscribe" for c in rpc.call_args_list) == release


async def test_recent_activity(monkeypatch: pytest.MonkeyPatch) -> None:
    codex = CodexAppServer("unused")
    codex._thread_activity = {"thread": 0}
    codex._publish_event(
        {"method": "item/agentMessage/delta", "params": {"threadId": "thread"}}
    )
    rpc = AsyncMock()
    monkeypatch.setattr(codex, "_call", rpc)
    await codex.release_idle_threads()
    rpc.assert_not_called()


async def test_activity_during_cleanup(monkeypatch: pytest.MonkeyPatch) -> None:
    codex = CodexAppServer("unused")
    codex._thread_activity = {"thread": 0}

    async def call(method: str, *_: Any) -> dict[str, Any]:
        if method == "thread/read":
            return {"thread": {"status": {"type": "idle"}}}
        assert method == "thread/goal/get"
        codex._publish_event(
            {"method": "turn/started", "params": {"threadId": "thread"}}
        )
        return {"goal": None}

    monkeypatch.setattr(codex, "_call", call)
    await codex.release_idle_threads()
    assert "thread" in codex._thread_activity


async def test_submit_during_cleanup(monkeypatch: pytest.MonkeyPatch) -> None:
    codex = CodexAppServer("unused")
    codex._thread_activity = {"thread": 0}
    checking = asyncio.Event()
    proceed = asyncio.Event()
    calls = []

    async def call(method: str, *_: Any) -> dict[str, Any]:
        calls.append(method)
        if method == "thread/read":
            checking.set()
            await proceed.wait()
            return {"thread": {"status": {"type": "idle"}}}
        return {}

    monkeypatch.setattr(codex, "_call", call)
    async with asyncio.timeout(2):
        cleanup = asyncio.create_task(codex.release_idle_threads())
        await checking.wait()
        submit = asyncio.create_task(codex.call("turn/start", {"threadId": "thread"}))
        await asyncio.sleep(0)
        assert not submit.done()
        proceed.set()
        await asyncio.gather(cleanup, submit)
    assert calls == [
        "thread/read",
        "thread/goal/get",
        "thread/unsubscribe",
        "thread/resume",
        "turn/start",
    ]
    assert "thread" in codex._thread_activity
    assert not codex._thread_locks


async def test_cleanup_retries(monkeypatch: pytest.MonkeyPatch) -> None:
    codex = CodexAppServer("unused")
    codex._thread_activity = {"failed": 0, "idle": 0}
    failed = True

    async def call(method: str, params: dict[str, Any]) -> dict[str, Any]:
        if method == "thread/goal/get" and params["threadId"] == "failed" and failed:
            raise AppServerError("unavailable")
        return {"thread": {"status": {"type": "idle"}}}

    monkeypatch.setattr(codex, "_call", call)
    await codex.release_idle_threads()
    assert codex._thread_activity == {"failed": 0}
    failed = False
    await codex.release_idle_threads()
    assert not codex._thread_activity
