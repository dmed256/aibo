from collections.abc import AsyncIterator
from contextlib import asynccontextmanager
from pathlib import Path
from typing import Any

import pytest
from fastapi import FastAPI
from httpx import ASGITransport, AsyncClient
from sqlalchemy.ext.asyncio import AsyncSession

from aibo.core import workspace
from aibo.core.tests.fakes import FakeBouncer
from aibo.db.client import get_db
from aibo.db.models import LocationModel
from aibo.server.dependencies import get_bouncer
from aibo.server.events import BouncerEvents, EventHub
from aibo.server.routes.workspace_routes import router
from aibo.server.schemas import Chat


async def test_goal_defaults_and_last_message(db: AsyncSession, tmp_path: Path) -> None:
    location = LocationModel(name="goals", path=str(tmp_path))
    db.add(location)
    await db.commit()
    bouncer = FakeBouncer()
    app = FastAPI()
    app.include_router(router)

    async def test_db() -> AsyncIterator[AsyncSession]:
        yield db

    app.dependency_overrides[get_db] = test_db
    app.dependency_overrides[get_bouncer] = lambda: bouncer
    async with AsyncClient(
        transport=ASGITransport(app=app), base_url="http://test"
    ) as client:
        created = (
            await client.post(
                "/api/chats",
                json={
                    "kind": "bot",
                    "location_id": str(location.id),
                    "goal": False,
                    "user_message": "  Fix the UI.\nKeep 猫.",
                    "m_context": "Relevant context",
                },
            )
        ).json()
        path = f"/api/chats/{created['id']}"
        assert created["goal_enabled"] is False
        sent = await client.post(
            path + "/submit", json={"message_id": created["messages"][-1]["id"]}
        )
        assert sent.status_code == 200
        assert bouncer.goals == []
        enabled = await client.put(path + "/goal", json={"enabled": True})
        assert enabled.status_code == 200
        assert bouncer.goals == ["  Fix the UI.\nKeep 猫."]
        assert enabled.json()["goal"]["objective"] == bouncer.goals[-1]
        count = len(enabled.json()["messages"])
        await client.post(path + "/submit", json={"text": "Also check scrolling"})
        assert len(bouncer.goals) == 1  # Steering preserves the original goal.
        updated = await client.put(path + "/goal", json={"enabled": True})
        assert bouncer.goals[-1] == "Also check scrolling"
        assert len(updated.json()["messages"]) == count + 2  # Only user + turn marker.
        await client.post(
            path + "/submit",
            json={"text": "Just answer", "m_context": "", "goal": False},
        )
        assert bouncer.goals[-1] is None
        resumed = await client.post(
            path + "/submit", json={"text": "New task", "m_context": ""}
        )
        assert resumed.json()["chat"]["goal_enabled"] is True
        assert bouncer.goals[-1] == "New task"
        empty = (await client.post("/api/chats", json={"kind": "manager"})).json()
        assert empty["goal_enabled"] is True
        assert (
            await client.put(f"/api/chats/{empty['id']}/goal", json={"enabled": True})
        ).status_code == 422
    shadow = await workspace.create_chat(db, kind=workspace.ChatKind.SHADOW)
    assert shadow.goal_enabled is False


async def test_manager_goal_is_coordination(db: AsyncSession) -> None:
    chat = await workspace.create_chat(db, kind=workspace.ChatKind.MANAGER)
    bouncer = FakeBouncer()
    app = FastAPI()
    app.include_router(router)

    async def test_db() -> AsyncIterator[AsyncSession]:
        yield db

    app.dependency_overrides[get_db] = test_db
    app.dependency_overrides[get_bouncer] = lambda: bouncer
    request = "  Fix the UI.\nKeep 猫."
    async with AsyncClient(
        transport=ASGITransport(app=app), base_url="http://test"
    ) as client:
        result = await client.post(
            f"/api/chats/{chat.id}/submit", json={"text": request}
        )
        assert result.status_code == 200
        goal = bouncer.goals[-1]
        assert goal and "complete this coordination goal" in goal
        assert "Do not wait for implementation" in goal
        assert goal.endswith("User request:\n" + request)
        assert bouncer.turns[-1]["text"] == request
        await client.put(f"/api/chats/{chat.id}/goal", json={"enabled": True})
        assert bouncer.goals[-1] == goal


async def test_goal_usage_and_elapsed(
    db: AsyncSession, monkeypatch: pytest.MonkeyPatch
) -> None:
    import datetime as dt

    chat = await workspace.create_chat(db, kind=workspace.ChatKind.BOT)
    events = BouncerEvents("http://unused", EventHub())
    start = workspace.now_utc()
    monkeypatch.setattr(workspace, "now_utc", lambda: start)
    try:
        await events._apply(db, chat, "turn/started", {"turn": {"id": "t1"}})
        # Duplicate lifecycle and cumulative usage events must never double-count.
        await events._apply(db, chat, "turn/started", {"turn": {"id": "t1"}})
        for _ in range(2):
            await events._apply(
                db,
                chat,
                "thread/tokenUsage/updated",
                {"tokenUsage": {"total": {"totalTokens": 234567}}},
            )
        goal = {"objective": "Fix the UI", "status": "blocked"}
        await events._apply(db, chat, "thread/goal/updated", {"goal": goal})
        assert Chat.from_model(chat).goal == goal
        monkeypatch.setattr(
            workspace, "now_utc", lambda: start + dt.timedelta(seconds=36)
        )
        for _ in range(2):
            await events._apply(
                db,
                chat,
                "turn/completed",
                {"turn": {"id": "t1", "status": "completed"}},
            )
        assert chat.elapsed_seconds == 36
        assert chat.tokens_used == 234567
        assert chat.running_since is None
        monkeypatch.setattr(workspace, "now_utc", lambda: start + dt.timedelta(hours=1))
        await events._apply(db, chat, "turn/started", {"turn": {"id": "t2"}})
        monkeypatch.setattr(
            workspace, "now_utc", lambda: start + dt.timedelta(hours=1, seconds=90)
        )
        await events._apply(
            db, chat, "turn/completed", {"turn": {"id": "t2", "status": "completed"}}
        )
        assert chat.elapsed_seconds == 126
        await events._apply(db, chat, "thread/goal/cleared", {})
        assert chat.goal is None and chat.goal_enabled is False
    finally:
        await events.client.close()


async def test_goal_recovered_between_turns(
    db: AsyncSession, monkeypatch: pytest.MonkeyPatch
) -> None:
    import aibo.server.events as events_module

    chat = await workspace.create_chat(db, kind=workspace.ChatKind.MANAGER)
    chat.codex_thread_id = "goal-recovery"
    chat.goal = {"objective": "Fix the UI", "status": "active"}
    await workspace.set_status(db, chat, workspace.ChatStatus.COMPLETED)
    await db.commit()
    bouncer = BouncerEvents("http://unused", EventHub())

    @asynccontextmanager
    async def test_session() -> AsyncIterator[AsyncSession]:
        yield db

    async def resume(_thread_id: str) -> dict[str, Any]:
        return {
            "thread": {"turns": [], "status": {"type": "idle"}},
            "goal": {"objective": "Fix the UI", "status": "complete"},
        }

    monkeypatch.setattr(events_module, "get_session", test_session)
    monkeypatch.setattr(bouncer.client, "resume_thread", resume)
    try:
        async with bouncer.hub.subscribe() as queue:
            await bouncer._reconcile()
            assert queue.get_nowait()["method"] == "thread/goal/updated"
            assert queue.empty()
        assert chat.goal["status"] == "complete"
        assert chat.status == workspace.ChatStatus.COMPLETED
    finally:
        await bouncer.client.close()
