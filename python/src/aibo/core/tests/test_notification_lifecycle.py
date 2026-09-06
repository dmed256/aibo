from collections.abc import AsyncIterator
from contextlib import asynccontextmanager

import pytest
import sqlalchemy as sa
from sqlalchemy.ext.asyncio import AsyncSession

from aibo.core import workspace
from aibo.db.models import ChatModel, NotificationModel
from aibo.server.events import BouncerEvents, EventHub


@pytest.mark.parametrize("late_goal", [False, True])
@pytest.mark.parametrize("goal_status", ["complete", "blocked"])
async def test_one_notification_per_goal(
    db: AsyncSession, monkeypatch: pytest.MonkeyPatch, late_goal: bool, goal_status: str
) -> None:
    chat = await workspace.create_chat(db, kind=workspace.ChatKind.BOT)
    chat.codex_thread_id = "goal-thread"
    chat.goal = {"objective": "Finish the work", "status": "active", "createdAt": 1}
    await db.commit()
    events = BouncerEvents("http://unused", EventHub())

    @asynccontextmanager
    async def session() -> AsyncIterator[AsyncSession]:
        yield db

    monkeypatch.setattr("aibo.server.events.get_session", session)

    async def send(method: str, **params: object) -> None:
        await events._persist(
            {"method": method, "params": {"threadId": "goal-thread", **params}}
        )

    async def finish(turn: str) -> None:
        await send("turn/completed", turn={"id": turn, "status": "completed"})

    try:
        for i in range(12):
            await send("turn/started", turn={"id": str(i)})
            await finish(str(i))
        await events._repair_notifications()
        assert (
            await db.scalar(sa.select(sa.func.count()).select_from(NotificationModel))
            == 0
        )
        assert (
            await db.scalar(
                sa.select(sa.func.count())
                .select_from(ChatModel)
                .where(ChatModel.kind == "shadow")
            )
            == 0
        )

        await send("turn/started", turn={"id": "final"})
        await send(
            "item/completed",
            turnId="final",
            item={"id": "answer", "type": "agentMessage", "text": "Work is finished."},
        )
        goal = {**chat.goal, "status": goal_status}
        if late_goal:
            await finish("final")
        await send("thread/goal/updated", goal=goal)
        if not late_goal:
            await finish("final")
        for _ in range(2):
            await finish("final")
            for i in range(12):
                await finish(str(i))
            await send("thread/goal/updated", goal=goal)
            await events._repair_notifications()
        notes = list((await db.scalars(sa.select(NotificationModel))).all())
        assert len(notes) == 1
        assert notes[0].body == "Work is finished."
        assert (
            await db.scalar(
                sa.select(sa.func.count())
                .select_from(ChatModel)
                .where(ChatModel.kind == "shadow")
            )
            == 1
        )
    finally:
        await events.client.close()


@pytest.mark.parametrize("status", ["failed", "interrupted"])
async def test_failures_still_notify(
    db: AsyncSession, monkeypatch: pytest.MonkeyPatch, status: str
) -> None:
    chat = await workspace.create_chat(db, kind=workspace.ChatKind.BOT)
    chat.codex_thread_id = "failure-thread"
    chat.goal = {"objective": "Work", "status": "active"}
    await db.commit()
    events = BouncerEvents("http://unused", EventHub())

    @asynccontextmanager
    async def session() -> AsyncIterator[AsyncSession]:
        yield db

    monkeypatch.setattr("aibo.server.events.get_session", session)
    try:
        await events._persist(
            {
                "method": "turn/completed",
                "params": {
                    "threadId": chat.codex_thread_id,
                    "turn": {"id": "failure", "status": status},
                },
            }
        )
        await events._repair_notifications()
        assert (
            await db.scalar(sa.select(sa.func.count()).select_from(NotificationModel))
            == 1
        )
    finally:
        await events.client.close()
