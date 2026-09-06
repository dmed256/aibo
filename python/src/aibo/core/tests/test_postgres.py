import asyncio
import datetime as dt
import os
from collections.abc import AsyncIterator
from contextlib import asynccontextmanager, suppress
from pathlib import Path
from typing import Any, cast
from unittest.mock import AsyncMock
from uuid import uuid4

import pytest
import sqlalchemy as sa
from sqlalchemy.ext.asyncio import (
    AsyncEngine,
    AsyncSession,
    async_sessionmaker,
    create_async_engine,
)

from aibo.core import workspace
from aibo.db.models import BaseDBModel, ChatModel, CounterModel, NotificationModel
from aibo.server.events import BouncerEvents, EventHub
from aibo.server.titles import TitleGenerator


@pytest.fixture
async def notification_engine() -> AsyncIterator[AsyncEngine]:
    if not os.environ.get("AIBO_POSTGRES_TEST_URL"):
        pytest.skip("AIBO_POSTGRES_TEST_URL is not set")
    schema = f"aibo_test_{uuid4().hex}"
    admin = create_async_engine(os.environ["AIBO_POSTGRES_TEST_URL"])
    async with admin.begin() as connection:
        await connection.execute(sa.text(f"CREATE SCHEMA {schema}"))
    engine = create_async_engine(
        os.environ["AIBO_POSTGRES_TEST_URL"],
        connect_args={"server_settings": {"search_path": schema}},
    )
    try:
        async with engine.begin() as connection:
            await connection.run_sync(BaseDBModel.metadata.create_all)
        yield engine
    finally:
        await engine.dispose()
        async with admin.begin() as connection:
            await connection.execute(sa.text(f"DROP SCHEMA {schema} CASCADE"))
        await admin.dispose()


async def test_titles_reach_both_workers(notification_engine: AsyncEngine) -> None:
    factory = async_sessionmaker(notification_engine, expire_on_commit=False)
    workers = [BouncerEvents("http://unused", EventHub()) for _ in range(2)]
    client = AsyncMock()
    client.generate_title.return_value = "New title #aibo #ui"
    titles = TitleGenerator(client, workers[0].hub, factory)
    try:
        async with factory() as db:
            chat = await workspace.create_chat(db, kind=workspace.ChatKind.MANAGER)
            await db.commit()
        async with workers[0].hub.subscribe() as first, workers[
            1
        ].hub.subscribe() as second:
            async with workers[0]._notification_listener(notification_engine), workers[
                1
            ]._notification_listener(notification_engine):
                await titles.generate(chat.id, "User request")
                for queue in (first, second):
                    assert await asyncio.wait_for(queue.get(), 2) == {
                        "kind": "chat_title_updated",
                        "chat_id": str(chat.id),
                        "title": "New title",
                    }
                    assert queue.empty()
    finally:
        await titles.close()
        for worker in workers:
            await worker.client.close()


async def test_notifications_reach_both_workers(
    notification_engine: AsyncEngine,
) -> None:
    engine = notification_engine
    factory = async_sessionmaker(engine, expire_on_commit=False)
    bouncers = [BouncerEvents("http://unused", EventHub()) for _ in range(2)]
    try:
        async with bouncers[0].hub.subscribe() as first, bouncers[
            1
        ].hub.subscribe() as second:
            async with bouncers[0]._notification_listener(engine), bouncers[
                1
            ]._notification_listener(engine):
                async with factory() as db:
                    chat = await workspace.create_chat(db, kind=workspace.ChatKind.BOT)
                    await db.commit()
                    note = await workspace.create_notification(
                        db, chat=chat, body="Recovered outcome"
                    )
                    assert first.empty() and second.empty()
                    await db.commit()
                    expected = {
                        "kind": "notification_created",
                        "notification_id": str(note.id),
                        "chat_id": str(chat.id),
                    }
                    assert await asyncio.wait_for(first.get(), 2) == expected
                    assert await asyncio.wait_for(second.get(), 2) == expected
                    chat_id = chat.id
                    await workspace.create_notification(
                        db, chat=chat, body="Rolled back"
                    )
                    await db.rollback()
                    reloaded = await workspace.get_chat(db, chat_id)
                    assert reloaded
                    chat = reloaded
                    committed = await workspace.create_notification(
                        db, chat=chat, body="Committed"
                    )
                    await db.commit()
                    for queue in (first, second):
                        event = await asyncio.wait_for(queue.get(), 2)
                        assert event["notification_id"] == str(committed.id)
                        assert queue.empty()
    finally:
        for bouncer in bouncers:
            await bouncer.client.close()


async def test_shadow_repair(
    notification_engine: AsyncEngine, monkeypatch: pytest.MonkeyPatch
) -> None:
    factory = async_sessionmaker(notification_engine, expire_on_commit=False)
    bouncers = [BouncerEvents("http://unused", EventHub()) for _ in range(2)]

    @asynccontextmanager
    async def session() -> AsyncIterator[AsyncSession]:
        async with factory() as db:
            yield db

    monkeypatch.setattr("aibo.server.events.get_session", session)
    try:
        async with factory() as db:
            source = await workspace.create_chat(db, kind=workspace.ChatKind.BOT)
            source_id = source.id
            await workspace.append_message(
                db,
                source,
                kind=workspace.MessageKind.ASSISTANT,
                content="The sidebar now retains its position.",
                data={"turn_id": "lost"},
            )
            await bouncers[0]._apply(
                db,
                source,
                "turn/completed",
                {"turn": {"id": "lost", "status": "completed"}},
            )
            await db.commit()
        # Simulate death after completion commit but before shadow creation.
        await asyncio.gather(*(bouncer._repair_notifications() for bouncer in bouncers))
        async with factory() as db:
            shadows = list(
                (
                    await db.scalars(
                        sa.select(ChatModel).where(
                            ChatModel.origin_chat_id == source_id
                        )
                    )
                ).all()
            )
            notes = list(
                (
                    await db.scalars(
                        sa.select(NotificationModel).where(
                            NotificationModel.chat_id == source_id
                        )
                    )
                ).all()
            )
            assert len(shadows) == len(notes) == 1
            assert notes[0].body == "The sidebar now retains its position."
            abandoned = await workspace.create_chat(
                db, kind=workspace.ChatKind.SHADOW, user_message="Summarize"
            )
            abandoned.origin_chat_id = source_id
            abandoned.shadow_for_turn_id = "abandoned"
            abandoned.messages[-1].data = {
                "outcome": "Tests pass.",
                "source_status": "interrupted",
            }
            abandoned.last_status_change_at = workspace.now_utc() - dt.timedelta(
                minutes=3
            )
            fresh = await workspace.create_chat(
                db, kind=workspace.ChatKind.SHADOW, user_message="Starting"
            )
            fresh.origin_chat_id = source_id
            fresh.shadow_for_turn_id = "fresh"
            await workspace.set_status(db, fresh, workspace.ChatStatus.QUEUED)
            fresh_id = fresh.id
            await db.commit()
        await asyncio.gather(
            *(bouncer._repair_notifications() for bouncer in bouncers for _ in range(2))
        )
        async with factory() as db:
            notes = list(
                (
                    await db.scalars(
                        sa.select(NotificationModel).where(
                            NotificationModel.chat_id == source_id
                        )
                    )
                ).all()
            )
            assert len(notes) == 2
            assert sorted(note.body for note in notes) == [
                "The sidebar now retains its position.",
                "b0 interrupted: Tests pass.",
            ]
            fresh = await db.get(ChatModel, fresh_id)
            assert fresh and fresh.status == workspace.ChatStatus.QUEUED
    finally:
        for bouncer in bouncers:
            await bouncer.client.close()


async def test_shadow_resume_race(
    notification_engine: AsyncEngine, monkeypatch: pytest.MonkeyPatch
) -> None:
    factory = async_sessionmaker(notification_engine, expire_on_commit=False)
    bouncers = [BouncerEvents("http://unused", EventHub()) for _ in range(2)]
    snapshot_read, completion_saved = asyncio.Event(), asyncio.Event()

    @asynccontextmanager
    async def session() -> AsyncIterator[AsyncSession]:
        async with factory() as db:
            yield db

    async def active(_thread: str) -> dict[str, Any]:
        snapshot_read.set()
        await completion_saved.wait()
        return {
            "thread": {
                "status": {"type": "active"},
                "turns": [
                    {
                        "id": "resumed",
                        "status": "inProgress",
                        "items": [
                            {"id": "answer", "type": "agentMessage", "text": "Partial"}
                        ],
                    }
                ],
            }
        }

    async def finished(_thread: str) -> dict[str, Any]:
        return {
            "thread": {
                "status": {"type": "idle"},
                "turns": [
                    {
                        "id": "resumed",
                        "status": "completed",
                        "items": [
                            {
                                "id": "answer",
                                "type": "agentMessage",
                                "text": "Recovered concise result",
                            }
                        ],
                    }
                ],
            }
        }

    monkeypatch.setattr("aibo.server.events.get_session", session)
    monkeypatch.setattr(bouncers[0].client, "resume_thread", active)
    monkeypatch.setattr(bouncers[1].client, "resume_thread", finished)
    try:
        async with factory() as db:
            source = await workspace.create_chat(db, kind=workspace.ChatKind.BOT)
            shadow = await workspace.create_chat(
                db, kind=workspace.ChatKind.SHADOW, user_message="Summarize"
            )
            shadow.origin_chat_id = source.id
            shadow.shadow_for_turn_id = "source-turn"
            shadow.codex_thread_id = "known-thread"
            shadow.last_status_change_at = workspace.now_utc() - dt.timedelta(minutes=3)
            shadow_id = shadow.id
            await db.commit()
        pending = asyncio.create_task(bouncers[0]._reconcile())
        try:
            await asyncio.wait_for(snapshot_read.wait(), 2)
            await bouncers[1]._reconcile()
        finally:
            completion_saved.set()
            await pending
        await bouncers[0]._repair_notifications()
        async with factory() as db:
            shadow = await db.get(ChatModel, shadow_id)
            assert (
                shadow
                and shadow.status == workspace.ChatStatus.COMPLETED
                and shadow.active_turn_id is None
            )
            notes = list((await db.scalars(sa.select(NotificationModel))).all())
            assert len(notes) == 1 and notes[0].body == "Recovered concise result"
    finally:
        for bouncer in bouncers:
            await bouncer.client.close()


async def test_listener_gap(
    notification_engine: AsyncEngine, monkeypatch: pytest.MonkeyPatch
) -> None:
    from aibo.server.routes.notifications import list_notifications

    engine = notification_engine
    factory = async_sessionmaker(engine, expire_on_commit=False)
    bouncer = BouncerEvents("http://unused", EventHub())
    original = bouncer._notification_listener
    disconnected, between_connections, resume_listener = (
        asyncio.Event(),
        asyncio.Event(),
        asyncio.Event(),
    )
    attempts = 0

    @asynccontextmanager
    async def listener(engine: AsyncEngine) -> AsyncIterator[asyncio.Event]:
        nonlocal attempts
        attempts += 1
        if attempts == 2:
            between_connections.set()
            await resume_listener.wait()
        async with original(engine):
            yield disconnected

    monkeypatch.setattr("aibo.server.events.get_async_engine", lambda: engine)
    monkeypatch.setattr(bouncer, "_notification_listener", listener)
    try:
        async with bouncer.hub.subscribe() as events:
            task = asyncio.create_task(bouncer._watch_notifications())
            try:
                assert await asyncio.wait_for(events.get(), 2) == {
                    "kind": "workspace_changed"
                }
                disconnected.set()
                await asyncio.wait_for(between_connections.wait(), 2)
                disconnected.clear()
                async with factory() as db:
                    chat = await workspace.create_chat(db, kind=workspace.ChatKind.BOT)
                    missed = await workspace.create_notification(
                        db, chat=chat, body="Saved during the gap"
                    )
                    await db.commit()
                    assert events.empty()
                    resume_listener.set()
                    assert await asyncio.wait_for(events.get(), 2) == {
                        "kind": "workspace_changed"
                    }
                    sidebar = await list_notifications(db)
                    assert [note.id for note in sidebar.unread] == [missed.id]
                    live = await workspace.create_notification(
                        db, chat=chat, body="Live delivery restored"
                    )
                    await db.commit()
                    assert (await asyncio.wait_for(events.get(), 2))[
                        "notification_id"
                    ] == str(live.id)
            finally:
                task.cancel()
                with suppress(asyncio.CancelledError):
                    await task
    finally:
        await bouncer.client.close()


@pytest.mark.parametrize("stage", ["thread", "turn", "running"])
async def test_late_shadow_start(
    notification_engine: AsyncEngine,
    monkeypatch: pytest.MonkeyPatch,
    stage: str,
    tmp_path: Path,
) -> None:
    from aibo.bouncer.client import BouncerError
    from aibo.db.models import LocationModel

    factory = async_sessionmaker(notification_engine, expire_on_commit=False)
    bouncer = BouncerEvents("http://unused", EventHub())
    waiting, release = asyncio.Event(), asyncio.Event()
    turns = 0

    @asynccontextmanager
    async def session() -> AsyncIterator[AsyncSession]:
        async with factory() as db:
            yield db

    async def start_thread(**_kwargs: object) -> dict[str, Any]:
        if stage == "thread":
            waiting.set()
            await release.wait()
        return {"thread": {"id": "late-thread"}}

    async def start_turn(**_kwargs: object) -> dict[str, Any]:
        nonlocal turns
        turns += 1
        if stage == "running":
            async with factory() as db:
                shadow = await db.scalar(
                    sa.select(ChatModel).where(
                        ChatModel.shadow_for_turn_id == "source-turn"
                    )
                )
                assert shadow
                shadow.active_turn_id = "live-turn"
                await workspace.set_status(db, shadow, workspace.ChatStatus.RUNNING)
                await db.commit()
        waiting.set()
        await release.wait()
        if stage == "running":
            raise BouncerError("Acknowledgement lost after the turn started")
        return {"turn": {"id": "late-turn"}}

    monkeypatch.setattr("aibo.server.events.get_session", session)
    monkeypatch.setattr(bouncer.client, "start_thread", start_thread)
    monkeypatch.setattr(bouncer.client, "start_turn", start_turn)
    try:
        async with factory() as db:
            location = LocationModel(name="test", path=str(tmp_path))
            db.add(location)
            await db.flush()
            source = await workspace.create_chat(
                db, kind=workspace.ChatKind.BOT, location_id=location.id
            )
            source_id = source.id
            await bouncer._apply(
                db,
                source,
                "turn/completed",
                {"turn": {"id": "source-turn", "status": "completed"}},
            )
            await db.commit()
        pending = asyncio.create_task(
            bouncer._launch_shadow(
                source_id, {"id": "source-turn", "status": "completed"}
            )
        )
        try:
            await asyncio.wait_for(waiting.wait(), 2)
            async with factory() as db:
                shadow = await db.scalar(
                    sa.select(ChatModel).where(
                        ChatModel.shadow_for_turn_id == "source-turn"
                    )
                )
                assert shadow
                shadow.last_status_change_at = workspace.now_utc() - dt.timedelta(
                    minutes=3
                )
                await db.commit()
            await bouncer._repair_notifications()
        finally:
            release.set()
            await pending
        async with factory() as db:
            shadow = await db.scalar(
                sa.select(ChatModel).where(
                    ChatModel.shadow_for_turn_id == "source-turn"
                )
            )
            assert shadow
            assert shadow.status == (
                workspace.ChatStatus.RUNNING
                if stage == "running"
                else workspace.ChatStatus.ERROR
            )
            assert shadow.active_turn_id == (
                "live-turn" if stage == "running" else None
            )
            assert await db.scalar(
                sa.select(sa.func.count()).select_from(NotificationModel)
            ) == (0 if stage == "running" else 1)
        assert turns == (0 if stage == "thread" else 1)
    finally:
        await bouncer.client.close()


@pytest.mark.skipif(
    not os.environ.get("AIBO_POSTGRES_TEST_URL"),
    reason="AIBO_POSTGRES_TEST_URL is not set",
)
@pytest.mark.parametrize("kind", [workspace.ChatKind.BOT, workspace.ChatKind.MANAGER])
async def test_chat_allocation_is_atomic(kind: workspace.ChatKind) -> None:
    url = os.environ["AIBO_POSTGRES_TEST_URL"]
    schema = f"aibo_test_{uuid4().hex}"
    admin_engine = create_async_engine(url)
    async with admin_engine.begin() as connection:
        await connection.execute(sa.text(f"CREATE SCHEMA {schema}"))

    engine = create_async_engine(
        url, connect_args={"server_settings": {"search_path": schema}}
    )
    try:
        async with engine.begin() as connection:
            await connection.run_sync(BaseDBModel.metadata.create_all)
        factory = async_sessionmaker(engine, expire_on_commit=False)
        async with factory.begin() as session:
            session.add(CounterModel(name=f"next_{kind}_number", value=250))

        async def create_bot() -> int:
            async with factory.begin() as session:
                chat = await workspace.create_chat(session, kind=kind)
                number = (
                    chat.bot_number
                    if kind == workspace.ChatKind.BOT
                    else chat.manager_number
                )
                assert number is not None
                return cast(int, number)

        numbers = await asyncio.gather(*(create_bot() for _ in range(12)))

        assert sorted(numbers) == [0, 1, 2, 3, 4, 5, 250, 251, 252, 253, 254, 255]
    finally:
        await engine.dispose()
        async with admin_engine.begin() as connection:
            await connection.execute(sa.text(f"DROP SCHEMA {schema} CASCADE"))
        await admin_engine.dispose()


@pytest.mark.skipif(
    not os.environ.get("AIBO_POSTGRES_TEST_URL"),
    reason="AIBO_POSTGRES_TEST_URL is not set",
)
async def test_workers_share_complete_message(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    url = os.environ["AIBO_POSTGRES_TEST_URL"]
    schema = f"aibo_test_{uuid4().hex}"
    admin_engine = create_async_engine(url)
    async with admin_engine.begin() as connection:
        await connection.execute(sa.text(f"CREATE SCHEMA {schema}"))
    engine = create_async_engine(
        url, connect_args={"server_settings": {"search_path": schema}}
    )
    factory = async_sessionmaker(engine, expire_on_commit=False)
    bouncers = [BouncerEvents("http://bouncer", EventHub()) for _ in range(2)]

    @asynccontextmanager
    async def session() -> AsyncIterator[AsyncSession]:
        async with factory() as db:
            yield db

    monkeypatch.setattr("aibo.server.events.get_session", session)
    try:
        async with engine.begin() as connection:
            await connection.run_sync(BaseDBModel.metadata.create_all)
        async with factory.begin() as db:
            chat = await workspace.create_chat(db, kind=workspace.ChatKind.BOT)
            chat.codex_thread_id = "stream-thread"
            chat_id = chat.id
        params = {
            "threadId": "stream-thread",
            "turnId": "turn",
            "itemId": "item",
            "delta": "unused",
        }
        events = [
            {
                "method": "item/agentMessage/delta",
                "params": params,
                "aibo": {"message_text": "text " * length},
            }
            for length in (1, 8, 3, 8, 2, 4)
        ]
        await asyncio.gather(
            *(bouncer._persist(event) for bouncer in bouncers for event in events)
        )
        async with factory() as db:
            reloaded = await workspace.get_chat(db, chat_id)
            assert reloaded
            chat = reloaded
            messages = [
                message
                for message in chat.messages
                if message.external_id == "item:item"
            ]
            assert messages == []
        final = {
            "method": "item/completed",
            "params": {
                "threadId": "stream-thread",
                "turnId": "turn",
                "item": {"id": "item", "type": "agentMessage", "text": "Final"},
            },
        }
        await asyncio.gather(
            *(
                bouncer._persist(event)
                for bouncer in bouncers
                for event in [final, *events]
            )
        )
        async with factory() as db:
            reloaded = await workspace.get_chat(db, chat_id)
            assert reloaded
            chat = reloaded
            messages = [
                message
                for message in chat.messages
                if message.external_id == "item:item"
            ]
            assert len(messages) == 1
            assert messages[0].content == "Final"
            assert "streaming" not in messages[0].data
        completed = {
            "method": "turn/completed",
            "params": {
                "threadId": "stream-thread",
                "turn": {"id": "turn", "status": "completed"},
            },
        }
        await asyncio.gather(
            *(bouncer._persist(completed) for bouncer in bouncers for _ in range(3))
        )
        async with factory() as db:
            shadows = list(
                (
                    await db.scalars(
                        sa.select(ChatModel).where(ChatModel.origin_chat_id == chat_id)
                    )
                ).all()
            )
            assert len(shadows) == 1
            # No location means notification fallback, without contacting Codex.
            notifications = list(
                (
                    await db.scalars(
                        sa.select(NotificationModel).where(
                            NotificationModel.chat_id == chat_id
                        )
                    )
                ).all()
            )
            assert len(notifications) == 1
            shadows[0].codex_thread_id = "shadow-thread"
            await db.commit()
            notification_id = notifications[0].id
        shadow_done = {
            "method": "turn/completed",
            "params": {
                "threadId": "shadow-thread",
                "turn": {"id": "shadow-turn", "status": "completed"},
            },
        }
        outcomes = await asyncio.gather(
            *(bouncer._persist(shadow_done) for bouncer in bouncers)
        )
        assert (
            outcomes
            == [
                {
                    "kind": "notification_created",
                    "notification_id": str(notification_id),
                    "chat_id": str(chat_id),
                }
            ]
            * 2
        )
    finally:
        for bouncer in bouncers:
            await bouncer.client.close()
        await engine.dispose()
        async with admin_engine.begin() as connection:
            await connection.execute(sa.text(f"DROP SCHEMA {schema} CASCADE"))
        await admin_engine.dispose()


async def test_history_projection(notification_engine: AsyncEngine) -> None:
    from aibo.core.tests import test_history

    factory = async_sessionmaker(notification_engine, expire_on_commit=False)
    async with factory() as db:
        await test_history.test_compact_pages(db)
        await test_history.test_progress(db)
        await test_history.test_notice_and_visibility(db)
        await test_history.test_detail_bounds(db)
        await test_history.test_compact_send(db)
        await test_history.test_completed_snapshot_version(db)
        await test_history.test_turn_bookkeeping(db)


@pytest.mark.parametrize("case", ["timestamps", "limit"])
async def test_notification_sort(notification_engine: AsyncEngine, case: str) -> None:
    from aibo.core.tests.test_notifications import (
        test_notification_order,
        test_notification_order_before_limit,
    )

    factory = async_sessionmaker(notification_engine, expire_on_commit=False)
    async with factory() as db:
        if case == "timestamps":
            await test_notification_order(db)
        else:
            await test_notification_order_before_limit(db)
