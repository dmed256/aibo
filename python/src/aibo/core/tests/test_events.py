from collections.abc import AsyncIterator
from contextlib import asynccontextmanager
from typing import Any, cast

import pytest
from sqlalchemy.ext.asyncio import AsyncSession

from aibo.core import workspace
from aibo.server.events import BouncerEvents, EventHub
from aibo.server.schemas import Chat


async def test_failed_turn_notice(db: AsyncSession) -> None:
    chat = await workspace.create_chat(db, kind=workspace.ChatKind.BOT)
    chat.codex_thread_id = "thread-1"
    await workspace.append_message(
        db, chat, kind=workspace.MessageKind.USER, content="Fix it"
    )
    bouncer = BouncerEvents("http://bouncer", EventHub())
    await bouncer._apply(
        db,
        chat,
        "turn/completed",
        {
            "turn": {
                "id": "failed-turn",
                "status": "failed",
                "error": {"message": "app-server disconnected"},
            },
        },
    )
    await db.commit()
    await db.refresh(chat)
    result = Chat.from_model(chat)
    assert result.notice == "The turn could not continue: app-server disconnected."
    assert result.notice_message_id == chat.messages[-1].id
    await workspace.append_message(
        db, chat, kind=workspace.MessageKind.USER, content="Try again"
    )
    await workspace.set_status(db, chat, workspace.ChatStatus.RUNNING)
    assert Chat.from_model(chat).notice is None
    assert any(
        message.data.get("event") == "turn/completed" for message in chat.messages
    )


async def test_old_turn_cannot_stop_resume(db: AsyncSession) -> None:
    chat = await workspace.create_chat(db, kind=workspace.ChatKind.BOT)
    chat.active_turn_id = "resumed"
    await workspace.set_status(db, chat, workspace.ChatStatus.RUNNING)
    await workspace.append_message(
        db,
        chat,
        kind=workspace.MessageKind.EVENT,
        content="Codex turn started",
        data={"event": "turn_started", "turn_id": "resumed"},
    )
    bouncer = BouncerEvents("http://bouncer", EventHub())
    await bouncer._apply(
        db, chat, "turn/completed", {"turn": {"id": "old", "status": "interrupted"}}
    )
    assert (
        chat.status == workspace.ChatStatus.RUNNING and chat.active_turn_id == "resumed"
    )
    await bouncer._apply(
        db, chat, "turn/completed", {"turn": {"id": "resumed", "status": "completed"}}
    )
    for turn_id in ("old", "even-older"):
        await bouncer._apply(
            db, chat, "turn/completed", {"turn": {"id": turn_id, "status": "failed"}}
        )
    assert chat.status == workspace.ChatStatus.COMPLETED and chat.active_turn_id is None
    assert Chat.from_model(chat).notice is None
    assert (
        len(
            [
                message
                for message in chat.messages
                if message.data.get("event") == "turn/completed"
            ]
        )
        == 3
    )


async def test_codex_events_update_chat(db: AsyncSession) -> None:
    chat = await workspace.create_chat(db, kind=workspace.ChatKind.MANAGER)
    chat.codex_thread_id = "thread-1"
    bouncer = BouncerEvents("http://bouncer", EventHub())

    await bouncer._apply(
        db,
        chat,
        "turn/started",
        {"threadId": "thread-1", "turn": {"id": "turn-1"}},
    )
    await bouncer._apply(
        db,
        chat,
        "item/completed",
        {
            "threadId": "thread-1",
            "turnId": "turn-1",
            "item": {"id": "item-1", "type": "agentMessage", "text": "Fixed"},
        },
    )
    await bouncer._apply(
        db,
        chat,
        "item/completed",
        {
            "threadId": "thread-1",
            "turnId": "turn-1",
            "item": {"id": "item-1", "type": "agentMessage", "text": "Fixed"},
        },
    )
    await bouncer._apply(
        db,
        chat,
        "turn/completed",
        {
            "threadId": "thread-1",
            "turn": {"id": "turn-1", "status": "completed"},
        },
    )
    await bouncer._apply(
        db,
        chat,
        "turn/completed",
        {
            "threadId": "thread-1",
            "turn": {"id": "turn-1", "status": "completed"},
        },
    )

    assert chat.status == workspace.ChatStatus.COMPLETED
    assert chat.active_turn_id is None
    assert [message.content for message in chat.messages[-2:]] == [
        "Fixed",
        "Codex turn completed",
    ]


async def test_complete_messages_only(
    db: AsyncSession, monkeypatch: pytest.MonkeyPatch
) -> None:
    chat = await workspace.create_chat(db, kind=workspace.ChatKind.BOT)
    chat.codex_thread_id = "thread"
    await db.commit()
    bouncer = BouncerEvents("http://bouncer", EventHub())

    @asynccontextmanager
    async def session() -> AsyncIterator[AsyncSession]:
        yield db

    monkeypatch.setattr("aibo.server.events.get_session", session)
    delta = {
        "method": "item/agentMessage/delta",
        "params": {
            "threadId": "thread",
            "turnId": "turn",
            "itemId": "partial",
            "delta": "Done",
        },
        "aibo": {"message_text": "Done"},
    }
    final = {
        "method": "item/completed",
        "params": {
            "threadId": "thread",
            "turnId": "turn",
            "item": {"id": "final", "type": "agentMessage", "text": "Done"},
        },
    }
    try:
        count = len(chat.messages)
        assert await bouncer._persist(delta) is None
        assert len(chat.messages) == count
        visible = await bouncer._persist(final)
        assert visible and visible["method"] == "item/completed"
        await bouncer._persist(final)
        assert await bouncer._persist(delta) is None
        result = Chat.from_model(chat)
        assert len(result.messages) == count + 1
        assert result.messages[-1].content == "Done"
    finally:
        await bouncer.client.close()


async def test_legacy_partial_hidden(db: AsyncSession) -> None:
    chat = await workspace.create_chat(db, kind=workspace.ChatKind.BOT)
    partial = await workspace.append_message(
        db,
        chat,
        kind=workspace.MessageKind.ASSISTANT,
        content="Unfinished",
        data={"streaming": True},
        external_id="item:old",
    )
    assert partial.id not in [message.id for message in Chat.from_model(chat).messages]
    assert Chat.from_model(chat).last_active_at is None
    bouncer = BouncerEvents("http://bouncer", EventHub())
    try:
        await bouncer._apply(
            db,
            chat,
            "item/completed",
            {"item": {"id": "old", "type": "agentMessage", "text": "Finished"}},
        )
        assert Chat.from_model(chat).messages[-1].content == "Finished"
        assert Chat.from_model(chat).messages[-1].id == partial.id
        assert Chat.from_model(chat).last_active_at > partial.created_at
    finally:
        await bouncer.client.close()


@pytest.mark.parametrize("finished", [True, False])
async def test_reconnect_replays_items(
    db: AsyncSession, monkeypatch: pytest.MonkeyPatch, finished: bool
) -> None:
    chat = await workspace.create_chat(db, kind=workspace.ChatKind.MANAGER)
    chat.codex_thread_id = "thread-replay"
    chat.active_turn_id = "turn-replay"
    await workspace.set_status(db, chat, workspace.ChatStatus.RUNNING)
    await db.commit()
    hub = EventHub()
    bouncer = BouncerEvents("http://bouncer", hub)

    class ReplayBouncer:
        async def resume_thread(self, thread_id: str) -> dict[str, Any]:
            return {
                "thread": {
                    "id": thread_id,
                    "status": {"type": "active"},
                    "turns": [
                        {
                            "id": "turn-replay",
                            "status": "completed" if finished else "inProgress",
                            "items": [
                                {
                                    "id": "item-replay",
                                    "type": "agentMessage",
                                    "text": "Recovered result",
                                }
                            ],
                        }
                    ],
                }
            }

    @asynccontextmanager
    async def test_session() -> AsyncIterator[AsyncSession]:
        yield db

    async def ignore_shadow(_chat_id: object, _turn: object) -> None:
        return None

    import aibo.server.events as events_module

    monkeypatch.setattr(events_module, "get_session", test_session)
    monkeypatch.setattr(bouncer, "client", cast(Any, ReplayBouncer()))
    monkeypatch.setattr(bouncer, "_launch_shadow", ignore_shadow)
    async with hub.subscribe() as events:
        await bouncer._reconcile()
        emitted = [events.get_nowait() for _ in range(2 if finished else 0)]

    assert [event["method"] for event in emitted] == (
        ["item/completed", "turn/completed"] if finished else []
    )
    assert chat.status == (
        workspace.ChatStatus.COMPLETED if finished else workspace.ChatStatus.RUNNING
    )
    if finished:
        assert [message.content for message in chat.messages[-2:]] == [
            "Recovered result",
            "Codex turn completed",
        ]
    else:
        assert not any(
            message.content == "Recovered result" for message in chat.messages
        )


async def test_shadow_message_is_private(
    db: AsyncSession, monkeypatch: pytest.MonkeyPatch
) -> None:
    shadow = await workspace.create_chat(db, kind=workspace.ChatKind.SHADOW)
    shadow.codex_thread_id = "shadow-thread"
    await db.commit()
    bouncer = BouncerEvents("http://bouncer", EventHub())

    @asynccontextmanager
    async def session() -> AsyncIterator[AsyncSession]:
        yield db

    monkeypatch.setattr("aibo.server.events.get_session", session)
    try:
        visible = await bouncer._persist(
            {
                "method": "item/completed",
                "params": {
                    "threadId": "shadow-thread",
                    "turnId": "turn",
                    "item": {"id": "item", "type": "agentMessage", "text": "Private"},
                },
            }
        )
        assert visible is None
        assert shadow.messages[-1].content == "Private"
        assert await workspace.list_chats(db) == []
        assert await workspace.get_chat(db, shadow.id) is None
    finally:
        await bouncer.client.close()
