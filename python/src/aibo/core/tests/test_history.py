from collections.abc import AsyncIterator
from uuid import UUID

import pytest
from fastapi import FastAPI, HTTPException
from httpx import ASGITransport, AsyncClient
from sqlalchemy.ext.asyncio import AsyncSession

from aibo.core import workspace
from aibo.db.client import get_db
from aibo.db.models import ChatMessageModel
from aibo.server import history
from aibo.server.routes.workspace_routes import router


async def test_compact_pages(db: AsyncSession) -> None:
    chat = await workspace.create_chat(db, kind=workspace.ChatKind.MANAGER)
    for i in range(65):
        await workspace.append_message(
            db, chat, kind=workspace.MessageKind.USER, content=f"user {i}"
        )
        await workspace.append_message(
            db,
            chat,
            kind=workspace.MessageKind.ASSISTANT,
            content="commentary" * 1000,
            data={"item": {"phase": "commentary"}},
        )
        await workspace.append_message(
            db, chat, kind=workspace.MessageKind.TOOL, content="payload" * 10000
        )
        await workspace.append_message(
            db,
            chat,
            kind=workspace.MessageKind.ASSISTANT,
            content=f"final {i}",
            data={"item": {"phase": "final_answer"}},
        )
    await db.commit()
    db.expunge_all()
    page = await history.history(db, chat.id)
    assert len([m for m in page.messages if m.kind in ("user", "assistant")]) == 60
    assert "payload" not in page.model_dump_json()
    assert (
        len(
            [
                m
                for m in page.messages
                if isinstance(item := m.data.get("item"), dict)
                and item.get("phase") == "commentary"
            ]
        )
        == 20
    )
    assert page.messages[-1].content == "final 64"
    assert page.older_before
    old = await history.history(db, chat.id, page.older_before)
    assert old.messages[-1].content == "final 44"
    assert not ({m.id for m in old.messages} & {m.id for m in page.messages})
    group = next(m for m in page.messages if m.data.get("hidden_count"))

    detail = await history.details(
        db, chat.id, UUID(str(group.data["first_id"])), UUID(str(group.data["last_id"]))
    )
    assert [m.kind for m in detail.messages] == ["tool"]
    assert len(detail.messages[0].content) == 70000
    assert detail.next_after is None


async def test_notice_and_visibility(db: AsyncSession) -> None:
    chat = await workspace.create_chat(db, kind=workspace.ChatKind.MANAGER)
    await workspace.append_message(
        db, chat, kind=workspace.MessageKind.USER, content="request"
    )
    await workspace.append_message(
        db,
        chat,
        kind=workspace.MessageKind.ASSISTANT,
        content="partial",
        data={"streaming": True},
    )
    await workspace.append_message(
        db, chat, kind=workspace.MessageKind.ASSISTANT, content="legacy final"
    )
    error = await workspace.append_message(
        db, chat, kind=workspace.MessageKind.ERROR, content="Failed precisely"
    )
    await workspace.set_status(db, chat, workspace.ChatStatus.ERROR)
    await db.commit()
    db.expunge_all()
    app = FastAPI()
    app.include_router(router)

    async def test_db() -> AsyncIterator[AsyncSession]:
        yield db

    app.dependency_overrides[get_db] = test_db
    async with AsyncClient(
        transport=ASGITransport(app=app), base_url="http://test"
    ) as client:
        response = await client.get(f"/api/chats/{chat.id}/history")
        assert response.status_code == 200
        page = response.json()
        assert page["notice"] == "Failed precisely"
        assert page["notice_message_id"] == str(error.id)
        assert [m["content"] for m in page["messages"] if m["kind"] != "event"] == [
            "request",
            "legacy final",
            "Failed precisely",
        ]
        assert page["last_active_at"]


async def test_detail_bounds(db: AsyncSession) -> None:
    chat = await workspace.create_chat(db, kind=workspace.ChatKind.MANAGER)
    items = [
        await workspace.append_message(
            db, chat, kind=workspace.MessageKind.TOOL, content=str(i)
        )
        for i in range(35)
    ]
    other = await workspace.create_chat(db, kind=workspace.ChatKind.MANAGER)
    foreign = await workspace.append_message(
        db, other, kind=workspace.MessageKind.USER, content="other"
    )
    shadow = await workspace.create_chat(db, kind=workspace.ChatKind.SHADOW)
    await db.commit()
    first = await history.details(db, chat.id, items[0].id, items[-1].id)
    assert len(first.messages) == 30 and first.next_after == items[29].id
    last = await history.details(
        db, chat.id, items[0].id, items[-1].id, first.next_after
    )
    assert [m.content for m in last.messages] == [str(i) for i in range(30, 35)]
    assert not last.next_after
    with pytest.raises(HTTPException):
        await history.details(db, chat.id, foreign.id, items[-1].id)
    with pytest.raises(HTTPException):
        await history.history(db, shadow.id)
    with pytest.raises(HTTPException):
        await history.details(db, shadow.id, items[0].id, items[-1].id)
    db.expunge_all()
    page = await history.history(db, chat.id)
    assert (
        sum(int(str(m.data.get("hidden_count", 0))) for m in page.messages) == 37
    )  # two initial role messages
    assert not any(
        isinstance(obj, ChatMessageModel) for obj in db.identity_map.values()
    )


async def test_compact_send(db: AsyncSession) -> None:
    from aibo.core.tests.fakes import FakeBouncer
    from aibo.server.dependencies import get_bouncer
    from aibo.server.events import EventHub
    from aibo.server.routes.system import compact_event

    chat = await workspace.create_chat(db, kind=workspace.ChatKind.MANAGER)
    await workspace.append_message(
        db, chat, kind=workspace.MessageKind.TOOL, content="large secret body" * 10000
    )
    await db.commit()
    app = FastAPI()
    app.include_router(router)
    app.state.event_hub = EventHub()

    async def test_db() -> AsyncIterator[AsyncSession]:
        yield db

    app.dependency_overrides[get_db] = test_db
    app.dependency_overrides[get_bouncer] = lambda: FakeBouncer()
    async with app.state.event_hub.subscribe() as queue:
        async with AsyncClient(
            transport=ASGITransport(app=app), base_url="http://test"
        ) as client:
            response = await client.post(
                f"/api/chats/{chat.id}/submit?compact=true",
                json={"text": "new request"},
            )
    assert response.status_code == 200
    payload = response.json()
    assert payload["chat"]["history_version"]
    assert "large secret body" not in response.text
    assert any(m["content"] == "new request" for m in payload["chat"]["messages"])
    events = []
    while not queue.empty():
        events.append(compact_event(queue.get_nowait()))
    assert "large secret body" not in str(events)
    event = {
        "kind": "codex",
        "chat_id": str(chat.id),
        "params": {"output": "x" * 1000000},
    }
    assert len(str(compact_event(event))) < 100
    assert "params" in event  # other clients retain the original event


async def test_turn_bookkeeping(db: AsyncSession) -> None:
    chat = await workspace.create_chat(db, kind=workspace.ChatKind.MANAGER)
    user = await workspace.append_message(
        db, chat, kind=workspace.MessageKind.USER, content="Codex turn started"
    )
    bookkeeping: list[tuple[str, dict[str, object]]] = [
        ("Codex turn completed", {}),  # legacy text-only rows
        ("Starting", {"event": "turn_started"}),
        ("Started", {"event": "turn/started"}),
        ("Done", {"event": "turn/completed", "turn": {"status": "completed"}}),
    ]
    items = []
    for i in range(32):
        items.append(
            await workspace.append_message(
                db, chat, kind=workspace.MessageKind.TOOL, content=f"tool {i}"
            )
        )
        for content, data in bookkeeping:
            await workspace.append_message(
                db, chat, kind=workspace.MessageKind.EVENT, content=content, data=data
            )
    failure = await workspace.append_message(
        db,
        chat,
        kind=workspace.MessageKind.EVENT,
        content="Codex turn failed",
        data={"event": "turn/completed", "turn": {"status": "failed"}},
    )
    interrupted = await workspace.append_message(
        db,
        chat,
        kind=workspace.MessageKind.EVENT,
        content="Codex turn interrupted",
        data={"event": "turn/completed", "turn": {"status": "interrupted"}},
    )
    answer = await workspace.append_message(
        db, chat, kind=workspace.MessageKind.ASSISTANT, content="Codex turn completed"
    )
    end = await workspace.append_message(
        db, chat, kind=workspace.MessageKind.EVENT, content="Codex turn completed"
    )
    await workspace.set_status(db, chat, workspace.ChatStatus.COMPLETED)
    await db.commit()
    db.expunge_all()

    page = await history.history(db, chat.id)
    assert [m.id for m in page.messages if m.kind != "event"] == [user.id, answer.id]
    assert page.messages[-1].id == answer.id  # no lifecycle-only hidden group
    assert page.notice is None
    group = next(m for m in page.messages if m.id == items[0].id)
    assert group.data["hidden_count"] == 34  # tools plus actionable outcomes
    first = await history.details(db, chat.id, items[0].id, end.id)
    assert [m.id for m in first.messages] == [m.id for m in items[:30]]
    second = await history.details(db, chat.id, items[0].id, end.id, first.next_after)
    assert [m.id for m in second.messages] == [m.id for m in items[30:]] + [
        failure.id,
        interrupted.id,
        answer.id,
    ]
    assert second.next_after is None
    # Filtering is read-only: recovery still has its durable completion record.
    assert (await db.get(ChatMessageModel, end.id)).content == "Codex turn completed"


async def test_completed_snapshot_version(db: AsyncSession) -> None:
    import datetime as dt

    chat = await workspace.create_chat(db, kind=workspace.ChatKind.MANAGER)
    await workspace.append_message(
        db, chat, kind=workspace.MessageKind.USER, content="request"
    )
    partial = await workspace.append_message(
        db,
        chat,
        kind=workspace.MessageKind.ASSISTANT,
        content="partial",
        data={"streaming": True},
    )
    await workspace.append_message(
        db, chat, kind=workspace.MessageKind.TOOL, content="later item"
    )
    await db.commit()
    before = await history.history(db, chat.id)
    partial.content = "finished answer"
    partial.data = {
        "completed_at": (workspace.now_utc() + dt.timedelta(seconds=1)).isoformat(),
        "item": {"phase": "final_answer"},
    }
    await db.commit()
    after = await history.history(db, chat.id)
    assert before.history_version and after.history_version
    assert after.history_version > before.history_version
    assert "finished answer" in [m.content for m in after.messages]


async def test_progress(db: AsyncSession) -> None:
    chat = await workspace.create_chat(db, kind=workspace.ChatKind.BOT)
    chat.status = workspace.ChatStatus.RUNNING
    await workspace.append_message(
        db, chat, kind=workspace.MessageKind.USER, content="request"
    )
    progress = await workspace.append_message(
        db,
        chat,
        kind=workspace.MessageKind.ASSISTANT,
        content="Checking the layout.",
        data={"item": {"type": "agentMessage", "phase": "commentary"}},
    )
    await workspace.append_message(
        db, chat, kind=workspace.MessageKind.TOOL, content="private tool output"
    )
    await db.commit()
    page = await history.history(db, chat.id)
    assert page.active
    assert [m.content for m in page.messages[-3:]] == [
        "request",
        "Checking the layout.",
        "",
    ]
    assert page.messages[-2].id == progress.id
    assert page.messages[-1].data["hidden_count"] == 1
    await workspace.append_message(
        db,
        chat,
        kind=workspace.MessageKind.ASSISTANT,
        content="Done.",
        data={"item": {"phase": "final_answer"}},
    )
    await db.commit()
    page = await history.history(db, chat.id)
    assert [m.content for m in page.messages[-4:]] == [
        "request",
        "Checking the layout.",
        "",
        "Done.",
    ]
