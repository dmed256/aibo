import datetime as dt

import pytest
from sqlalchemy.ext.asyncio import AsyncSession

from aibo.core import workspace
from aibo.db.models import CounterModel, ProjectModel
from aibo.server.schemas import ChatSummary


async def test_last_active(db: AsyncSession) -> None:
    chat = await workspace.create_chat(db, kind=workspace.ChatKind.BOT)
    assert ChatSummary.from_model(chat).last_active_at is None
    user = await workspace.append_message(
        db, chat, kind=workspace.MessageKind.USER, content="Request"
    )
    assert ChatSummary.from_model(chat).last_active_at == user.created_at
    final = await workspace.append_message(
        db,
        chat,
        kind=workspace.MessageKind.ASSISTANT,
        content="Done",
        data={"item": {"phase": "final_answer"}},
    )
    pending_data: list[dict[str, object]] = [
        {"item": {"phase": "commentary"}},
        {"streaming": True},
    ]
    for data in pending_data:
        await workspace.append_message(
            db, chat, kind=workspace.MessageKind.ASSISTANT, content="Working", data=data
        )
    await workspace.set_status(db, chat, workspace.ChatStatus.ERROR)
    assert ChatSummary.from_model(chat).last_active_at == final.created_at
    # A completed legacy partial is dated at completion, not its first token.
    completed_at = final.created_at + dt.timedelta(seconds=10)
    final.data = {"completed_at": completed_at.isoformat()}
    assert ChatSummary.from_model(chat).last_active_at == completed_at
    user.created_at = completed_at + dt.timedelta(seconds=1)
    assert ChatSummary.from_model(chat).last_active_at == user.created_at


async def test_bot_numbers_wrap_and_resolve_latest(db: AsyncSession) -> None:
    db.add(CounterModel(name="next_bot_number", value=255))
    await db.flush()

    bot_255 = await workspace.create_chat(db, kind=workspace.ChatKind.BOT)
    first_bot_0 = await workspace.create_chat(db, kind=workspace.ChatKind.BOT)
    counter = await db.get(CounterModel, "next_bot_number")
    assert counter
    counter.value = 0
    second_bot_0 = await workspace.create_chat(db, kind=workspace.ChatKind.BOT)

    assert bot_255.bot_number == 255
    assert first_bot_0.bot_number == 0
    assert second_bot_0.bot_number == 0
    # Activity on the older assignment must not steal a repeated label.
    first_bot_0.activity_at = second_bot_0.created_at + dt.timedelta(days=1)
    await db.flush()
    latest = await workspace.latest_bot(db, 0)
    assert latest and latest.id == second_bot_0.id


async def test_manager_numbers_and_search(db: AsyncSession) -> None:
    first = await workspace.create_chat(db, kind=workspace.ChatKind.MANAGER)
    db.add(CounterModel(name="next_bot_number", value=21))
    bot = await workspace.create_chat(db, kind=workspace.ChatKind.BOT)
    second = await workspace.create_chat(db, kind=workspace.ChatKind.MANAGER)
    assert workspace.chat_label(first) == "m0"
    assert workspace.chat_label(second) == "m1"
    assert workspace.chat_label(bot) == "b21"
    assert [chat.id for chat in await workspace.list_chats(db, query="m1")] == [
        second.id
    ]
    original_activity = second.activity_at
    await workspace.append_message(
        db, second, kind=workspace.MessageKind.ASSISTANT, content="More output"
    )
    assert second.activity_at == original_activity


async def test_search_pages(db: AsyncSession) -> None:
    chats = [
        await workspace.create_chat(db, kind=workspace.ChatKind.BOT, title="Review")
        for _ in range(123)
    ]
    await workspace.create_chat(db, kind=workspace.ChatKind.SHADOW, title="Review")
    # Equal activity timestamps must still have a deterministic page boundary.
    for chat in chats:
        chat.activity_at = chats[0].activity_at
    await db.flush()
    pages = [
        await workspace.list_chats(db, query="Review", limit=50, offset=offset)
        for offset in (0, 50, 100, 150)
    ]
    assert [len(page) for page in pages] == [50, 50, 23, 0]
    assert [chat.id for page in pages for chat in page] == sorted(
        (chat.id for chat in chats), reverse=True
    )


async def test_shadow_chats_stay_hidden(db: AsyncSession) -> None:
    manager = await workspace.create_chat(db, kind=workspace.ChatKind.MANAGER)
    shadow = await workspace.create_chat(db, kind=workspace.ChatKind.SHADOW)

    assert [chat.id for chat in await workspace.list_chats(db)] == [manager.id]
    assert await workspace.get_chat(db, shadow.id) is None
    with pytest.raises(ValueError, match="shadow chats"):
        await workspace.create_notification(db, chat=shadow, body="hidden")


async def test_chat_search_combines_bot_number_and_title(db: AsyncSession) -> None:
    db.add(CounterModel(name="next_bot_number", value=21))
    await db.flush()
    match = await workspace.create_chat(
        db, kind=workspace.ChatKind.BOT, title="This is blah work"
    )
    await workspace.create_chat(db, kind=workspace.ChatKind.MANAGER, title="Unrelated")

    assert [chat.id for chat in await workspace.list_chats(db, query="1 blah")] == [
        match.id
    ]
    assert [chat.id for chat in await workspace.list_chats(db, query="1 blwk")] == [
        match.id
    ]
    assert await workspace.list_chats(db, query="%") == []


async def test_home_groups(db: AsyncSession) -> None:
    project = ProjectModel(name="older", description="Older project")
    db.add(project)
    await db.flush()
    older = await workspace.create_chat(
        db, kind=workspace.ChatKind.MANAGER, project_id=project.id
    )
    for _ in range(101):
        await workspace.create_chat(db, kind=workspace.ChatKind.BOT)
    assert older.id not in {
        chat.id for chat in await workspace.list_chats(db, limit=100)
    }
    assert [
        chat.id
        for chat in await workspace.list_chats(db, project_id=project.id, limit=10)
    ] == [older.id]
    unassigned = await workspace.list_chats(db, unassigned=True, limit=10)
    assert len(unassigned) == 10
    assert all(chat.project_id is None for chat in unassigned)
