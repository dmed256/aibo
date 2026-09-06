from __future__ import annotations

import datetime as dt
import json
import os
import re
from enum import StrEnum
from typing import Any, cast
from uuid import UUID

import sqlalchemy as sa
from sqlalchemy.dialects.postgresql import insert as pg_insert
from sqlalchemy.dialects.sqlite import insert as sqlite_insert
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.orm import selectinload

from aibo.common.runtime_files import ensure_project_readme, role_instructions
from aibo.common.time import now_utc
from aibo.db.models import ChatMessageModel, ChatModel, CounterModel, NotificationModel

NOTIFICATION_CHANNEL = sa.func.concat(
    "aibo_notifications_", sa.func.md5(sa.func.current_schema())
)


class ChatKind(StrEnum):
    MANAGER = "manager"
    BOT = "bot"
    SHADOW = "shadow"


class ChatStatus(StrEnum):
    IDLE = "idle"
    QUEUED = "queued"
    RUNNING = "running"
    COMPLETED = "completed"
    INTERRUPTED = "interrupted"
    CANCELLED = "cancelled"
    ERROR = "error"
    BLOCKED = "blocked"

    @property
    def active(self) -> bool:
        return self in {self.QUEUED, self.RUNNING}


class MessageKind(StrEnum):
    SYSTEM = "system"
    USER = "user"
    ASSISTANT = "assistant"
    TOOL = "tool"
    EVENT = "event"
    ERROR = "error"


def chat_label(chat: ChatModel) -> str:
    if chat.kind == ChatKind.BOT:
        return f"b{chat.bot_number}"
    if chat.kind == ChatKind.MANAGER:
        return f"m{chat.manager_number}"
    return "sb"


def delegation_message(user_message: str, m_context: str) -> str:
    return (
        "# user message\n\n"
        f"{user_message}\n\n"
        "# m context\n\n"
        f"{m_context.strip()}"
    )


async def allocate_chat_number(db: AsyncSession, kind: ChatKind) -> int:
    # One upsert locks/increments the counter even when it does not exist yet.
    insert = pg_insert if db.get_bind().dialect.name == "postgresql" else sqlite_insert
    statement = insert(CounterModel).values(name=f"next_{kind}_number", value=1)
    statement = statement.on_conflict_do_update(
        index_elements=[CounterModel.name],
        set_={"value": (CounterModel.value + 1) % 256},
    ).returning(CounterModel.value)
    next_number = await db.scalar(statement)
    return (cast(int, next_number) - 1) % 256


async def create_chat(
    db: AsyncSession,
    *,
    kind: ChatKind,
    title: str = "New chat",
    user_message: str | None = None,
    attachments: list[str] | None = None,
    source_message_id: UUID | None = None,
    m_context: str | None = None,
    location_id: UUID | None = None,
    project_id: UUID | None = None,
    goal: bool = True,
) -> ChatModel:
    number = await allocate_chat_number(db, kind) if kind != ChatKind.SHADOW else None
    chat = ChatModel(
        kind=kind,
        bot_number=number if kind == ChatKind.BOT else None,
        manager_number=number if kind == ChatKind.MANAGER else None,
        title=title.strip() or "New chat",
        location_id=None if kind == ChatKind.MANAGER else location_id,
        project_id=project_id,
        goal_enabled=goal and kind != ChatKind.SHADOW,
    )
    db.add(chat)
    await db.flush()

    db.add(
        ChatMessageModel(
            chat=chat,
            kind=MessageKind.SYSTEM,
            content=role_instructions(kind),
            created_at=now_utc(),
        )
    )
    await db.flush()
    created = await get_chat(db, chat.id, include_shadow=True)
    assert created is not None
    if kind != ChatKind.SHADOW:
        await refresh_context(db, created)
    if user_message or attachments:
        user_message = user_message or ""
        content = user_message
        if kind == ChatKind.BOT and m_context is not None:
            content = delegation_message(content, m_context)
        await append_message(
            db,
            chat,
            kind=MessageKind.USER,
            content=content,
            data={
                "user_message": user_message,
                "attachments": attachments or [],
                **(
                    {"source_message_id": str(source_message_id)}
                    if source_message_id
                    else {}
                ),
            },
        )

    await db.flush()
    return created


async def refresh_context(db: AsyncSession, chat: ChatModel) -> str:
    """Snapshot current project instructions separately from the user's words."""
    sections = []
    if chat.kind == ChatKind.MANAGER:
        sections.append(
            "Aibo API: "
            + os.environ.get("AIBO_PUBLIC_URL", "http://127.0.0.1:5000")
            + "/api. Use /openapi.json for current request shapes; this supersedes legacy routes in role notes. Managers have no assigned location; choose locations for bots. "
            "Delegate using source_message_id from the original manager user message "
            f"(GET /api/chats/{chat.id}); this copies its verbatim text and all image attachments. "
            "Use it on POST /api/chats or POST /api/chats/{id}/submit with m_context "
            "(empty when unnecessary), instead of reconstructing text or listing image paths in prose."
        )
    if chat.location and chat.kind != ChatKind.MANAGER:
        sections.append(f"Location: {chat.location.name} -> {chat.location.path}")
    elif chat.project:
        sections.append("Location: none")
    if chat.project:
        readme = ensure_project_readme(chat.project.name)
        sections.append(
            f"Project: {chat.project.name}\n{chat.project.description}\n"
            f"README: {readme}\n\n{readme.read_text()}"
        )
    elif chat.location and chat.kind != ChatKind.MANAGER:
        sections.append("Project: none")
    context = "\n\n".join(sections)
    previous = next(
        (
            message.content
            for message in reversed(chat.messages)
            if message.data.get("event") == "chat_context"
        ),
        "",
    )
    if not context and previous:
        context = "No project or location assigned."
    if context != previous:
        await append_message(
            db,
            chat,
            kind=MessageKind.SYSTEM,
            content=context,
            data={"event": "chat_context"},
        )
    return context


def user_objective(message: ChatMessageModel) -> str:
    original = message.data.get("user_message")
    return original if isinstance(original, str) else str(message.content)


def goal_objective(chat: ChatModel, message: ChatMessageModel) -> str:
    objective = user_objective(message)
    if chat.kind == ChatKind.MANAGER and objective.strip():
        return (
            "Coordinate the following request according to your manager role. "
            "For delegated work, complete this coordination goal once the correct "
            "worker(s) have accepted the handoff. Do not wait for implementation, "
            "poll, or monitor unless the user explicitly requests monitoring. "
            "The worker owns the implementation goal.\n\nUser request:\n" + objective
        )
    return objective


async def append_message(
    db: AsyncSession,
    chat: ChatModel,
    *,
    kind: MessageKind,
    content: str,
    data: dict[str, object] | None = None,
    external_id: str | None = None,
) -> ChatMessageModel:
    timestamp = now_utc()
    message = ChatMessageModel(
        chat=chat,
        kind=kind,
        content=content,
        data=data or {},
        external_id=external_id,
        created_at=timestamp,
    )
    db.add(message)
    if kind == MessageKind.USER:
        chat.activity_at = timestamp
        chat.last_user_message_at = timestamp
    await db.flush()
    return message


def last_active_at(chat: ChatModel) -> dt.datetime | None:
    """Latest user submission or final answer, not status or partial output."""
    timestamps = []
    for message in chat.messages:
        if message.kind == MessageKind.USER:
            timestamps.append(message.created_at)
        elif message.kind == MessageKind.ASSISTANT:
            item = message.data.get("item") or {}
            if message.data.get("streaming") or item.get("phase") == "commentary":
                continue
            completed_at = message.data.get("completed_at")
            timestamps.append(
                dt.datetime.fromisoformat(completed_at)
                if isinstance(completed_at, str)
                else message.created_at
            )
    return max(
        (
            timestamp.replace(tzinfo=dt.timezone.utc)
            if timestamp.tzinfo is None
            else timestamp
            for timestamp in timestamps
        ),
        default=None,
    )


def chat_order(db: AsyncSession) -> tuple[sa.ColumnElement[Any], ...]:
    """Match last_active_at in SQL so sorting precedes notification pagination."""
    message = ChatMessageModel
    completed = message.data["completed_at"].as_string()
    # SQLite's CAST(... AS DATETIME) returns a year, not a timestamp.
    completed_time = (
        sa.func.strftime("%Y-%m-%d %H:%M:%f", completed)
        if db.get_bind().dialect.name == "sqlite"
        else sa.cast(completed, sa.DateTime(timezone=True))
    )
    timestamp = sa.case(
        (message.kind == MessageKind.USER, message.created_at),
        (
            sa.and_(
                message.kind == MessageKind.ASSISTANT,
                sa.func.coalesce(message.data["streaming"].as_boolean(), False).is_(
                    False
                ),
                sa.func.coalesce(message.data["item"]["phase"].as_string(), "")
                != "commentary",
            ),
            sa.func.coalesce(completed_time, message.created_at),
        ),
    )
    latest = (
        sa.select(sa.func.max(timestamp))
        .where(message.chat_id == ChatModel.id)
        .correlate(ChatModel)
        .scalar_subquery()
    )
    return (
        ChatModel.status.in_([ChatStatus.QUEUED, ChatStatus.RUNNING]).desc(),
        sa.func.coalesce(latest, ChatModel.created_at).desc(),
    )


async def get_chat(
    db: AsyncSession, chat_id: UUID, *, include_shadow: bool = False
) -> ChatModel | None:
    query = (
        sa.select(ChatModel)
        .options(selectinload(ChatModel.messages))
        .execution_options(populate_existing=True)
        .where(ChatModel.id == chat_id)
    )
    if not include_shadow:
        query = query.where(ChatModel.kind != ChatKind.SHADOW)
    return cast(ChatModel | None, await db.scalar(query))


async def list_chats(
    db: AsyncSession,
    *,
    query: str | None = None,
    project_id: UUID | None = None,
    limit: int = 20,
    offset: int = 0,
    unassigned: bool = False,
) -> list[ChatModel]:
    statement = (
        sa.select(ChatModel)
        .where(ChatModel.kind != ChatKind.SHADOW, ChatModel.archived_at.is_(None))
        .order_by(ChatModel.activity_at.desc(), ChatModel.id.desc())
        .offset(offset)
        .limit(limit)
    )
    if query:
        for token in query.strip().split():
            reference = re.fullmatch(r"([bm]?)(\d{1,3})", token.lower())
            if reference:
                prefix, number = reference.groups()
                columns = {"b": ChatModel.bot_number, "m": ChatModel.manager_number}
                candidates = [columns[prefix]] if prefix else list(columns.values())
                statement = statement.where(
                    sa.or_(
                        ChatModel.title.ilike(f"%{token}%"),
                        *(
                            sa.cast(column, sa.String).ilike(f"%{number}%")
                            for column in candidates
                        ),
                    )
                )
            else:
                pattern = (
                    "%"
                    + "%".join(
                        "\\" + char if char in "\\%_" else char for char in token
                    )
                    + "%"
                )
                statement = statement.where(ChatModel.title.ilike(pattern, escape="\\"))
    if unassigned:
        statement = statement.where(ChatModel.project_id.is_(None))
    elif project_id:
        statement = statement.where(ChatModel.project_id == project_id)
    return list((await db.scalars(statement)).all())


async def latest_bot(db: AsyncSession, number: int) -> ChatModel | None:
    return cast(
        ChatModel | None,
        await db.scalar(
            sa.select(ChatModel)
            .where(ChatModel.kind == ChatKind.BOT, ChatModel.bot_number == number)
            .order_by(ChatModel.created_at.desc())
            .limit(1)
        ),
    )


async def set_status(
    db: AsyncSession, chat: ChatModel, status: ChatStatus
) -> ChatModel:
    if chat.status == status:
        return chat
    timestamp = now_utc()
    if chat.running_since and status != ChatStatus.RUNNING:
        started = chat.running_since.replace(tzinfo=dt.timezone.utc)
        chat.elapsed_seconds += max(0, (timestamp - started).total_seconds())
        chat.running_since = None
    if status == ChatStatus.RUNNING and not chat.running_since:
        chat.running_since = timestamp
    chat.status = status
    chat.last_status_change_at = timestamp
    chat.activity_at = timestamp
    await db.flush()
    return chat


def chat_notice(chat: ChatModel) -> tuple[str | None, UUID | None]:
    """Describe the current outcome using persisted status and the latest turn."""
    defaults = {
        ChatStatus.INTERRUPTED: "Interrupted by the user. The conversation can be continued.",
        ChatStatus.CANCELLED: "Cancelled before the next operation.",
        ChatStatus.ERROR: "The turn could not continue.",
        ChatStatus.BLOCKED: "Blocked: a prerequisite needs attention.",
    }
    status = ChatStatus(chat.status)
    for message in reversed(chat.messages):
        if message.kind == MessageKind.USER:
            break
        event = message.data.get("event")
        if event == "location_changed" and status in {
            ChatStatus.IDLE,
            ChatStatus.INTERRUPTED,
        }:
            return message.content, message.id
        if status not in defaults:
            continue
        if message.kind == MessageKind.ERROR:
            return message.content, message.id
        if (
            (event == "status_changed" and message.data.get("status") == status)
            or (event == "location_unavailable" and status == ChatStatus.BLOCKED)
            or (event == "turn_start_failed" and status == ChatStatus.ERROR)
        ):
            return message.content, message.id
        if event == "turn/completed" and status == ChatStatus.ERROR:
            turn = message.data.get("turn")
            error = turn.get("error") if isinstance(turn, dict) else None
            if not isinstance(error, dict):
                continue
            detail = error.get("message")
            if isinstance(detail, str) and detail:
                return f"The turn could not continue: {detail.rstrip('.')}.", message.id
    return defaults.get(status), None


async def move_chat(
    db: AsyncSession,
    chat: ChatModel,
    *,
    location_id: UUID,
    location_name: str,
    location_path: str,
) -> ChatModel:
    if chat.location_id == location_id:
        return chat

    chat.location_id = location_id
    await note_location_change(
        db,
        chat,
        location_id=location_id,
        location_name=location_name,
        location_path=location_path,
    )
    return chat


async def note_location_change(
    db: AsyncSession,
    chat: ChatModel,
    *,
    location_id: UUID,
    location_name: str,
    location_path: str,
) -> None:
    if ChatStatus(chat.status).active:
        await set_status(db, chat, ChatStatus.INTERRUPTED)
    await append_message(
        db,
        chat,
        kind=MessageKind.EVENT,
        content=f"Moved to the {location_name} location ({location_path})",
        data={"event": "location_changed", "location_id": str(location_id)},
    )


async def create_notification(
    db: AsyncSession,
    *,
    chat: ChatModel,
    body: str,
    source_chat_id: UUID | None = None,
) -> NotificationModel:
    if chat.kind == ChatKind.SHADOW:
        raise ValueError("shadow chats cannot own visible notifications")
    notification = NotificationModel(
        chat_id=chat.id, source_chat_id=source_chat_id, body=body.strip()
    )
    db.add(notification)
    await db.flush()
    if db.get_bind().dialect.name == "postgresql":
        await db.execute(
            sa.select(
                sa.func.pg_notify(
                    NOTIFICATION_CHANNEL,
                    json.dumps(
                        {
                            "kind": "notification_created",
                            "notification_id": str(notification.id),
                            "chat_id": str(notification.chat_id),
                        }
                    ),
                )
            )
        )
    return notification
