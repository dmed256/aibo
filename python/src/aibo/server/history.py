"""Read-only transcript projections. Hidden bodies never leave the database."""
import datetime as dt
from uuid import UUID

import sqlalchemy as sa
from fastapi import HTTPException
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.orm import noload

from aibo.core.workspace import ChatKind, chat_notice, last_active_at
from aibo.db.models import ChatMessageModel as M
from aibo.db.models import ChatModel
from aibo.server import schemas


class History(schemas.Chat):
    older_before: UUID | None = None


class Details(schemas.BaseModel):
    messages: list[schemas.Message]
    next_after: UUID | None = None


def visible() -> sa.ColumnElement[bool]:
    return M.kind.in_(["user", "assistant", "error"])


def stored() -> sa.ColumnElement[bool]:
    return sa.func.coalesce(M.data["streaming"].as_boolean(), False).is_(False)


def routine_turn() -> sa.ColumnElement[bool]:
    """Lifecycle bookkeeping is durable, but never transcript content."""
    return sa.and_(
        M.kind == "event",
        sa.func.coalesce(
            sa.or_(
                M.content.in_(["Codex turn started", "Codex turn completed"]),
                M.data["event"].as_string().in_(["turn_started", "turn/started"]),
                sa.and_(
                    M.data["event"].as_string() == "turn/completed",
                    M.data["turn"]["status"].as_string() == "completed",
                ),
            ),
            False,
        ),
    )


async def boundary(db: AsyncSession, chat_id: UUID, message_id: UUID) -> M:
    # Only cursor metadata is needed, even when the referenced body is enormous.
    row = (
        await db.execute(
            sa.select(M.id, M.created_at).where(
                M.chat_id == chat_id, M.id == message_id
            )
        )
    ).first()
    if row is None:
        raise HTTPException(404, "Message cursor not found in this chat")
    return M(id=row.id, created_at=row.created_at)


def position(message: M) -> tuple[object, UUID]:
    return message.created_at, message.id


async def history(
    db: AsyncSession, chat_id: UUID, before: UUID | None = None
) -> History:
    chat = await db.scalar(
        sa.select(ChatModel)
        .options(noload(ChatModel.messages))
        .where(ChatModel.id == chat_id, ChatModel.kind != ChatKind.SHADOW)
    )
    if chat is None:
        raise HTTPException(404, "Chat not found")
    where = [M.chat_id == chat_id, stored(), ~routine_turn()]
    if before:
        cursor = await boundary(db, chat_id, before)
        where.append(sa.tuple_(M.created_at, M.id) < position(cursor))
    recent = (
        await db.execute(
            sa.select(M.id, M.created_at)
            .where(*where, visible())
            .order_by(M.created_at.desc(), M.id.desc())
            .limit(61)
        )
    ).all()
    older = len(recent) > 60
    if older:
        where.append(
            sa.tuple_(M.created_at, M.id) >= (recent[59].created_at, recent[59].id)
        )
    # Preserve outcome notices, but omit large protocol/tool/context payloads in SQL.
    notice = (
        M.data["event"]
        .as_string()
        .in_(
            [
                "location_changed",
                "status_changed",
                "location_unavailable",
                "turn_start_failed",
                "turn/completed",
            ]
        )
    )
    body = sa.or_(
        visible(), sa.and_(notice, M.data["event"].as_string() != "turn/completed")
    )
    rows = (
        await db.execute(
            sa.select(
                M.id,
                M.kind,
                M.created_at,
                sa.case((body, M.content), else_="").label("content"),
                sa.case((visible(), M.data), else_=sa.literal({}, type_=sa.JSON)).label(
                    "data"
                ),
                visible().label("visible"),
                M.data["event"].as_string().label("event"),
                M.data["status"].as_string().label("status"),
                M.data["turn"]["error"].label("error"),
            )
            .where(*where)
            .order_by(M.created_at, M.id)
        )
    ).all()
    # Transient objects are not attached to the session or used by write paths.
    projected = [
        M(
            id=r.id,
            kind=r.kind,
            created_at=r.created_at,
            content=r.content,
            data=r.data
            if r.visible
            else {"event": r.event, "status": r.status, "turn": {"error": r.error}},
        )
        for r in rows
    ]
    summary = schemas.ChatSummary.from_model(chat)
    messages: list[schemas.Message] = []
    for row, message in zip(rows, projected):
        if row.visible:
            messages.append(schemas.Message.model_validate(message))
        elif messages and messages[-1].data.get("hidden_count"):
            group = messages[-1].data
            group["hidden_count"] = int(str(group["hidden_count"])) + 1
            group["last_id"] = str(message.id)
        else:
            messages.append(
                schemas.Message(
                    id=message.id,
                    kind="event",
                    content="",
                    created_at=message.created_at,
                    data={
                        "hidden_count": 1,
                        "first_id": str(message.id),
                        "last_id": str(message.id),
                    },
                )
            )
    # Compute metadata from the projected transcript, not hidden body hydration.
    transient = ChatModel(status=chat.status, messages=projected)
    notice_text, notice_id = chat_notice(transient)
    summary.last_active_at = last_active_at(transient)
    return History(
        **summary.model_dump(),
        messages=messages,
        notice=notice_text,
        notice_message_id=notice_id,
        older_before=rows[0].id if older else None,
        history_version=max(
            [chat.updated_at.replace(tzinfo=dt.timezone.utc)]
            + [r.created_at.replace(tzinfo=dt.timezone.utc) for r in rows]
            + ([summary.last_active_at] if summary.last_active_at else [])
        ).isoformat(),
    )


async def details(
    db: AsyncSession, chat_id: UUID, first: UUID, last: UUID, after: UUID | None = None
) -> Details:
    public = await db.scalar(
        sa.select(ChatModel.id).where(
            ChatModel.id == chat_id, ChatModel.kind != ChatKind.SHADOW
        )
    )
    if public is None:
        raise HTTPException(404, "Chat not found")
    start = await boundary(db, chat_id, first)
    end = await boundary(db, chat_id, last)
    where = [
        M.chat_id == chat_id,
        stored(),
        ~routine_turn(),
        sa.tuple_(M.created_at, M.id) >= position(start),
        sa.tuple_(M.created_at, M.id) <= position(end),
    ]
    if after:
        cursor = await boundary(db, chat_id, after)
        where.append(sa.tuple_(M.created_at, M.id) > position(cursor))
    rows = list(
        (
            await db.scalars(
                sa.select(M).where(*where).order_by(M.created_at, M.id).limit(31)
            )
        ).all()
    )
    return Details(
        messages=[schemas.Message.model_validate(m) for m in rows[:30]],
        next_after=rows[29].id if len(rows) > 30 else None,
    )
