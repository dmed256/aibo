from __future__ import annotations

import datetime as dt
from typing import TYPE_CHECKING, Any
from uuid import UUID, uuid4

import sqlalchemy as sa
import sqlalchemy.orm as orm

from aibo.common.time import now_utc
from aibo.db.models.base_db_model import BaseDBModel, TimestampMixin

if TYPE_CHECKING:
    from aibo.db.models.location_model import LocationModel
    from aibo.db.models.project_model import ProjectModel

CHAT_KINDS = ("manager", "bot", "shadow")
CHAT_STATUSES = (
    "idle",
    "queued",
    "running",
    "completed",
    "interrupted",
    "cancelled",
    "error",
    "blocked",
)
MESSAGE_KINDS = ("system", "user", "assistant", "tool", "event")


class ChatModel(TimestampMixin, BaseDBModel):
    __tablename__ = "chats"
    __table_args__ = (
        sa.CheckConstraint("kind IN ('manager', 'bot', 'shadow')"),
        sa.CheckConstraint(
            "status IN ('idle', 'queued', 'running', 'completed', "
            "'interrupted', 'cancelled', 'error', 'blocked')"
        ),
        sa.CheckConstraint(
            "(kind = 'bot' AND bot_number BETWEEN 0 AND 255) "
            "OR (kind != 'bot' AND bot_number IS NULL)"
        ),
        sa.CheckConstraint(
            "(kind = 'manager' AND manager_number IS NOT NULL "
            "AND manager_number BETWEEN 0 AND 255) "
            "OR (kind != 'manager' AND manager_number IS NULL)",
            name="chats_manager_number_check",
        ),
        sa.Index("chats_manager_number_idx", "manager_number", "created_at"),
        sa.Index("chats_activity_idx", "activity_at"),
        sa.Index("chats_bot_number_idx", "bot_number", "created_at"),
    )

    id: orm.Mapped[UUID] = orm.mapped_column(sa.Uuid, primary_key=True, default=uuid4)
    kind: orm.Mapped[str] = orm.mapped_column(sa.String(16))
    bot_number: orm.Mapped[int | None]
    manager_number: orm.Mapped[int | None]
    title: orm.Mapped[str] = orm.mapped_column(sa.String(300), default="New chat")
    status: orm.Mapped[str] = orm.mapped_column(sa.String(16), default="idle")
    codex_thread_id: orm.Mapped[str | None] = orm.mapped_column(
        sa.String(100), unique=True
    )
    active_turn_id: orm.Mapped[str | None] = orm.mapped_column(sa.String(100))
    goal_enabled: orm.Mapped[bool] = orm.mapped_column(
        default=True, server_default=sa.true()
    )
    goal: orm.Mapped[dict[str, Any] | None] = orm.mapped_column(sa.JSON)
    tokens_used: orm.Mapped[int] = orm.mapped_column(
        sa.BigInteger, default=0, server_default="0"
    )
    elapsed_seconds: orm.Mapped[float] = orm.mapped_column(
        default=0, server_default="0"
    )
    running_since: orm.Mapped[dt.datetime | None] = orm.mapped_column(
        sa.DateTime(timezone=True)
    )

    location_id: orm.Mapped[UUID | None] = orm.mapped_column(
        sa.ForeignKey("locations.id", ondelete="SET NULL")
    )
    project_id: orm.Mapped[UUID | None] = orm.mapped_column(
        sa.ForeignKey("projects.id", ondelete="SET NULL")
    )
    origin_chat_id: orm.Mapped[UUID | None] = orm.mapped_column(
        sa.ForeignKey("chats.id", ondelete="CASCADE")
    )
    shadow_for_turn_id: orm.Mapped[str | None] = orm.mapped_column(
        sa.String(100), unique=True
    )

    activity_at: orm.Mapped[dt.datetime] = orm.mapped_column(
        sa.DateTime(timezone=True), default=now_utc
    )
    last_user_message_at: orm.Mapped[dt.datetime | None] = orm.mapped_column(
        sa.DateTime(timezone=True)
    )
    last_status_change_at: orm.Mapped[dt.datetime] = orm.mapped_column(
        sa.DateTime(timezone=True), default=now_utc
    )
    archived_at: orm.Mapped[dt.datetime | None] = orm.mapped_column(
        sa.DateTime(timezone=True)
    )

    location: orm.Mapped[LocationModel | None] = orm.relationship(lazy="selectin")
    project: orm.Mapped[ProjectModel | None] = orm.relationship(lazy="selectin")
    messages: orm.Mapped[list[ChatMessageModel]] = orm.relationship(
        back_populates="chat",
        cascade="all, delete-orphan",
        lazy="selectin",
        order_by="ChatMessageModel.created_at",
    )


class ChatMessageModel(BaseDBModel):
    __tablename__ = "chat_messages"
    __table_args__ = (
        sa.CheckConstraint(
            "kind IN ('system', 'user', 'assistant', 'tool', 'event', 'error')"
        ),
        sa.UniqueConstraint("chat_id", "external_id"),
        sa.Index("chat_messages_chat_idx", "chat_id", "created_at"),
    )

    id: orm.Mapped[UUID] = orm.mapped_column(sa.Uuid, primary_key=True, default=uuid4)
    chat_id: orm.Mapped[UUID] = orm.mapped_column(
        sa.ForeignKey("chats.id", ondelete="CASCADE")
    )
    kind: orm.Mapped[str] = orm.mapped_column(sa.String(16))
    content: orm.Mapped[str] = orm.mapped_column(sa.Text)
    data: orm.Mapped[dict[str, Any]] = orm.mapped_column(sa.JSON, default=dict)
    external_id: orm.Mapped[str | None] = orm.mapped_column(sa.String(200))
    created_at: orm.Mapped[dt.datetime] = orm.mapped_column(
        sa.DateTime(timezone=True), default=now_utc
    )

    chat: orm.Mapped[ChatModel] = orm.relationship(back_populates="messages")


class NotificationModel(BaseDBModel):
    __tablename__ = "notifications"
    __table_args__ = (sa.Index("notifications_read_idx", "read_at", "created_at"),)

    id: orm.Mapped[UUID] = orm.mapped_column(sa.Uuid, primary_key=True, default=uuid4)
    chat_id: orm.Mapped[UUID] = orm.mapped_column(
        sa.ForeignKey("chats.id", ondelete="CASCADE")
    )
    source_chat_id: orm.Mapped[UUID | None] = orm.mapped_column(
        sa.ForeignKey("chats.id", ondelete="SET NULL"), unique=True
    )
    body: orm.Mapped[str] = orm.mapped_column(sa.Text)
    created_at: orm.Mapped[dt.datetime] = orm.mapped_column(
        sa.DateTime(timezone=True), default=now_utc
    )
    read_at: orm.Mapped[dt.datetime | None] = orm.mapped_column(
        sa.DateTime(timezone=True)
    )

    chat: orm.Mapped[ChatModel] = orm.relationship(
        foreign_keys=[chat_id], lazy="selectin"
    )


class CounterModel(BaseDBModel):
    __tablename__ = "counters"

    name: orm.Mapped[str] = orm.mapped_column(sa.String(80), primary_key=True)
    value: orm.Mapped[int]


class ModelSettingsModel(BaseDBModel):
    __tablename__ = "model_settings"

    id: orm.Mapped[int] = orm.mapped_column(primary_key=True)
    models: orm.Mapped[dict[str, Any]] = orm.mapped_column(sa.JSON)
