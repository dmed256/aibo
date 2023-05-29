import datetime as dt
from typing import TYPE_CHECKING, Optional, Self
from uuid import UUID, uuid4

import sqlalchemy as sa
import sqlalchemy.orm as orm

from aibo.common.chat.message_content import MessageContent, MessageContents
from aibo.common.chat.message_source import MessageSource
from aibo.common.time import now_utc
from aibo.db.client import get_session
from aibo.db.models.base_db_model import BaseDBModel
from aibo.db.models.custom_types import (
    MessageContentColumn,
    MessageContentsColumn,
    MessageSourceColumn,
    UUIDColumn,
)

if TYPE_CHECKING:
    from aibo.db.models.conversation_model import ConversationModel


class MessageModel(BaseDBModel):
    __tablename__ = "messages"
    __table_args__ = (
        sa.Index("messages_idx_trace_id", "trace_id"),
        sa.Index("messages_idx_conversation_id", "conversation_id"),
        sa.Index("messages_idx_created_at", "created_at"),
    )

    id: orm.Mapped[UUID] = orm.mapped_column(
        UUIDColumn, primary_key=True, default=uuid4
    )
    trace_id: orm.Mapped[UUID] = orm.mapped_column(UUIDColumn)

    conversation_id: orm.Mapped[Optional[UUID]] = orm.mapped_column(
        UUIDColumn,
        sa.ForeignKey("conversations.id"),
    )
    parent_id: orm.Mapped[Optional[UUID]] = orm.mapped_column(
        UUIDColumn,
        sa.ForeignKey("messages.id"),
        nullable=True,
    )
    original_parent_id: orm.Mapped[Optional[UUID]] = orm.mapped_column(
        UUIDColumn,
        sa.ForeignKey("messages.id"),
        nullable=True,
    )

    source: orm.Mapped[MessageSource] = orm.mapped_column(MessageSourceColumn)
    source_text: orm.Mapped[str]
    role: orm.Mapped[str]
    contents: orm.Mapped[MessageContents] = orm.mapped_column(MessageContentsColumn)
    content_text: orm.Mapped[str]
    created_at: orm.Mapped[dt.datetime] = orm.mapped_column(default=now_utc)
    deleted_at: orm.Mapped[Optional[dt.datetime]]

    async def soft_delete(self) -> Self:
        if self.deleted_at:
            return self

        async with get_session() as session:
            self.deleted_at = now_utc()
            await session.commit()

        return self

    @staticmethod
    async def get_message_children(id: UUID) -> list["MessageModel"]:
        async with get_session() as session:
            query_result = await session.execute(
                sa.select(MessageModel).where(MessageModel.parent_id == id)
            )

        return list(query_result.scalars().all())
