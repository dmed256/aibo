from typing import cast
from uuid import UUID

import sqlalchemy as sa
from sqlalchemy.ext.asyncio import AsyncSession

from aibo.core.workspace import ChatKind, MessageKind, user_objective
from aibo.db.models import ChatMessageModel, ChatModel
from aibo.server.routes.attachments import validate_paths
from aibo.server.routes.common import not_found


async def source_content(db: AsyncSession, message_id: UUID) -> tuple[str, list[str]]:
    message = await db.scalar(
        sa.select(ChatMessageModel)
        .join(ChatModel)
        .where(
            ChatMessageModel.id == message_id,
            ChatMessageModel.kind == MessageKind.USER,
            ChatModel.kind == ChatKind.MANAGER,
        )
    )
    if not message:
        raise not_found("manager user message")
    attachments = message.data.get("attachments", [])
    validate_paths(attachments)
    return user_objective(message), list(cast(list[str], attachments))
