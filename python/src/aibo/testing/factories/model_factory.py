import datetime as dt
from typing import Any, Optional
from uuid import UUID, uuid4

import sqlalchemy as sa

from aibo.common.chat import (
    CompletionErrorContent,
    FunctionRequestContent,
    FunctionResponseContent,
    FunctionResponseErrorType,
    FunctionResponseStatus,
    HumanSource,
    MessageContent,
    MessageRole,
    MessageSource,
    OpenAIModelSource,
    ProgrammaticSource,
    TextMessageContent,
    stringify_message_contents,
)
from aibo.common.openai import CompletionError
from aibo.common.time import now_utc
from aibo.core import chat
from aibo.db.client import get_session
from aibo.db.models import ConversationModel, MessageModel
from aibo.testing.factories.base_factory import BaseFactory, BaseModelFactory, fake

__all__ = [
    "MessageModelFactory",
    "ConversationModelFactory",
]


class MessageModelFactory(BaseModelFactory[MessageModel]):
    @staticmethod
    async def build(
        *,
        conversation: Optional[ConversationModel] = None,
        parent: Optional[MessageModel] = None,
        source: Optional[MessageSource] = None,
        role: Optional[MessageRole] = None,
        contents: Optional[list[MessageContent]] = None,
        created_at: Optional[dt.datetime] = None,
        deleted_at: Optional[dt.datetime] = None,
    ) -> MessageModel:
        from aibo.testing import factory

        conversation = conversation or await ConversationModelFactory.create()
        role = role or fake.random_element(elements=MessageRole)
        source = source or await factory.MessageSourceFactory.build(role=role)
        contents = contents or [await factory.TextMessageContentFactory.build()]

        return MessageModel(
            trace_id=conversation.trace_id,
            conversation_id=conversation.id,
            parent_id=parent and parent.id,
            original_parent_id=parent and parent.id,
            source=source,
            source_text=str(source),
            role=role or fake.enum(MessageRole),
            contents=contents,
            content_text=stringify_message_contents(contents),
            created_at=created_at or now_utc(),
            deleted_at=deleted_at,
        )


class ConversationModelFactory(BaseModelFactory[ConversationModel]):
    @staticmethod
    async def build(
        *,
        trace_id: Optional[UUID] = None,
        title: Optional[str] = None,
        openai_model_source: Optional[OpenAIModelSource] = None,
        enabled_package_names: Optional[list[str]] = None,
        conversation_depth: Optional[int] = None,
        created_at: Optional[dt.datetime] = None,
        deleted_at: Optional[dt.datetime] = None,
    ) -> ConversationModel:
        from aibo.testing import factory

        return ConversationModel(
            trace_id=trace_id or uuid4(),
            title=title or fake.sentence(),
            openai_model_source=openai_model_source
            or await factory.OpenAIModelSourceFactory.build(),
            enabled_package_names=enabled_package_names or [],
            conversation_depth=conversation_depth or 0,
            created_at=created_at or now_utc(),
            deleted_at=deleted_at,
        )

    @classmethod
    async def post_create(cls, obj: ConversationModel) -> ConversationModel:
        conversation = obj
        system_message = await MessageModelFactory.create(
            conversation=conversation,
            role=MessageRole.SYSTEM,
        )

        conversation.root_message_id = system_message.id
        conversation.current_message_id = system_message.id

        async with get_session() as session:
            await session.execute(
                sa.update(ConversationModel)
                .where(ConversationModel.id == conversation.id)
                .values(
                    root_message_id=system_message.id,
                    current_message_id=system_message.id,
                )
            )
            await session.commit()

        return conversation
