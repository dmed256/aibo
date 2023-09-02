import datetime as dt
from typing import Any, Optional

from aibo.common.time import now_utc
from aibo.core import chat
from aibo.db import documents
from aibo.testing.factories.base_factory import BaseFactory, fake
from aibo.testing.factories.documents_factory import (
    CompletionErrorContentFactory,
    ConversationDocumentFactory,
    HumanSourceFactory,
    MessageDocumentFactory,
    OpenAIModelSourceFactory,
    ProgrammaticSourceFactory,
    TextMessageContentFactory,
)

__all__ = [
    "CreateMessageInputsFactory",
    "StreamingMessageChunkFactory",
    "ErrorMessageChunkFactory",
    "MessageFactory",
    "ConversationFactory",
]


class CreateMessageInputsFactory(BaseFactory[chat.CreateMessageInputs]):
    @staticmethod
    async def build(
        *,
        role: Optional[chat.MessageRole] = None,
        content: Optional[chat.MessageContent] = None,
    ) -> chat.CreateMessageInputs:
        return chat.CreateMessageInputs(
            role=role or fake.random_element(elements=chat.MessageRole),
            content=content or await TextMessageContentFactory.build(),
        )


class StreamingMessageChunkFactory(BaseFactory[chat.StreamingMessageChunk]):
    @staticmethod
    async def build(*, text: Optional[str] = None) -> chat.StreamingMessageChunk:
        return chat.StreamingMessageChunk(text=text or fake.sentence())


class ErrorMessageChunkFactory(BaseFactory[chat.ErrorMessageChunk]):
    @staticmethod
    async def build(
        *,
        source: Optional[chat.ProgrammaticSource] = None,
        content: Optional[chat.CompletionErrorContent] = None,
    ) -> chat.ErrorMessageChunk:
        return chat.ErrorMessageChunk(
            source=source or await ProgrammaticSourceFactory.build(),
            content=content or await CompletionErrorContentFactory.build(),
        )


class MessageFactory(BaseFactory[chat.Message]):
    @staticmethod
    async def build(
        *,
        status: Optional[chat.Message.Status] = None,
        conversation: Optional[chat.Conversation] = None,
        parent: Optional[chat.Message] = None,
        source: Optional[chat.MessageSource] = None,
        role: Optional[chat.MessageRole] = None,
        content: Optional[chat.MessageContent] = None,
        created_at: Optional[dt.datetime] = None,
        deleted_at: Optional[dt.datetime] = None,
    ) -> chat.Message:
        conversation = conversation or await ConversationFactory.build()
        conversation_doc = conversation.get_document()

        message_doc = await MessageDocumentFactory.create(
            conversation_doc=conversation_doc,
            parent_doc=parent and parent.get_document(),
            source=source,
            role=role,
            content=content,
            created_at=created_at,
            deleted_at=deleted_at,
        )
        return chat.Message.from_document(message_doc, history=[])


class ConversationFactory(BaseFactory[chat.Conversation]):
    @staticmethod
    async def build(
        *,
        title: Optional[str] = None,
        openai_model_source: Optional[chat.OpenAIModelSource] = None,
        enabled_tools: Optional[list[Any]] = None,
        conversation_depth: Optional[int] = None,
        created_at: Optional[dt.datetime] = None,
        deleted_at: Optional[dt.datetime] = None,
    ) -> chat.Conversation:
        enabled_tools = enabled_tools or []

        conversation_document = await ConversationDocumentFactory.create(
            title=title,
            openai_model_source=openai_model_source,
            enabled_tool_names=[tool.name for tool in enabled_tools],
            conversation_depth=conversation_depth,
            created_at=created_at,
            deleted_at=deleted_at,
        )

        conversation = await chat.Conversation.from_document(conversation_document)
        await conversation.sync_messages()
        return conversation
