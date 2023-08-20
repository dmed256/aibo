import datetime as dt
from typing import Optional, Self
from uuid import UUID

from pydantic import BaseModel, ValidationError, root_validator

from aibo.core import chat

__all__ = ["Message", "ConversationSummary", "Conversation", "MessageEdge"]


class Message(BaseModel):
    id: UUID
    status: chat.Message.Status
    conversation_id: UUID
    parent_id: Optional[UUID] = None
    source: chat.MessageSource
    source_text: str
    role: chat.MessageRole
    content: chat.MessageContent
    content_text: str
    created_at: dt.datetime
    deleted_at: Optional[dt.datetime] = None

    @classmethod
    def from_chat(cls, message: chat.Message) -> Self:
        return cls(
            id=message.id,
            status=message.status,
            conversation_id=message.conversation_id,
            parent_id=message.parent_id,
            source=message.source,
            source_text=message.source_text,
            role=message.role,
            content=message.content,
            content_text=message.content_text,
            created_at=message.created_at,
            deleted_at=message.deleted_at,
        )


class ConversationSummary(BaseModel):
    id: UUID
    title: str
    openai_model_source: chat.OpenAIModelSource
    enabled_tool_names: list[str]
    created_at: dt.datetime

    @classmethod
    def from_chat(cls, conversation: chat.ConversationSummary) -> Self:
        return cls(
            id=conversation.id,
            title=conversation.title,
            openai_model_source=conversation.openai_model_source,
            enabled_tool_names=[tool.id for tool in conversation.enabled_tools],
            created_at=conversation.created_at,
        )


class Conversation(ConversationSummary):
    root_message_id: UUID
    current_message_id: UUID
    all_messages: dict[UUID, Message]

    @classmethod
    def from_chat(cls, conversation: chat.Conversation) -> Self:  # type: ignore[override]
        return cls(
            **dict(ConversationSummary.from_chat(conversation)),
            root_message_id=conversation.root_message.id,
            current_message_id=conversation.current_message.id,
            all_messages={
                message.id: Message.from_chat(message)
                for message in conversation.all_messages.values()
            },
        )


class MessageEdge(BaseModel):
    conversation_id: UUID
    parent_id: UUID
    child_id: UUID
    created_at: dt.datetime

    @classmethod
    def from_chat(cls, message_edge: chat.MessageEdgeDocument) -> Self:
        return cls(**message_edge.dict())
