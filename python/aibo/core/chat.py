from __future__ import annotations

import asyncio
import datetime as dt
import functools
import re
from enum import StrEnum
from typing import (
    Annotated,
    Any,
    AsyncGenerator,
    Iterable,
    Literal,
    Optional,
    Self,
    Sequence,
    Type,
    Union,
    cast,
)
from uuid import UUID, uuid4

import openai
import pymongo
import tenacity
from pydantic import BaseModel, Field
from termcolor import colored

from aibo.common.constants import NULL_UUID, Env
from aibo.common.result import Error, Result
from aibo.common.time import now_utc
from aibo.core.openai import (
    ErrorMessageChunk,
    OpenAIMessage,
    OpenAIRole,
    StreamingMessageChunk,
    StreamingMessageResult,
    SuccessMessageChunk,
    stream_completion,
)
from aibo.db.documents import (
    CompletionErrorContent,
    ConversationDocument,
    HumanSource,
    MessageContent,
    MessageDocument,
    MessageEdgeDocument,
    MessageRole,
    MessageSource,
    OpenAIModelSource,
    ProgrammaticSource,
    TextMessageContent,
    ToolRequestContent,
    ToolResponseContent,
    ToolResponseErrorType,
    ToolResponseStatus,
)

__all__ = [
    "CompletionErrorContent",
    "Conversation",
    "ConversationDocument",
    "ConversationSummary",
    "ErrorMessageChunk",
    "HumanSource",
    "Message",
    "MessageContent",
    "MessageDocument",
    "MessageEdgeDocument",
    "MessageRole",
    "MessageSource",
    "OpenAIModelSource",
    "ProgrammaticSource",
    "StreamingMessageChunk",
    "StreamingMessageResult",
    "SuccessMessageChunk",
    "TextMessageContent",
    "ToolRequestContent",
    "ToolResponseContent",
    "ToolResponseErrorType",
    "ToolResponseStatus",
]


ROLE_COLORS: dict[MessageRole, str] = {
    MessageRole.SYSTEM: "green",
    MessageRole.USER: "cyan",
    MessageRole.ASSISTANT: "yellow",
    MessageRole.ERROR: "light_gray",
}

OPENAI_ROLES: dict[MessageRole, OpenAIRole] = {
    MessageRole.SYSTEM: "system",
    MessageRole.USER: "user",
    MessageRole.ASSISTANT: "assistant",
    MessageRole.FUNCTION: "function",
}


class CreateMessageInputs(BaseModel):
    role: MessageRole
    content: MessageContent


class Message(BaseModel):
    class Status(StrEnum):
        STREAMING = "streaming"
        COMPLETED = "completed"

    id: UUID
    status: Status
    conversation_id: UUID
    parent_id: Optional[UUID] = None
    source: MessageSource
    role: MessageRole
    content: MessageContent
    created_at: dt.datetime
    deleted_at: Optional[dt.datetime] = None
    history: list["Message"] = Field([], exclude=True)

    @classmethod
    def from_document(
        cls, doc: MessageDocument, *, history: list["Message"]
    ) -> "Message":
        return cls(
            id=doc.id,
            status=cls.Status.COMPLETED,
            conversation_id=doc.conversation_id,
            parent_id=doc.parent_id,
            source=doc.source,
            role=doc.role,
            content=doc.content,
            created_at=doc.created_at,
            deleted_at=doc.deleted_at,
            history=history,
        )

    async def get_document(self) -> MessageDocument:
        return cast(MessageDocument, await MessageDocument.by_id(self.id))

    def to_document(self) -> MessageDocument:
        return MessageDocument(
            id=self.id,
            conversation_id=self.conversation_id,
            parent_id=self.parent_id,
            source=self.source,
            source_text=self.source_text,
            role=self.role,
            content=self.content,
            content_text=self.content_text,
            created_at=self.created_at,
            deleted_at=self.deleted_at,
        )

    @property
    def source_text(self) -> str:
        return str(self.source)

    @property
    def content_text(self) -> str:
        return str(self.content)

    def to_openai(self) -> Optional[OpenAIMessage]:
        content = self.content.to_openai()
        if content is None:
            return None

        openai_role = OPENAI_ROLES[self.role]

        message = OpenAIMessage(
            role=openai_role,
            content=content,
        )
        if isinstance(self.content, ToolResponseContent):
            message.name = self.content.tool
            message.function_call = self.content.response

        return message

    async def get_children(self) -> list["Message"]:
        child_history = [*self.history, self]
        return [
            self.from_document(doc, history=child_history)
            for doc in await MessageDocument.get_message_children(
                conversation_id=self.conversation_id,
                parent_id=self.id,
            )
        ]

    async def get_conversation(self) -> Conversation:
        conversation = await Conversation.get(self.conversation_id)
        assert conversation, f"Conversation no longer exists: {self.conversation_id}"

        return conversation

    async def change_parent(self, parent_id: UUID, changed_at: dt.datetime) -> None:
        self.parent_id = parent_id
        await self.to_document().change_parent(
            parent_id,
            changed_at=changed_at,
        )


class ConversationSummary(BaseModel):
    id: UUID
    title: str

    # Model
    openai_model_source: OpenAIModelSource
    enabled_tools: list[Any]  # Replace Any with tool

    # Nested info
    conversation_depth: int

    created_at: dt.datetime

    @classmethod
    def from_document(cls, doc: ConversationDocument) -> Self:
        return cls(
            id=doc.id,
            title=doc.title,
            openai_model_source=doc.openai_model_source,
            enabled_tools=[],
            conversation_depth=doc.conversation_depth,
            created_at=doc.created_at,
        )

    async def get_document(self) -> ConversationDocument:
        return cast(ConversationDocument, await ConversationDocument.by_id(self.id))

    @classmethod
    async def search(
        cls,
        *,
        after_date: Optional[dt.datetime],
        before_date: Optional[dt.datetime],
        query: Optional[str],
    ) -> list[Self]:
        filters: dict[str, Any] = {}

        if after_date or before_date:
            created_at_filter = {}
            if after_date:
                created_at_filter["$gte"] = after_date
            if before_date:
                created_at_filter["$lte"] = before_date
            filters["created_at"] = created_at_filter

        if query := query and query.strip():
            filters["title"] = {
                "$text": {
                    "$search": " ".join(word.strip() for word in query.split()),
                },
            }

        conversation_docs = await ConversationDocument.safe_find(**filters)
        return [
            cls.from_document(conversation_doc)
            for conversation_doc in conversation_docs
        ]

    async def set_title(self, title: str) -> None:
        self.title = title
        await ConversationDocument.partial_update(
            id=self.id,
            title=title,
        )


class Conversation(ConversationSummary):
    root_message: Message
    current_message: Message
    all_messages: dict[UUID, Message]

    @classmethod
    async def create(
        cls,
        *,
        openai_model_source: OpenAIModelSource,
        enabled_tools: list[Any],  # Replace Any with tool
        system_message_inputs: CreateMessageInputs,
        title: Optional[str] = None,
    ) -> Self:
        conversation_doc = await ConversationDocument(
            title=title or "New chat",
            openai_model_source=openai_model_source,
            enabled_tool_names=[tool.name for tool in enabled_tools],
            root_message_id=NULL_UUID,
            current_message_id=NULL_UUID,
            origin_message_id=None,
        ).insert()

        system_source = HumanSource(user="dmed")
        system_content = system_message_inputs.content
        system_message_doc = await MessageDocument(
            conversation_id=conversation_doc.id,
            parent_id=None,
            source=system_source,
            source_text=str(system_source),
            role=system_message_inputs.role,
            content=system_content,
            content_text=str(system_content),
        ).insert()

        conversation_doc = await ConversationDocument.partial_update(
            id=conversation_doc.id,
            root_message_id=system_message_doc.id,
            current_message_id=system_message_doc.id,
        )

        return await cls.from_document(conversation_doc)

    async def soft_delete(self, *, soft_delete_messages: bool = False) -> None:
        if soft_delete_messages:
            await MessageDocument.collection.update_many(
                {"conversation_id": self.id},
                {
                    "$set": {
                        "deleted_at": now_utc(),
                    },
                },
            )

        conversation_doc = await self.get_document()
        await conversation_doc.soft_delete()

    @classmethod
    async def from_document(cls, doc: ConversationDocument) -> Self:  # type: ignore[override]
        root_message_doc = await MessageDocument.by_id(doc.root_message_id)
        current_message_doc = await MessageDocument.by_id(doc.current_message_id)
        assert root_message_doc, f"Message no longer exists: {doc.root_message_id}"
        assert (
            current_message_doc
        ), f"Message no longer exists: {doc.current_message_id}"

        root_message = Message.from_document(
            root_message_doc,
            history=[],
        )
        current_message = Message.from_document(
            current_message_doc,
            history=[],
        )

        conversation = cls(
            **dict(ConversationSummary.from_document(doc)),
            root_message=root_message,
            current_message=current_message,
            all_messages={},
        )
        await conversation.sync_messages()

        return conversation

    @classmethod
    async def get(cls, id: UUID) -> Optional[Self]:
        conversation_doc = await ConversationDocument.by_id(id)
        if not conversation_doc:
            return None

        return await cls.from_document(conversation_doc)

    async def sync_messages(self) -> None:
        self.all_messages = {
            doc.id: Message.from_document(doc, history=[])
            for doc in await MessageDocument.get_conversation_messages(self.id)
        }
        for message in self.all_messages.values():
            self._set_message_history(message)

    async def get_message_edges(
        self, *, include_deletions: bool = False
    ) -> list[MessageEdgeDocument]:
        if include_deletions:
            return await MessageEdgeDocument.find({"conversation_id": self.id})

        latest_edges = await MessageEdgeDocument.aggregate(
            [
                {"$filter": {"cond": {"conversation_id": self.id}}},
                {
                    "$sort": {
                        "child_id": pymongo.DESCENDING,
                        "created_at": pymongo.DESCENDING,
                    }
                },
                {
                    "$group": {
                        "id": "$_id",
                        "conversation_id": "$conversation_id",
                        "child_id": "$child_id",
                        "parent_id": {"$first": "$parent_id"},
                        "created_at": {"$first": "$created_at"},
                    }
                },
            ]
        )
        return [
            edge
            for edge in latest_edges
            if edge.child_id in self.all_messages
            and edge.parent_id in self.all_messages
        ]

    def _set_message_history(self, message: Message) -> None:
        # No-op if message is root or already has history
        if not message.parent_id or message.history:
            return

        parent_message = self.all_messages[message.parent_id]
        self._set_message_history(parent_message)
        message.history = [*parent_message.history, parent_message]

    async def insert_message(
        self,
        *,
        source: MessageSource,
        role: MessageRole,
        content: MessageContent,
        parent_id: Optional[UUID] = None,
        set_to_current_message: bool = False,
    ) -> Message:
        parent_id = parent_id or self.current_message.id
        updates_current_message = parent_id == self.current_message.id
        message_doc = await MessageDocument(
            conversation_id=self.id,
            parent_id=parent_id,
            source=source,
            source_text=str(source),
            role=role,
            content=content,
            content_text=str(content),
        ).insert()

        inserted_message = Message.from_document(
            message_doc,
            history=[*self.current_message.history, self.current_message],
        )
        self.all_messages[inserted_message.id] = inserted_message

        if updates_current_message:
            self.current_message = inserted_message
            await ConversationDocument.partial_update(
                id=self.id,
                current_message_id=self.current_message.id,
            )
        elif set_to_current_message:
            self.current_message = inserted_message

        return inserted_message

    async def insert_user_message(self, text: str) -> Message:
        env = Env.get()
        return await self.insert_message(
            source=HumanSource(user=env.CURRENT_USER),
            role=MessageRole.USER,
            content=TextMessageContent(text=text),
        )

    async def generate_assistant_message(self) -> Message:
        async for message in self.stream_assistant_message():
            pass
        return message

    async def stream_assistant_message(
        self, *, source: Optional[OpenAIModelSource] = None
    ) -> AsyncGenerator[Message, None]:
        source = source or self.openai_model_source.copy()

        content = ""
        temp_id = uuid4()
        temp_created_at = now_utc()

        async for chunk in self.stream_assistant_message_chunks(source=source):
            if isinstance(chunk, StreamingMessageChunk):
                content += chunk.text
                yield Message(
                    id=temp_id,
                    status=Message.Status.STREAMING,
                    conversation_id=self.id,
                    parent_id=self.current_message.id,
                    source=source,
                    role=MessageRole.ASSISTANT,
                    content=TextMessageContent(text=content),
                    created_at=temp_created_at,
                    history=[],
                )
            elif isinstance(chunk, SuccessMessageChunk):
                yield await self.insert_message(
                    source=source,
                    role=MessageRole.ASSISTANT,
                    content=TextMessageContent(text=content),
                )
            elif isinstance(chunk, ErrorMessageChunk):
                yield await self.insert_message(
                    source=chunk.source,
                    role=MessageRole.ERROR,
                    content=chunk.content,
                )

    async def stream_assistant_message_chunks(
        self,
        *,
        source: Optional[OpenAIModelSource] = None,
    ) -> AsyncGenerator[StreamingMessageResult, None]:
        source = source or self.openai_model_source.copy()

        async for chunk in stream_completion(
            source=source,
            messages=self.get_current_history(),
            tools=self.enabled_tools,
        ):
            yield chunk

    async def generate_title(self, *, model: Optional[str]) -> None:
        env = Env.get()

        title_conversation = await Conversation.create(
            title=f"Title for {self.id}",
            openai_model_source=OpenAIModelSource.build(model=model),
            enabled_tools=[],
            system_message_inputs=CreateMessageInputs(
                role=MessageRole.SYSTEM,
                content=TextMessageContent(
                    text="You are a helpful AI that follows instructions as precise as possible, replying in a concise and succinct manner.",
                ),
            ),
        )

        await title_conversation.insert_message(
            source=HumanSource(user=env.CURRENT_USER),
            role=MessageRole.USER,
            content=TextMessageContent(
                text=f"""
--------------------
{self.stringify_conversation()}
--------------------

Create a small 3-6 word tweet that captures the intent of the above within three to six words and 2 lowercase twitter-like tags. Do not use newlines or quotes. Here are some examples of good correct tweets:
- Docstrings for generate_title #docstring #python
- Capital of Thailand #geography #thailand
- Pokemon guessing game #fun #pokemon
""".strip(),
            ),
        )
        generated_message = await title_conversation.generate_assistant_message()
        await title_conversation.soft_delete()

        await self.set_title(re.sub(r"\s+", " ", str(generated_message.content)))

    async def edit_message(
        self,
        message: Message,
        *,
        source: MessageSource,
        role: MessageRole,
        content: MessageContent,
    ) -> Message:
        if message.parent_id is None:
            raise Result.error(  # type: ignore[misc]
                "Cannot edit root message", error_code=Error.Code.INVALID_ARGUMENT
            )

        new_message = await self.insert_message(
            parent_id=message.parent_id,
            source=source,
            role=role,
            content=content,
        )
        for child_message in await message.get_children():
            await child_message.change_parent(
                new_message.id, changed_at=new_message.created_at
            )

        await self.delete_message(message)

        return new_message

    async def delete_message(self, message: Message) -> None:
        if message.parent_id is None:
            raise Result.error(  # type: ignore[misc]
                "Cannot delete root message", error_code=Error.Code.INVALID_ARGUMENT
            )

        changed_at = now_utc()
        await asyncio.gather(
            *[
                child_message.change_parent(message.parent_id, changed_at=changed_at)
                for child_message in await message.get_children()
            ]
        )

        soft_deleted_message_doc = await message.to_document().soft_delete()
        message.deleted_at = soft_deleted_message_doc.deleted_at

        if self.current_message.id == message.id:
            next_current_message = self.all_messages.get(message.parent_id)
            assert (
                next_current_message
            ), f"Message no longer exists: {message.parent_id}"

            self.current_message = next_current_message
            await ConversationDocument.partial_update(
                id=self.id,
                current_message_id=self.current_message.id,
            )

    def get_current_history(self) -> list[Message]:
        return self.get_message_history(self.current_message)

    def get_message_history(
        self,
        message: Message,
    ) -> list[Message]:
        messages = [message]
        while True:
            parent_id = messages[0].parent_id
            if parent_id is None:
                break

            message_parent = self.all_messages.get(parent_id)
            if message_parent is None:
                break

            messages.insert(0, message_parent)

        return messages

    async def stringify_conversation(self, *, enable_colors: bool = False) -> str:
        conversation_str = ""
        await self.sync_messages()
        for message in self.get_current_history():
            role = f"[{message.role.upper()}]"
            content = str(message.content)

            if enable_colors:
                role_color = ROLE_COLORS[message.role]
                role = colored(role, role_color)
                content = colored(content, f"light_{role_color}")

            conversation_str += f"{role}\n{content}\n\n"

        return conversation_str

    async def pretty_print_current_history(self) -> None:
        print(await self.stringify_conversation(enable_colors=True))


Message.model_rebuild()
ConversationSummary.model_rebuild()
Conversation.model_rebuild()
