import datetime as dt
import functools
import re
from typing import Any, Iterable, Optional, Self, Type
from uuid import UUID, uuid4

import openai
import pymongo
import tenacity
from pydantic import BaseModel, Field
from termcolor import colored

from aibo.common.constants import NULL_UUID, Env
from aibo.common.openai import OpenAIMessage
from aibo.common.result import Error, Result
from aibo.common.time import now_utc
from aibo.common.types import StrEnum
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
    "Message",
    "CompletionErrorContent",
    "ConversationSummary",
    "Conversation",
    "ConversationDocument",
    "ToolRequestContent",
    "ToolResponseContent",
    "ToolResponseErrorType",
    "ToolResponseStatus",
    "HumanSource",
    "MessageContent",
    "MessageDocument",
    "MessageEdgeDocument",
    "MessageRole",
    "MessageSource",
    "OpenAIModelSource",
    "ProgrammaticSource",
    "TextMessageContent",
]


ROLE_COLORS = {
    MessageRole.SYSTEM: "green",
    MessageRole.USER: "cyan",
    MessageRole.ASSISTANT: "yellow",
    MessageRole.ERROR: "light_gray",
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
    history: list["Message"] = Field([], repr=False)

    @classmethod
    def from_document(cls, doc: MessageDocument, *, history: list[Self]) -> Self:
        return Message(
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
    def source_text(self):
        return str(self.source)

    @property
    def content_text(self):
        return str(self.content)

    def to_openai(self, *, conversation: "Conversation") -> Optional[OpenAIMessage]:
        content = self.content.to_openai(conversation=conversation)
        if content is None:
            return None

        openai_role = {
            MessageRole.SYSTEM: "system",
            MessageRole.USER: "user",
            MessageRole.ASSISTANT: "assistant",
            MessageRole.FUNCTION: "function",
        }[self.role]

        message = OpenAIMessage(
            role=openai_role,
            content=content,
        )
        if isinstance(self.content, ToolResponseContent):
            message.name = self.content.tool
            message.function_call = self.content.response

        return message

    def get_children(self):
        child_history = [*self.history, self]
        return [
            Message.from_document(doc, history=child_history)
            for doc in MessageDocument.get_message_children(
                conversation_id=self.conversation_id,
                parent_id=self.id,
            )
        ]

    def get_conversation(self) -> "Conversation":
        return Conversation.get(self.conversation_id)


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

    @classmethod
    def search(
        cls,
        *,
        after_date: Optional[dt.datetime],
        before_date: Optional[dt.datetime],
        query: Optional[str],
    ) -> list[Self]:
        filters = {}

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

        conversation_docs = ConversationDocument.safe_find(**filters)
        return [
            cls.from_document(conversation_doc)
            for conversation_doc in conversation_docs
        ]

    def set_title(self, title: str):
        self.title = title
        ConversationDocument.partial_update(
            id=self.id,
            title=title,
        )


class Conversation(ConversationSummary):
    root_message: Message
    current_message: Message
    all_messages: dict[UUID, Message]

    @classmethod
    def create(
        cls,
        *,
        openai_model_source: OpenAIModelSource,
        enabled_tools: list[Any],  # Replace Any with tool
        system_message_inputs: CreateMessageInputs,
        title: Optional[str] = None,
    ) -> Self:
        conversation_doc = ConversationDocument(
            title=title or "New chat",
            openai_model_source=openai_model_source,
            enabled_tool_names=[tool.name for tool in enabled_tools],
            root_message_id=NULL_UUID,
            current_message_id=NULL_UUID,
            origin_message_id=None,
        ).insert()

        system_source = HumanSource(user="dmed")
        system_content = system_message_inputs.content
        system_message_doc = MessageDocument(
            conversation_id=conversation_doc.id,
            parent_id=None,
            source=system_source,
            source_text=str(system_source),
            role=system_message_inputs.role,
            content=system_content,
            content_text=str(system_content),
        ).insert()

        conversation_doc = ConversationDocument.partial_update(
            id=conversation_doc.id,
            root_message_id=system_message_doc.id,
            current_message_id=system_message_doc.id,
        )

        return cls.from_document(conversation_doc)

    def soft_delete(self, *, soft_delete_messages: bool = False):
        if soft_delete_messages:
            MessageDocument.collection.update_many(
                {"conversation_id": self.id},
                {
                    "$set": {
                        "deleted_at": now_utc(),
                    },
                },
            )

        conversation_doc = ConversationDocument.by_id(self.id)
        if conversation_doc:
            conversation_doc.soft_delete()

    @classmethod
    def from_document(cls, doc: ConversationDocument) -> Self:
        root_message = Message.from_document(
            MessageDocument.by_id(doc.root_message_id),
            history=[],
        )
        current_message = Message.from_document(
            MessageDocument.by_id(doc.current_message_id),
            history=[],
        )

        conversation = cls(
            **dict(ConversationSummary.from_document(doc)),
            root_message=root_message,
            current_message=current_message,
            all_messages=[],
        )
        conversation.sync_messages()

        return conversation

    @classmethod
    def get(cls, id: UUID) -> Optional[Self]:
        conversation_doc = ConversationDocument.by_id(id)
        if not conversation_doc:
            return None

        return cls.from_document(conversation_doc)

    def sync_messages(self):
        self.all_messages = {
            doc.id: Message.from_document(doc, history=[])
            for doc in MessageDocument.get_conversation_messages(self.id)
        }
        for message in self.all_messages.values():
            self._set_message_history(message)

    def get_message_edges(self, *, include_deletions: bool = False):
        if include_deletions:
            return MessageEdgeDocument.find({"conversation_id": self.id})

        latest_edges = MessageEdgeDocument.aggregate(
            [
                {"$filter": {"cond": {"conversation_id": self.id}}},
                {"$sort": {"child_id": pymongo.DESC, "created_at": pymongo.DESC}},
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

    def _set_message_history(self, message: Message):
        # No-op if message is root or already has history
        if not message.parent_id or message.history:
            return

        parent_message = self.all_messages[message.parent_id]
        self._set_message_history(parent_message)
        message.history = [*parent_message.history, parent_message]

    def insert_message(
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
        message_doc = MessageDocument(
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
            ConversationDocument.partial_update(
                id=self.id,
                current_message_id=self.current_message.id,
            )
        elif set_to_current_message:
            self.current_message = inserted_message

        return inserted_message

    def insert_user_message(self, text: str) -> Message:
        env = Env.get()
        return self.insert_message(
            source=HumanSource(user=env.CURRENT_USER),
            role=MessageRole.USER,
            content=TextMessageContent(text=text),
        )

    @tenacity.retry(
        wait=tenacity.wait_random_exponential(min=1, max=5),
        stop=tenacity.stop_after_attempt(3),
    )
    async def generate_assistant_message(self) -> Message:
        source = self.openai_model_source.copy()
        functions = [
            function.dict(exclude_none=True)
            for tool in self.enabled_tools
            for function in tool.to_openai()
        ]

        try:
            response = await openai.ChatCompletion.acreate(
                model=source.model,
                n=1,
                temperature=source.temperature,
                max_tokens=source.max_tokens,
                messages=[
                    openai_message.dict(exclude_none=True)
                    for message in self.get_current_history()
                    if (openai_message := message.to_openai(conversation=self))
                ],
                # API expects functions to only be defined if it's a non-empty list
                **(dict(functions=functions) if functions else {}),
            )
        except openai.OpenAIError as exc:
            return self.insert_message(
                source=ProgrammaticSource(source="api_error"),
                role=MessageRole.ERROR,
                content=CompletionErrorContent.from_openai(exc),
            )
        except Exception as exc:
            return self.insert_message(
                source=ProgrammaticSource(source="server_error"),
                role=MessageRole.ERROR,
                content=CompletionErrorContent(
                    error_type=CompletionErrorContent.ErrorType.AIBO_SERVER,
                    text=str(exc),
                ),
            )

        content = response.choices[0]["message"]["content"]
        return self.insert_message(
            source=source,
            role=MessageRole.ASSISTANT,
            content=TextMessageContent(text=content),
        )

    async def stream_assistant_message(self):
        source = self.openai_model_source.copy()
        functions = [
            function.dict(exclude_none=True)
            for tool in self.enabled_tools
            for function in tool.to_openai()
        ]

        try:
            response = await openai.ChatCompletion.acreate(
                model=source.model,
                stream=True,
                n=1,
                temperature=source.temperature,
                max_tokens=source.max_tokens,
                messages=[
                    openai_message.dict(exclude_none=True)
                    for message in self.get_current_history()
                    if (openai_message := message.to_openai(conversation=self))
                ],
                # API expects functions to only be defined if it's a non-empty list
                **(dict(functions=functions) if functions else {}),
            )

            temp_id = uuid4()
            temp_created_at = now_utc()
            content = ""
            async for chunk in response:
                chunk_info = chunk["choices"][0]
                is_done = chunk_info["finish_reason"]
                if is_done:
                    break

                delta = chunk_info["delta"].get("content")
                if delta is None:
                    break

                content += delta
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

            yield self.insert_message(
                source=source,
                role=MessageRole.ASSISTANT,
                content=TextMessageContent(text=content),
            )
        except openai.OpenAIError as exc:
            yield self.insert_message(
                source=ProgrammaticSource(source="api_error"),
                role=MessageRole.ERROR,
                content=CompletionErrorContent.from_openai(exc),
            )
        except Exception as exc:
            yield self.insert_message(
                source=ProgrammaticSource(source="server_error"),
                role=MessageRole.ERROR,
                content=CompletionErrorContent(
                    error_type=CompletionErrorContent.ErrorType.AIBO_SERVER,
                    text=str(exc),
                ),
            )

    async def generate_title(self):
        env = Env.get()

        title_conversation = Conversation.create(
            title=f"Title for {self.id}",
            openai_model_source=OpenAIModelSource.build(),
            enabled_tools=[],
            system_message_inputs=CreateMessageInputs(
                role=MessageRole.SYSTEM,
                content=TextMessageContent(
                    text="You are a helpful AI that follows instructions as precise as possible, replying in a concise and succinct manner.",
                ),
            ),
        )

        title_conversation.insert_message(
            source=HumanSource(user=env.CURRENT_USER),
            role=MessageRole.USER,
            content=TextMessageContent(
                text=f"""
--------------------
{self.stringify_conversation()}
--------------------

Capture the intent of the above within three to six words and 2 tags without newlines

Example of good summaries include:
- Docstrings for generate_title #docstring #python
- Capital of Thailand #geography #thailand
- Pokemon guessing game #fun #pokemon

Remember, only use less than ten words in total!
""".strip(),
            ),
        )
        generated_message = await title_conversation.generate_assistant_message()
        title_conversation.soft_delete()

        self.set_title(str(generated_message.content))

    def edit_message(
        message: Message,
        *,
        source: MessageSource,
        role: MessageRole,
        content: MessageContent,
    ) -> Message:
        if message.parent_id is None:
            raise Error(
                "Cannot edit root message", error_code=Error.Code.INVALID_ARGUMENT
            )

        new_message = self.insert_message(
            parent_id=message.parent_id,
            source=source,
            role=role,
            content=content,
        )
        for child_message in message.get_children():
            child_message.change_parent(
                new_message.id, changed_at=new_message.created_at
            )

        self.delete_message(message)

        return new_message

    def delete_message(self, message: Message):
        if message.parent_id is None:
            raise Error(
                "Cannot delete root message", error_code=Error.Code.INVALID_ARGUMENT
            )

        changed_at = now_utc()
        for child_message in message.get_children():
            child_message.change_parent(message.parent_id, changed_at=changed_at)

        soft_deleted_message_doc = message.to_document().soft_delete()
        message.deleted_at = soft_deleted_message_doc.deleted_at

        if self.current_message.id == message.id:
            self.current_message = self.all_messages.get(message.parent_id)
            ConversationDocument.partial_update(
                id=self.id,
                current_message_id=self.current_message.id,
            )

    def get_current_history(self, *, sync: bool = False) -> list[Message]:
        return self.get_message_history(self.current_message, sync=sync)

    def get_message_history(
        self, message: Message, *, sync: bool = False
    ) -> list[Message]:
        if sync:
            self.sync_messages()

        messages = [message]
        while True:
            message_parent = self.all_messages.get(messages[0].parent_id)
            if not message_parent:
                break
            messages.insert(0, message_parent)

        return messages

    def stringify_conversation(self, *, colored=False) -> str:
        conversation_str = ""
        for message in self.get_current_history(sync=True):
            role = f"[{message.role.upper()}]"
            content = str(message.content)

            if colored:
                role_color = ROLE_COLORS[message.role]
                role = colored(role, role_color)
                content = colored(content, f"light_{role_color}")

            conversation_str += f"{role}\n{content}\n\n"

        return conversation_str

    def pretty_print_current_history(self):
        print(self.stringify_conversation(colored=True))


Message.update_forward_refs()
ConversationSummary.update_forward_refs()
Conversation.update_forward_refs()
