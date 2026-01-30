from __future__ import annotations

import asyncio
import datetime as dt
import multiprocessing as mp
import os
import re
from concurrent.futures import ProcessPoolExecutor
from enum import StrEnum
from typing import AsyncGenerator, Generator, Self, cast
from uuid import UUID, uuid4

import openai
import sqlalchemy as sa
from pydantic import BaseModel
from termcolor import colored

from aibo.common.chat import (
    CompletionErrorContent,
    FunctionRequestContent,
    FunctionResponseContent,
    FunctionResponseErrorType,
    FunctionResponseStatus,
    HumanSource,
    ImageMessageContent,
    MessageContent,
    MessageRole,
    MessageSource,
    OpenAIModelSource,
    ProgrammaticSource,
    ReasoningContent,
    TextMessageContent,
    stringify_message_contents,
)
from aibo.common.constants import Env
from aibo.common.openai import is_reasoning_model
from aibo.common.result import Error, Result
from aibo.core.openai import (
    ErrorMessageChunk,
    FunctionCallChunk,
    StreamingMessageChunk,
    StreamingMessageResult,
    StreamingReasoningChunk,
    SuccessMessageChunk,
    stream_completion,
)
from aibo.core.package import Function, FunctionContext, Package
from aibo.db.client import get_session
from aibo.db.models import ConversationModel, MessageModel

__all__ = [
    "CompletionErrorContent",
    "Conversation",
    "ConversationModel",
    "ConversationSummary",
    "CreateMessageInputs",
    "ErrorMessageChunk",
    "HumanSource",
    "Message",
    "MessageContent",
    "MessageModel",
    "MessageRole",
    "MessageSource",
    "OpenAIModelSource",
    "ProgrammaticSource",
    "StreamingMessageChunk",
    "StreamingMessageResult",
    "StreamingReasoningChunk",
    "SuccessMessageChunk",
    "TextMessageContent",
    "ImageMessageContent",
    "FunctionRequestContent",
    "FunctionResponseContent",
    "FunctionResponseErrorType",
    "FunctionResponseStatus",
]


ROLE_COLORS: dict[MessageRole, str] = {
    MessageRole.SYSTEM: "green",
    MessageRole.USER: "yellow",
    MessageRole.ASSISTANT: "cyan",
    MessageRole.TOOL: "magenta",
    MessageRole.ERROR: "red",
}

OPENAI_ROLES: dict[MessageRole, str] = {
    MessageRole.SYSTEM: "system",
    MessageRole.USER: "user",
    MessageRole.ASSISTANT: "assistant",
    MessageRole.TOOL: "tool",
}


class CreateMessageInputs(BaseModel):
    role: MessageRole
    contents: list[MessageContent]


class Message(BaseModel):
    class Status(StrEnum):
        STREAMING = "streaming"
        COMPLETED = "completed"

    id: UUID
    status: Status
    trace_id: UUID
    conversation_id: UUID
    parent_id: UUID | None = None
    original_parent_id: UUID | None = None
    source: MessageSource
    role: MessageRole
    contents: list[MessageContent]
    created_at: dt.datetime
    deleted_at: dt.datetime | None = None

    @classmethod
    def from_model(cls, model: MessageModel) -> "Message":
        return cls(
            id=model.id,
            status=cls.Status.COMPLETED,
            trace_id=model.trace_id,
            conversation_id=model.conversation_id,
            parent_id=model.parent_id,
            original_parent_id=model.original_parent_id,
            source=model.source,
            role=model.role,
            contents=model.contents,
            created_at=model.created_at,
            deleted_at=model.deleted_at,
        )

    async def get_model(self) -> MessageModel:
        return cast(MessageModel, await MessageModel.by_id(self.id))

    def to_model(self) -> MessageModel:
        return MessageModel(
            id=self.id,
            trace_id=self.trace_id,
            conversation_id=self.conversation_id,
            parent_id=self.parent_id,
            original_parent_id=self.original_parent_id,
            source=self.source,
            source_text=self.source_text,
            role=self.role,
            contents=self.contents,
            content_text=self.content_text,
            created_at=self.created_at,
            deleted_at=self.deleted_at,
        )

    @property
    def source_text(self) -> str:
        return str(self.source)

    @property
    def content_text(self) -> str:
        return stringify_message_contents(self.contents)

    @property
    def is_reasoning_model(self) -> bool:
        if isinstance(self.source, OpenAIModelSource):
            return is_reasoning_model(self.source.model)
        return False

    async def get_openai_contents(
        self,
    ) -> list[openai.reasoning.types.ResponseInputItemParam]:
        maybe_openai_contents = await asyncio.gather(
            *[
                content.to_openai(role=self.role)
                for content in self.contents
                if not isinstance(
                    content,
                    (
                        CompletionErrorContent,
                        FunctionRequestContent,
                        FunctionResponseContent,
                    ),
                )
            ]
        )

        return [content for content in maybe_openai_contents if content is not None]

    async def to_openai(self) -> openai.types.responses.ResponseInputItemParam | None:
        if not self.contents:
            return None

        # Check if the contents themselves are inputs (e.g. messages)
        if (
            openai_input := await self.contents[0].to_openai_input(role=self.role)
        ) is not None:
            return openai_input

        # Get the actual contents (not inputs / messages)
        if not (openai_contents := await self.get_openai_contents()):
            return None

        # Handle system messages a bit different for reasoning models
        if self.role == MessageRole.SYSTEM and self.is_reasoning_model:
            return openai.types.responses.EasyInputMessageParam(
                type="message",
                role="user",
                content=[
                    openai.types.responses.ResponseInputTextParam(
                        type="input_text", text="# Custom instructions \n"
                    ),
                    *openai_contents,
                ],
            )

        return openai.types.responses.EasyInputMessageParam(
            type="message",
            role=self.role,
            content=openai_contents,
        )

    async def get_children(self) -> list["Message"]:
        return [
            self.from_model(model)
            for model in await MessageModel.get_message_children(self.id)
        ]

    async def get_conversation(self) -> Conversation:
        conversation = await Conversation.get(self.conversation_id)
        assert conversation, f"Conversation no longer exists: {self.conversation_id}"

        return conversation

    async def change_parent(self, parent_id: UUID) -> Self:
        self.parent_id = parent_id
        async with get_session() as session:
            await session.execute(
                sa.update(MessageModel)
                .where(MessageModel.id == self.id)
                .values(parent_id=parent_id)
            )
            await session.commit()

        return self


class ConversationSummary(BaseModel):
    id: UUID
    trace_id: UUID
    title: str

    # Model
    openai_model_source: OpenAIModelSource
    packages: list[Package]

    # Nested info
    conversation_depth: int

    created_at: dt.datetime

    @classmethod
    def from_model(cls, model: ConversationModel) -> Self:
        return cls(
            id=model.id,
            trace_id=model.trace_id,
            title=model.title,
            openai_model_source=model.openai_model_source,
            packages=[
                package
                for package_name in model.enabled_package_names
                if (package := Package.get(package_name))
            ],
            conversation_depth=model.conversation_depth,
            created_at=model.created_at,
        )

    async def get_model(self) -> ConversationModel:
        return cast(ConversationModel, await ConversationModel.by_id(self.id))

    @classmethod
    async def search(
        cls,
        *,
        after_date: dt.datetime | None = None,
        before_date: dt.datetime | None = None,
        query: str | None = None,
        limit: int | None = None,
        show_deleted: bool = False,
    ) -> list[Self]:
        db_query = sa.select(ConversationModel).order_by(
            ConversationModel.created_at.desc()
        )

        if after_date:
            db_query = db_query.where(ConversationModel.created_at >= after_date)

        if before_date:
            db_query = db_query.where(ConversationModel.created_at <= before_date)

        query = query and query.strip()
        if query:
            db_query = db_query.where(ConversationModel.title.ilike(f"%{query}%"))

        if not show_deleted:
            db_query = db_query.where(ConversationModel.deleted_at == None)

        if limit is not None:
            db_query = db_query.limit(limit)

        async with get_session() as session:
            query_result = await session.execute(db_query)

        return [
            cls.from_model(conversation_model)
            for conversation_model in query_result.scalars().all()
        ]

    async def set_title(self, title: str) -> Self:
        self.title = title
        async with get_session() as session:
            await session.execute(
                sa.update(ConversationModel)
                .where(ConversationModel.id == self.id)
                .values(title=title)
            )
            await session.commit()

        return self

    async def set_enabled_packages(self, package_names: list[str]) -> Self:
        self.packages = [
            package
            for package_name in set(package_names)
            if (package := Package.get(package_name))
        ]
        enabled_package_names = [package.name for package in self.packages]
        async with get_session() as session:
            await session.execute(
                sa.update(ConversationModel)
                .where(ConversationModel.id == self.id)
                .values(enabled_package_names=enabled_package_names)
            )
            await session.commit()

        return self


class Conversation(ConversationSummary):
    cwd: str | None
    root_message: Message
    current_message: Message
    all_messages: dict[UUID, Message]

    @staticmethod
    def default_title() -> str:
        return "New chat"

    @classmethod
    async def create(
        cls,
        *,
        openai_model_source: OpenAIModelSource,
        enabled_package_names: list[str] | None = None,
        system_message_inputs: CreateMessageInputs,
        trace_id: UUID | None = None,
        title: str | None = None,
        cwd: str | None = None,
    ) -> Self:
        trace_id = trace_id or uuid4()
        enabled_package_names = enabled_package_names or []

        conversation_model = await ConversationModel(
            trace_id=trace_id,
            title=title or cls.default_title(),
            openai_model_source=openai_model_source,
            enabled_package_names=list(
                {
                    package_name
                    for package_name in enabled_package_names
                    if Package.get(package_name)
                }
            ),
            origin_message_id=None,
            cwd=cwd,
        ).insert()

        system_source = HumanSource(user="dmed")
        system_contents = system_message_inputs.contents
        system_message_model = await MessageModel(
            conversation_id=conversation_model.id,
            trace_id=trace_id,
            parent_id=None,
            source=system_source,
            source_text=str(system_source),
            role=system_message_inputs.role,
            contents=system_contents,
            content_text=stringify_message_contents(system_contents),
        ).insert()

        conversation_model.root_message_id = system_message_model.id
        conversation_model.current_message_id = system_message_model.id

        async with get_session() as session:
            await session.execute(
                sa.update(ConversationModel)
                .where(ConversationModel.id == conversation_model.id)
                .values(
                    root_message_id=system_message_model.id,
                    current_message_id=system_message_model.id,
                )
            )
            await session.commit()

        return await cls.from_model(conversation_model)

    async def soft_delete(self, *, soft_delete_messages: bool = False) -> None:
        conversation_model = await self.get_model()
        await conversation_model.soft_delete()

    @classmethod
    async def from_model(cls, model: ConversationModel) -> Self:  # type: ignore[override]
        if not model.root_message_id or not model.current_message_id:
            raise Result.error(  # type: ignore[misc]
                "Invalid conversation",
                error_code=Error.Code.INVALID_ARGUMENT,
            )

        root_message_model = await MessageModel.by_id(model.root_message_id)
        current_message_model = await MessageModel.by_id(model.current_message_id)
        assert root_message_model, f"Message no longer exists: {model.root_message_id}"
        assert (
            current_message_model
        ), f"Message no longer exists: {model.current_message_id}"

        root_message = Message.from_model(
            root_message_model,
        )
        current_message = Message.from_model(
            current_message_model,
        )

        conversation = cls(
            **dict(ConversationSummary.from_model(model)),
            cwd=model.cwd,
            root_message=root_message,
            current_message=current_message,
            all_messages={},
        )
        await conversation.sync_messages()

        return conversation

    @classmethod
    async def get(cls, id: UUID) -> Self | None:
        conversation_model = await ConversationModel.by_id(id)
        if not conversation_model:
            return None

        return await cls.from_model(conversation_model)

    async def sync_messages(self) -> None:
        self.all_messages = {
            model.id: Message.from_model(model)
            for model in await ConversationModel.get_uncached_messages(self.id)
        }

    async def set_current_message(self, current_message: Message) -> None:
        self.current_message = current_message
        async with get_session() as session:
            await session.execute(
                sa.update(ConversationModel)
                .where(ConversationModel.id == self.id)
                .values(
                    current_message_id=current_message.id,
                )
            )
            await session.commit()

    async def insert_message(
        self,
        *,
        source: MessageSource,
        role: MessageRole,
        contents: list[MessageContent],
        parent_id: UUID | None = None,
        set_to_current_message: bool = False,
    ) -> Message:
        parent_id = parent_id or self.current_message.id
        updates_current_message = parent_id == self.current_message.id
        message_model = await MessageModel(
            conversation_id=self.id,
            trace_id=self.trace_id,
            parent_id=parent_id,
            source=source,
            source_text=str(source),
            role=role,
            contents=contents,
            content_text=stringify_message_contents(contents),
        ).insert()

        inserted_message = Message.from_model(
            message_model,
        )
        self.all_messages[inserted_message.id] = inserted_message

        if updates_current_message:
            await self.set_current_message(inserted_message)

        elif set_to_current_message:
            self.current_message = inserted_message

        return inserted_message

    async def insert_user_message(self, text: str) -> Message:
        env = Env.get()
        return await self.insert_message(
            source=HumanSource(user=env.CURRENT_USER),
            role=MessageRole.USER,
            contents=[TextMessageContent(text=text)],
        )

    async def generate_assistant_message(self) -> list[Message]:
        async for messages in self.stream_assistant_messages():
            pass

        return messages

    async def delete_last_assistant_message(self) -> int:
        delete_count = 0
        for message in reversed(self.get_current_history()):
            if message.role not in [MessageRole.ASSISTANT, MessageRole.TOOL]:
                break

            await self.delete_message(message)
            delete_count += 1

        return delete_count

    async def regenerate_last_assistant_message(self) -> list[Message]:
        for message in reversed(self.get_current_history()):
            if message.role not in [MessageRole.ASSISTANT, MessageRole.TOOL]:
                break

            await self.delete_message(message)

        return await self.generate_assistant_message()

    async def stream_assistant_messages(
        self, *, source: OpenAIModelSource | None = None
    ) -> AsyncGenerator[list[Message], None]:
        source = source or self.openai_model_source.copy()

        # We sample again if we see a tool response
        should_sample_again = True
        messages: list[Message] = []

        while should_sample_again:
            should_sample_again = False

            # TODO: Add streaming event mapping support, assumes `StreamingMessageChunk`
            #       is the full text
            async for chunk in self.stream_assistant_message_chunks(source=source):
                if isinstance(chunk, StreamingMessageChunk):
                    messages.append(
                        await self.insert_message(
                            source=source,
                            role=MessageRole.ASSISTANT,
                            contents=[
                                TextMessageContent(
                                    item_id=chunk.item_id,
                                    text=chunk.text,
                                ),
                            ],
                        )
                    )
                elif isinstance(chunk, FunctionCallChunk):
                    function_request_content = FunctionRequestContent(
                        item_id=chunk.item_id,
                        tool_call_id=chunk.tool_call_id,
                        package=chunk.package,
                        function=chunk.function,
                        arguments_json=chunk.arguments_json,
                    )
                    messages.append(
                        await self.insert_message(
                            source=source,
                            role=MessageRole.ASSISTANT,
                            contents=[function_request_content],
                        )
                    )
                    if (package := Package.get(chunk.package)) and (
                        function := package.functions.get(chunk.function)
                    ):
                        should_sample_again = True
                        yield messages

                        conversation_model = await ConversationModel.by_id(self.id)

                        message, new_cwd = await self.call_function(
                            conversation=self,
                            function=function,
                            item_id=function_request_content.item_id,
                            tool_call_id=function_request_content.tool_call_id,
                            arguments=function_request_content.arguments,
                            conversation_cwd=conversation_model.cwd,
                        )
                        messages.append(message)
                        # Update cwd if it changed
                        if new_cwd:
                            await conversation_model.set_cwd(new_cwd)

                elif isinstance(chunk, StreamingReasoningChunk):
                    messages.append(
                        await self.insert_message(
                            source=source,
                            role=MessageRole.ASSISTANT,
                            contents=[
                                ReasoningContent(
                                    item_id=chunk.item_id,
                                    summaries=chunk.summaries,
                                    encrypted_reasoning=chunk.encrypted_reasoning,
                                )
                            ],
                        )
                    )
                elif isinstance(chunk, ErrorMessageChunk):
                    messages.append(
                        await self.insert_message(
                            source=ProgrammaticSource(source=chunk.source),
                            role=MessageRole.ERROR,
                            contents=[CompletionErrorContent.from_error(chunk.content)],
                        )
                    )

                yield messages

    async def stream_assistant_message_chunks(
        self,
        *,
        source: OpenAIModelSource | None = None,
    ) -> AsyncGenerator[StreamingMessageResult, None]:
        source = source or self.openai_model_source.copy()
        conversation_model = await self.get_model()
        async for chunk in stream_completion(
            source=source,
            messages=self.get_current_history(),
            packages=self.packages,
            cwd=conversation_model.cwd,
        ):
            yield chunk

    async def maybe_override_openai_model_source(
        self,
        *,
        model: str,
        temperature: float | None = None,
    ) -> OpenAIModelSource:
        if model == self.openai_model_source.model:
            return self.openai_model_source

        source = OpenAIModelSource.build(
            model=model,
            temperature=temperature,
        )
        await self.set_openai_model_source(source)
        return source

    async def set_openai_model_source(
        self,
        source: OpenAIModelSource,
    ) -> None:
        async with get_session() as session:
            await session.execute(
                sa.update(ConversationModel)
                .where(ConversationModel.id == self.id)
                .values(openai_model_source=source)
            )
            await session.commit()

    async def generate_title(self, *, model: str | None = None) -> None:
        env = Env.get()
        title_model = model if model is not None else env.OPENAI_TITLE_MODEL

        # Get the cheapest model to do the title generation
        title_conversation = await Conversation.create(
            title=f"Title for {self.id}",
            trace_id=self.trace_id,
            openai_model_source=OpenAIModelSource.build(model=title_model),
            system_message_inputs=CreateMessageInputs(
                role=MessageRole.SYSTEM,
                contents=[
                    TextMessageContent(
                        text="You are a helpful AI that follows instructions as precise as possible, replying in a concise and succinct manner.",
                    ),
                ],
            ),
        )

        conversation_text = await self.stringify_conversation()
        if len(conversation_text) > 2000:
            conversation_text = (
                conversation_text[:1000] + "..." + conversation_text[-1000:]
            )

        user_content = TextMessageContent(
            text=f"""
--------------------
{conversation_text}
--------------------

Create a small 3-6 word tweet that captures the intent of the above within three to six words and 2 lowercase twitter-like tags. Do not use newlines or quotes. Here are some examples of good correct tweets:
- Modelstrings for generate_title #modelstring #python
- Capital of Thailand #geography #thailand
- Pokemon guessing game #fun #pokemon
""".strip(),
        )
        await title_conversation.insert_message(
            source=HumanSource(user=env.CURRENT_USER),
            role=MessageRole.USER,
            contents=[user_content],
        )
        *_, generated_message = await title_conversation.generate_assistant_message()
        await title_conversation.soft_delete()

        title = str(generated_message.contents[0])

        # Combine whitespace
        title = re.sub(r"\s+", " ", title)
        # Remove annoying wrapper comments
        title = re.sub(r"""^['"`]?""", "", title)
        title = re.sub(r"""['"`]?$""", "", title)

        await self.set_title(title)

    async def edit_message(
        self,
        message: Message,
        *,
        source: MessageSource,
        role: MessageRole,
        contents: list[MessageContent],
    ) -> Message:
        if message.parent_id is None:
            raise Result.error(  # type: ignore[misc]
                "Cannot edit root message", error_code=Error.Code.INVALID_ARGUMENT
            )

        new_message = await self.insert_message(
            parent_id=message.parent_id,
            source=source,
            role=role,
            contents=contents,
        )
        for child_message in await message.get_children():
            await child_message.change_parent(new_message.id)

        await self.delete_message(message)

        return new_message

    async def delete_message(self, message: Message) -> None:
        if message.parent_id is None:
            raise Result.error(  # type: ignore[misc]
                "Cannot delete root message", error_code=Error.Code.INVALID_ARGUMENT
            )

        await asyncio.gather(
            *[
                child_message.change_parent(message.parent_id)
                for child_message in await message.get_children()
            ]
        )

        soft_deleted_message_model = await message.to_model().soft_delete()
        message.deleted_at = soft_deleted_message_model.deleted_at

        if self.current_message.id == message.id:
            next_current_message = self.all_messages.get(message.parent_id)
            assert (
                next_current_message
            ), f"Message no longer exists: {message.parent_id}"

            await self.set_current_message(next_current_message)

    def iter_current_history(self) -> Generator[Message, None, None]:
        for message in self.get_current_history():
            yield message

    def iter_message_history(self, message: Message) -> Generator[Message, None, None]:
        for message in self.get_message_history(message):
            yield message

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
            content = message.content_text

            if enable_colors:
                role_color = ROLE_COLORS[message.role]
                role = colored(role, role_color)
                content = colored(content, f"light_{role_color}")

            conversation_str += f"{role}\n{content}\n\n"

        return conversation_str

    async def pretty_print_current_history(self) -> None:
        print(await self.stringify_conversation(enable_colors=True))

    @classmethod
    async def call_function(
        cls,
        *,
        conversation: Conversation,
        function: Function,
        item_id: str,
        tool_call_id: str,
        arguments: dict,
        conversation_cwd: str | None,
    ) -> tuple[Message, str | None]:
        """
        Call a function in a separate process to avoid cwd safety issues.
        Returns tuple of (message, new_cwd)
        """
        loop = asyncio.get_event_loop()

        # Run in process pool executor
        with ProcessPoolExecutor(max_workers=1) as executor:
            future = loop.run_in_executor(
                executor,
                cls._call_function_sync,
                conversation,
                function,
                item_id,
                tool_call_id,
                arguments,
                conversation_cwd,
            )
            response_message, conversation_cwd = await future

        await conversation.sync_messages()
        await conversation.set_current_message(response_message)
        return response_message, conversation_cwd

    @staticmethod
    def _call_function_sync(
        conversation: Conversation,
        function: Function,
        item_id: str,
        tool_call_id: str,
        arguments: dict,
        conversation_cwd: str | None,
    ):
        try:
            if conversation_cwd:
                os.chdir(conversation_cwd)

            # Call the function
            response_message = asyncio.run(
                function(
                    conversation=conversation,
                    item_id=item_id,
                    tool_call_id=tool_call_id,
                    arguments=arguments,
                )
            )
            return response_message, os.getcwd()
        except:
            import traceback

            traceback.print_exc()
            traceback.print_exception()


FunctionContext.model_rebuild()
