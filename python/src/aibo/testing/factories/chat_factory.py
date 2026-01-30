import datetime as dt
from typing import Any

from aibo.common.chat import (
    CompletionErrorContent,
    FunctionRequestContent,
    FunctionResponseContent,
    FunctionResponseErrorType,
    FunctionResponseStatus,
    HumanSource,
    MessageRole,
    MessageSource,
    OpenAIModelSource,
    ProgrammaticSource,
    TextMessageContent,
)
from aibo.common.openai import CompletionError
from aibo.core import chat
from aibo.testing.factories.base_factory import BaseFactory, fake

__all__ = [
    "HumanSourceFactory",
    "OpenAIModelSourceFactory",
    "ProgrammaticSourceFactory",
    "MessageSourceFactory",
    "TextMessageContentFactory",
    "CompletionErrorContentFactory",
    "FunctionRequestContentFactory",
    "FunctionResponseContentFactory",
    "CreateMessageInputsFactory",
    "StreamingMessageChunkFactory",
    "MessageFactory",
    "ConversationFactory",
]


class HumanSourceFactory(BaseFactory[HumanSource]):
    @staticmethod
    async def build(*, user: str | None = None) -> HumanSource:
        return HumanSource(
            user=user or fake.name(),
        )


class OpenAIModelSourceFactory(BaseFactory[OpenAIModelSource]):
    @staticmethod
    async def build(
        *,
        model: str | None = None,
        temperature: float | None = None,
    ) -> OpenAIModelSource:
        return OpenAIModelSource(
            model=model or "fake-model",
            temperature=temperature or 0.3,
        )


class ProgrammaticSourceFactory(BaseFactory[ProgrammaticSource]):
    @staticmethod
    async def build(*, source: str | None = None) -> ProgrammaticSource:
        return ProgrammaticSource(
            source=source or fake.word(),
        )


class MessageSourceFactory(BaseFactory[MessageSource]):
    @staticmethod
    async def build(*, role: MessageRole) -> MessageSource:
        if role == chat.MessageRole.USER:
            return await HumanSourceFactory.build()

        if role == chat.MessageRole.ASSISTANT:
            return await OpenAIModelSourceFactory.build()

        return await ProgrammaticSourceFactory.build()


class TextMessageContentFactory(BaseFactory[TextMessageContent]):
    @staticmethod
    async def build(*, text: str | None = None) -> TextMessageContent:
        return TextMessageContent(
            text=text or fake.sentence(),
        )


class CompletionErrorContentFactory(BaseFactory[CompletionErrorContent]):
    @staticmethod
    async def build(
        *,
        error_type: CompletionError.ErrorType | None = None,
        text: str | None = None,
    ) -> CompletionErrorContent:
        return CompletionErrorContent(
            error_type=error_type or fake.enum(CompletionError.ErrorType),
            text=text or fake.sentence(),
        )


class FunctionRequestContentFactory(BaseFactory[FunctionRequestContent]):
    @staticmethod
    async def build(
        *,
        item_id: str | None = None,
        tool_call_id: str | None = None,
        package: str | None = None,
        function: str | None = None,
        arguments_json: str | None = None,
    ) -> FunctionRequestContent:
        return FunctionRequestContent(
            item_id=item_id or fake.uuid4(),
            tool_call_id=tool_call_id or fake.word(),
            package=package or fake.word(),
            function=function or fake.word(),
            arguments_json=arguments_json or "{}",
        )


class FunctionResponseContentFactory(BaseFactory[FunctionResponseContent]):
    @staticmethod
    async def build(
        *,
        tool_call_id: str | None = None,
        package: str | None = None,
        function: str | None = None,
        status: FunctionResponseStatus | None = None,
        error_type: FunctionResponseErrorType | None = None,
        error_message: str | None = None,
        arguments: dict[str, Any] | None = None,
        response: dict[str, Any] | None = None,
    ) -> FunctionResponseContent:
        return FunctionResponseContent(
            tool_call_id=tool_call_id or fake.word(),
            package=package or fake.word(),
            function=function or fake.word(),
            status=status or fake.enum(FunctionResponseStatus),
            error_type=error_type or fake.enum(FunctionResponseErrorType),
            error_message=error_message or fake.sentence(),
            arguments=arguments or {},
            response=response or {},
        )


class CreateMessageInputsFactory(BaseFactory[chat.CreateMessageInputs]):
    @staticmethod
    async def build(
        *,
        role: chat.MessageRole | None = None,
        content: chat.MessageContent | None = None,
    ) -> chat.CreateMessageInputs:
        return chat.CreateMessageInputs(
            role=role or fake.random_element(elements=chat.MessageRole),
            contents=[content or await TextMessageContentFactory.build()],
        )


class StreamingMessageChunkFactory(BaseFactory[chat.StreamingMessageChunk]):
    @staticmethod
    async def build(*, text: str | None = None) -> chat.StreamingMessageChunk:
        return chat.StreamingMessageChunk(text=text or fake.sentence())


class MessageFactory(BaseFactory[chat.Message]):
    @staticmethod
    async def build(
        *,
        status: chat.Message.Status | None = None,
        conversation: chat.Conversation | None = None,
        parent: chat.Message | None = None,
        source: chat.MessageSource | None = None,
        role: chat.MessageRole | None = None,
        content: chat.MessageContent | None = None,
        created_at: dt.datetime | None = None,
        deleted_at: dt.datetime | None = None,
    ) -> chat.Message:
        from aibo.testing import factory

        conversation = conversation or await ConversationFactory.build()
        conversation_doc = conversation.get_model()

        message_doc = await factory.MessageModelFactory.create(
            conversation_doc=conversation_doc,
            parent_doc=parent and parent.get_model(),
            source=source,
            role=role,
            content=content,
            created_at=created_at,
            deleted_at=deleted_at,
        )
        return chat.Message.from_model(message_doc)


class ConversationFactory(BaseFactory[chat.Conversation]):
    @staticmethod
    async def build(
        *,
        title: str | None = None,
        openai_model_source: chat.OpenAIModelSource | None = None,
        enabled_packages: list[Any] | None = None,
        conversation_depth: int | None = None,
        created_at: dt.datetime | None = None,
        deleted_at: dt.datetime | None = None,
    ) -> chat.Conversation:
        from aibo.testing import factory

        enabled_packages = enabled_packages or []

        conversation_model = await factory.ConversationModelFactory.create(
            title=title,
            openai_model_source=openai_model_source,
            enabled_package_names=[package.name for package in enabled_packages],
            conversation_depth=conversation_depth,
            created_at=created_at,
            deleted_at=deleted_at,
        )

        conversation = await chat.Conversation.from_model(conversation_model)
        await conversation.sync_messages()
        return conversation
