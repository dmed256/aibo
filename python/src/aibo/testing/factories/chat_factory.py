import datetime as dt
from typing import Any, Optional

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
)
from aibo.common.openai import CompletionError
from aibo.common.time import now_utc
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
    "ErrorMessageChunkFactory",
    "MessageFactory",
    "ConversationFactory",
]


class HumanSourceFactory(BaseFactory[HumanSource]):
    @staticmethod
    async def build(*, user: Optional[str] = None) -> HumanSource:
        return HumanSource(
            user=user or fake.name(),
        )


class OpenAIModelSourceFactory(BaseFactory[OpenAIModelSource]):
    @staticmethod
    async def build(
        *,
        model: Optional[str] = None,
        temperature: Optional[float] = None,
    ) -> OpenAIModelSource:
        return OpenAIModelSource(
            model=model or "fake-model",
            temperature=temperature or 0.3,
        )


class ProgrammaticSourceFactory(BaseFactory[ProgrammaticSource]):
    @staticmethod
    async def build(*, source: Optional[str] = None) -> ProgrammaticSource:
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
    async def build(*, text: Optional[str] = None) -> TextMessageContent:
        return TextMessageContent(
            text=text or fake.sentence(),
        )


class CompletionErrorContentFactory(BaseFactory[CompletionErrorContent]):
    @staticmethod
    async def build(
        *,
        error_type: Optional[CompletionError.ErrorType] = None,
        text: Optional[str] = None,
    ) -> CompletionErrorContent:
        return CompletionErrorContent(
            error_type=error_type or fake.enum(CompletionError.ErrorType),
            text=text or fake.sentence(),
        )


class FunctionRequestContentFactory(BaseFactory[FunctionRequestContent]):
    @staticmethod
    async def build(
        *,
        package: Optional[str] = None,
        function: Optional[str] = None,
        text: Optional[str] = None,
    ) -> FunctionRequestContent:
        return FunctionRequestContent(
            package=package or fake.word(),
            function=function or fake.word(),
            text=text or "{}",
        )


class FunctionResponseContentFactory(BaseFactory[FunctionResponseContent]):
    @staticmethod
    async def build(
        *,
        package: Optional[str] = None,
        function: Optional[str] = None,
        status: Optional[FunctionResponseStatus] = None,
        error_type: Optional[FunctionResponseErrorType] = None,
        error_message: Optional[str] = None,
        arguments: Optional[dict[str, Any]] = None,
        response: Optional[dict[str, Any]] = None,
    ) -> FunctionResponseContent:
        return FunctionResponseContent(
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
        role: Optional[chat.MessageRole] = None,
        content: Optional[chat.MessageContent] = None,
    ) -> chat.CreateMessageInputs:
        return chat.CreateMessageInputs(
            role=role or fake.random_element(elements=chat.MessageRole),
            contents=[content or await TextMessageContentFactory.build()],
        )


class StreamingMessageChunkFactory(BaseFactory[chat.StreamingMessageChunk]):
    @staticmethod
    async def build(*, text: Optional[str] = None) -> chat.StreamingMessageChunk:
        return chat.StreamingMessageChunk(text=text or fake.sentence())


class ErrorMessageChunkFactory(BaseFactory[chat.ErrorMessageChunk]):
    @staticmethod
    async def build(
        *,
        source: Optional[str] = None,
        content: Optional[chat.CompletionErrorContent] = None,
    ) -> chat.ErrorMessageChunk:
        return chat.ErrorMessageChunk(
            source=source or fake.word(),
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
        title: Optional[str] = None,
        openai_model_source: Optional[chat.OpenAIModelSource] = None,
        enabled_packages: Optional[list[Any]] = None,
        conversation_depth: Optional[int] = None,
        created_at: Optional[dt.datetime] = None,
        deleted_at: Optional[dt.datetime] = None,
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
