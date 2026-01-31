from __future__ import annotations

import traceback
from functools import cache
from typing import TYPE_CHECKING, AsyncGenerator

import openai
import openai.types.shared_params.reasoning as reasoning_params

from aibo.common.chat.message_source import OpenAIModelSource
from aibo.common.constants import Env
from aibo.common.openai import (
    CompletionError,
    ErrorMessageChunk,
    FunctionCallChunk,
    StreamingMessageChunk,
    StreamingMessageResult,
    StreamingReasoningChunk,
    SuccessMessageChunk,
)
from aibo.core.codex_exec import (
    CodexAgentMessageItem,
    CodexCommandExecutionItem,
    CodexErrorEvent,
    CodexFileChangeItem,
    CodexItemCompletedEvent,
    CodexReasoningItem,
    CodexThreadStartedEvent,
    build_codex_prompt,
    stream_codex_events,
)
from aibo.core.package import Package
from aibo.db.models import ConversationModel

if TYPE_CHECKING:
    from aibo.core.chat import Message

__all__ = [
    "CompletionError",
    "ErrorMessageChunk",
    "FunctionCallChunk",
    "StreamingMessageChunk",
    "StreamingMessageResult",
    "SuccessMessageChunk",
    "stream_completion",
]

ENABLED_ENVIRONMENTS = ["dev"]


@cache
def openai_client() -> openai.AsyncOpenAI:
    return openai.AsyncOpenAI()


async def stream_completion(
    *,
    source: OpenAIModelSource,
    messages: list[Message],
    packages: list[Package] | None = None,
    cwd: str | None = None,
) -> AsyncGenerator[StreamingMessageResult, None]:
    _verify_openai_enabled()

    try:
        if source.is_codex_model:
            async for result in _stream_codex_completion(
                model_name=source.codex_model,
                messages=messages,
                cwd=cwd,
            ):
                yield result
        else:
            async for result in _stream_responses_completion(
                source=source,
                messages=messages,
                packages=packages,
            ):
                yield result
    except openai.OpenAIError as exc:
        yield ErrorMessageChunk(
            source="api_error",
            content=CompletionError.from_openai(exc),
        )
    except Exception:
        yield ErrorMessageChunk(
            source="server_error",
            content=CompletionError(
                error_type=CompletionError.ErrorType.AIBO_SERVER,
                text=traceback.format_exc(),
            ),
        )


async def _stream_codex_completion(
    *,
    model_name: str | None,
    messages: list[Message],
    cwd: str | None,
) -> AsyncGenerator[StreamingMessageResult, None]:
    if not messages:
        return

    conversation_id = messages[-1].conversation_id
    conversation_model = await ConversationModel.by_id(conversation_id)
    if not conversation_model:
        raise ValueError(f"Conversation not found: {conversation_id}")

    prompt, image_paths = await build_codex_prompt(messages)
    try:
        async for event in stream_codex_events(
            prompt=prompt,
            model_name=model_name,
            cwd=cwd,
            image_paths=image_paths,
            session_id=conversation_model.codex_thread_id,
        ):
            if isinstance(event, CodexThreadStartedEvent):
                await conversation_model.set_codex_thread_id(event.thread_id)
            elif isinstance(event, CodexErrorEvent):
                yield ErrorMessageChunk(
                    source="server_error",
                    content=CompletionError(
                        error_type=CompletionError.ErrorType.AIBO_SERVER,
                        text=event.message,
                    ),
                )
                return
            elif isinstance(event, CodexItemCompletedEvent):
                item = event.item
                if isinstance(item, CodexAgentMessageItem):
                    yield StreamingMessageChunk(
                        item_id=item.id,
                        text=item.text,
                    )
                elif isinstance(item, CodexReasoningItem):
                    yield StreamingReasoningChunk(
                        item_id=item.id,
                        summaries=[item.text],
                        encrypted_reasoning=None,
                    )
                elif isinstance(item, CodexCommandExecutionItem):
                    yield StreamingReasoningChunk(
                        item_id=item.id,
                        summaries=[item.to_summary()],
                        encrypted_reasoning=None,
                    )
    except Exception as exception:
        error_text = f"{type(exception).__name__}: {exception}"
        yield ErrorMessageChunk(
            source="server_error",
            content=CompletionError(
                error_type=CompletionError.ErrorType.AIBO_SERVER,
                text=error_text,
            ),
        )
        return


async def _stream_responses_completion(
    *,
    source: OpenAIModelSource,
    messages: list[Message],
    packages: list[Package] | None = None,
) -> AsyncGenerator[StreamingMessageResult, None]:
    temperature: float | None = source.temperature
    reasoning: reasoning_params.Reasoning | None = None
    if source.is_reasoning_model:
        temperature = None
        # TODO: Make this configurable
        reasoning = reasoning_params.Reasoning(
            effort="high",
            summary="detailed",
        )

    response = await openai_client().responses.create(
        # TODO: Add streaming event mapping support
        stream=False,
        # store=True is required for reasoning(?) since id is required
        store=True,
        model=source.model,
        temperature=temperature,
        reasoning=reasoning,
        input=[
            openai_input
            for message in messages
            if (openai_input := await message.to_openai())
        ],
        tools=[
            tool_schema
            for package in (packages or [])
            for tool_schema in package.to_openai()
        ],
    )

    # TODO: Add streaming event mapping support
    for item in response.output:
        if isinstance(item, openai.types.responses.ResponseOutputMessage):
            yield _get_streaming_message_chunk(item)
        elif isinstance(item, openai.types.responses.ResponseFunctionToolCall):
            yield _get_function_call_chunk(item)
        elif isinstance(item, openai.types.responses.ResponseReasoningItem):
            yield StreamingReasoningChunk(
                item_id=item.id,
                summaries=[summary.text for summary in item.summary],
                encrypted_reasoning=item.encrypted_content,
            )
        else:
            yield ErrorMessageChunk(
                source="server_error",
                content=CompletionError(
                    error_type=CompletionError.ErrorType.AIBO_SERVER,
                    text=f"Unknown response type: {type(item)}",
                ),
            )


def _verify_openai_enabled() -> None:
    env = Env.get().ENV
    if env not in ENABLED_ENVIRONMENTS:
        raise EnvironmentError(
            f"Can only run on environments {ENABLED_ENVIRONMENTS}, found: {env}"
        )


def _get_streaming_message_chunk(
    output: openai.types.responses.ResponseOutputMessage,
) -> StreamingMessageChunk:
    text = ""
    for content in output.content:
        if isinstance(content, openai.types.responses.ResponseOutputText):
            text += content.text
        elif isinstance(content, openai.types.responses.ResponseOutputRefusal):
            text += f"(Refusal: {content.refusal})"

    return StreamingMessageChunk(item_id=output.id, text=text)


def _get_function_call_chunk(
    tool_call: openai.types.responses.ResponseFunctionToolCall,
) -> FunctionCallChunk:
    maybe_function_name = Package.split_openai_function_name(tool_call.name)
    if not maybe_function_name:
        raise ValueError(f"Invalid package function name: {tool_call.name}")

    package, function = maybe_function_name
    return FunctionCallChunk(
        item_id=tool_call.id,
        tool_call_id=tool_call.call_id,
        package=package,
        function=function,
        arguments_json=tool_call.arguments,
    )
