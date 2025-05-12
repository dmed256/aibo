from __future__ import annotations

import traceback
from functools import cache
from typing import TYPE_CHECKING, AsyncGenerator, Optional

import openai

from aibo.common.chat.message_source import OpenAIModelSource
from aibo.common.constants import Env
from aibo.common.openai import (
    CompletionError,
    Embeddings,
    ErrorMessageChunk,
    FunctionCallChunk,
    StreamingMessageChunk,
    StreamingMessageResult,
    StreamingReasoningChunk,
    SuccessMessageChunk,
)
from aibo.common.utils import chunk_sequence
from aibo.core.package import Package

if TYPE_CHECKING:
    from aibo.core.chat import Message

__all__ = [
    "CompletionError",
    "Embeddings",
    "ErrorMessageChunk",
    "FunctionCallChunk",
    "StreamingMessageChunk",
    "StreamingMessageResult",
    "SuccessMessageChunk",
    "get_string_embeddings",
    "stream_completion",
]

ENABLED_ENVIRONMENTS = ["dev"]
EMBEDDING_BATCH_SIZE = 100


@cache
def openai_client() -> openai.AsyncOpenAI:
    return openai.AsyncOpenAI()


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


async def stream_completion(
    *,
    source: OpenAIModelSource,
    messages: list[Message],
    packages: Optional[list[Package]] = None,
) -> AsyncGenerator[StreamingMessageResult, None]:
    _verify_openai_enabled()

    try:
        temperature = source.temperature
        reasoning: openai.types.Reasoning | None = None
        if source.is_reasoning_model:
            temperature = None
            # TODO: Make this configurable
            reasoning = openai.types.Reasoning(
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


async def get_string_embeddings(
    contents: list[str], *, model: Optional[str] = None
) -> Embeddings:
    _verify_openai_enabled()

    model = model or Env.get().OPENAI_EMBEDDING_MODEL

    all_embeddings = Embeddings(model=model, embeddings=[])
    for batch_contents in chunk_sequence(contents, EMBEDDING_BATCH_SIZE):
        batch_embeddings = await _get_string_embedding_batch(
            contents=batch_contents, model=model
        )
        all_embeddings.embeddings.extend(batch_embeddings.embeddings)

    return all_embeddings


async def _get_string_embedding_batch(
    *,
    contents: list[str],
    model: str,
) -> Embeddings:
    response = await openai_client().embeddings.create(input=contents, model=model)
    return Embeddings(
        model=model,
        embeddings=[
            embedding_response.embedding for embedding_response in response.data
        ],
    )
