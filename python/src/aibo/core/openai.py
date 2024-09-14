import os
from functools import cache
from typing import (
    TYPE_CHECKING,
    Annotated,
    Any,
    AsyncGenerator,
    Literal,
    Optional,
    TypeVar,
)
from uuid import uuid4

import numpy as np
import openai
from pydantic import BaseModel, Field

from aibo.common.chat.message_source import OpenAIModelSource, ProgrammaticSource
from aibo.common.constants import Env
from aibo.common.openai import (
    CompletionError,
    Embeddings,
    ErrorMessageChunk,
    FunctionCallStartChunk,
    OpenAIFunction,
    OpenAIMessage,
    OpenAIRole,
    StreamingMessageChunk,
    StreamingMessageResult,
    SuccessMessageChunk,
)
from aibo.common.time import now_utc
from aibo.common.utils import chunk_sequence
from aibo.core.package import Function, Package

if TYPE_CHECKING:
    from aibo.core.chat import Message

__all__ = [
    "CompletionError",
    "Embeddings",
    "ErrorMessageChunk",
    "FunctionCallStartChunk",
    "OpenAIFunction",
    "OpenAIMessage",
    "OpenAIRole",
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


async def _build_openai_args(
    *,
    stream: bool,
    source: OpenAIModelSource,
    messages: list["Message"],
    packages: Optional[list[Package]] = None,
    force_function: Optional[Function] = None,
) -> dict[str, Any]:
    packages = packages or []
    if force_function and force_function.package not in packages:
        packages.append(force_function.package)

    functions = [
        function_schema
        for package in packages
        for function_schema in package.to_openai()
    ]
    args: dict[str, Any] = dict(
        stream=stream,
        model=source.model,
        temperature=source.temperature,
        n=1,
        messages=[
            openai_message.model_dump(exclude_none=True)
            for message in messages
            if (
                openai_message := await message.to_openai(
                    openai_model=source.openai_model
                )
            )
        ],
    )

    # o1 doesn't support stream or temperature yet
    is_reasoning_model = args["model"].startswith("o1")
    if is_reasoning_model:
        args.pop("stream", None)
        args.pop("temperature", None)

    # o1 doesn't support function-calling yet
    if not is_reasoning_model:
        # API expects functions to only be defined if it's a non-empty list
        if functions:
            args["functions"] = functions
        if force_function:
            args["function_call"] = {"name": force_function.qualified_name}

    return args


async def stream_completion(
    *,
    source: OpenAIModelSource,
    messages: list["Message"],
    packages: Optional[list[Package]] = None,
    force_function: Optional[Function] = None,
) -> AsyncGenerator[StreamingMessageResult, None]:
    _verify_openai_enabled()

    try:
        completion_args = await _build_openai_args(
            stream=True,
            source=source,
            messages=messages,
            packages=packages,
            force_function=force_function,
        )
        completion_stream = await openai_client().chat.completions.create(
            **completion_args
        )

        # Hack to support o1
        if not completion_args.get("stream"):
            completion = completion_stream.choices[0]
            yield StreamingMessageChunk(text=completion.message.content)
            yield SuccessMessageChunk()
            return

        async for chunk in completion_stream:
            chunk_info = chunk.choices[0]
            is_done = chunk_info.finish_reason
            if is_done:
                break

            delta = chunk_info.delta

            if text := delta.content:
                yield StreamingMessageChunk(text=text)

            if function_call := delta.function_call:
                function_name = function_call.name
                text = function_call.arguments
                if text:
                    yield StreamingMessageChunk(text=text)
                elif function_name:
                    maybe_function_name = Package.split_openai_function_name(
                        function_name
                    )
                    if not maybe_function_name:
                        raise ValueError(
                            f"Invalid package function name: {function_name}"
                        )

                    package, function = maybe_function_name
                    yield FunctionCallStartChunk(
                        package=package,
                        function=function,
                    )
                else:
                    raise ValueError("Expected function_name or argument definition")

        yield SuccessMessageChunk()
    except openai.OpenAIError as exc:
        yield ErrorMessageChunk(
            source="api_error",
            content=CompletionError.from_openai(exc),
        )
    except Exception as exc:
        yield ErrorMessageChunk(
            source="server_error",
            content=CompletionError(
                error_type=CompletionError.ErrorType.AIBO_SERVER,
                text=str(exc),
            ),
        )


async def fetch_completion(
    *,
    source: OpenAIModelSource,
    messages: list["Message"],
    packages: Optional[list[Package]] = None,
    force_function: Optional[Function] = None,
) -> "Message":
    from aibo.core import chat

    _verify_openai_enabled()

    try:
        response = await openai_client().chat.completions.create(
            **await _build_openai_args(
                stream=False,
                source=source,
                messages=messages,
                packages=packages,
                force_function=force_function,
            )
        )
        text = response["choices"]["message"]["content"]

        content: chat.MessageContent = chat.TextMessageContent(text=text)
    except openai.OpenAIError as exc:
        content = chat.CompletionErrorContent.from_openai(exc)
    except Exception as exc:
        content = chat.CompletionErrorContent.from_error(
            CompletionError(
                error_type=CompletionError.ErrorType.AIBO_SERVER,
                text=str(exc),
            )
        )

    parent_message = messages[-1]
    return chat.Message(
        id=uuid4(),
        status=chat.Message.Status.COMPLETED,
        trace_id=parent_message.trace_id,
        conversation_id=parent_message.conversation_id,
        parent_id=parent_message.id,
        source=source,
        role=chat.MessageRole.ASSISTANT,
        contents=[
            chat.TextMessageContent(text=text),
        ],
        created_at=now_utc(),
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
