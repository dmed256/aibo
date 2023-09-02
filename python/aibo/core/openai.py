import functools
import os
import typing
from typing import (
    TYPE_CHECKING,
    Annotated,
    Any,
    AsyncGenerator,
    Literal,
    Optional,
    TypeVar,
    Union,
)

import openai
from pydantic import BaseModel, Field

from aibo.common.constants import Env
from aibo.db.documents.message_content import CompletionErrorContent
from aibo.db.documents.message_source import OpenAIModelSource, ProgrammaticSource

if TYPE_CHECKING:
    from aibo.core.chat import Message

__all__ = [
    "OpenAIRole",
    "OpenAIMessage",
    "OpenAIFunction",
    "StreamingMessageChunk",
    "SuccessMessageChunk",
    "ErrorMessageChunk",
    "StreamingMessageResult",
]

ENABLED_ENVIRONMENTS = ["dev"]

OpenAIRole = Literal["system", "user", "assistant", "function"]

T = TypeVar("T")


class OpenAIMessage(BaseModel):
    role: OpenAIRole
    content: str
    name: Optional[str] = None
    function_call: Optional[dict[str, Any]] = None


class OpenAIFunction(BaseModel):
    name: str
    description: str
    parameters: dict[str, Any]


class StreamingMessageChunk(BaseModel):
    status: Literal["streaming"] = "streaming"
    text: str


class SuccessMessageChunk(BaseModel):
    status: Literal["success"] = "success"


class ErrorMessageChunk(BaseModel):
    status: Literal["error"] = "error"
    source: ProgrammaticSource
    content: CompletionErrorContent


StreamingMessageResult = Annotated[
    Union[
        StreamingMessageChunk,
        SuccessMessageChunk,
        ErrorMessageChunk,
    ],
    Field(discriminator="status"),
]


def _verify_openai_enabled() -> None:
    env = Env.get().ENV
    if env not in ENABLED_ENVIRONMENTS:
        raise EnvironmentError(
            f"Can only run on environments {ENABLED_ENVIRONMENTS}, found: {env}"
        )


async def stream_completion(
    *,
    source: OpenAIModelSource,
    messages: list["Message"],
    tools: Optional[list[Any]] = None,
) -> AsyncGenerator[StreamingMessageResult, None]:
    _verify_openai_enabled()

    tools = tools or []
    functions = [
        function.model_dump(exclude_none=True)
        for tool in tools
        for function in tool.to_openai()
    ]

    try:
        completion_stream = await openai.ChatCompletion.acreate(  # type: ignore[no-untyped-call]
            model=source.model,
            stream=True,
            n=1,
            temperature=source.temperature,
            max_tokens=source.max_tokens,
            messages=[
                openai_message.model_dump(exclude_none=True)
                for message in messages
                if (openai_message := message.to_openai())
            ],
            # API expects functions to only be defined if it's a non-empty list
            **(dict(functions=functions) if functions else {}),
        )

        async for chunk in completion_stream:
            chunk_info = chunk["choices"][0]
            is_done = chunk_info["finish_reason"]
            if is_done:
                break

            delta = chunk_info["delta"].get("content")
            if delta is None:
                break

            yield StreamingMessageChunk(text=delta)

        yield SuccessMessageChunk()
    except openai.OpenAIError as exc:
        yield ErrorMessageChunk(
            source=ProgrammaticSource(source="api_error"),
            content=CompletionErrorContent.from_openai(exc),
        )
    except Exception as exc:
        yield ErrorMessageChunk(
            source=ProgrammaticSource(source="server_error"),
            content=CompletionErrorContent(
                error_type=CompletionErrorContent.ErrorType.AIBO_SERVER,
                text=str(exc),
            ),
        )


async def get_string_embedding(
    content: str, *, model: Optional[str] = None
) -> list[float]:
    _verify_openai_enabled()

    model = model or Env.get().OPENAI_EMBEDDING_MODEL

    # From docs: Replace newlines, which can negatively affect performance
    content = content.replace("\n", " ")
    response = await openai.Embedding.acreate(  # type: ignore[no-untyped-call]
        input=content,
        model=model,
    )
    return typing.cast(list[float], response["data"][0]["embedding"])


async def get_file_embedding(
    filename: str, *, model: Optional[str] = None
) -> list[float]:
    with open(os.path.expanduser(filename), "r") as fd:
        # From docs: Replace newlines, which can negatively affect performance
        content = fd.read()

    return await get_string_embedding(content, model=model)
