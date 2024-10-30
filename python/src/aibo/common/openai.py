from __future__ import annotations

from enum import StrEnum
from functools import cache
from typing import Annotated, Any, Iterable, Literal, Self, Union

import numpy as np
import openai
from pydantic import BaseModel, ConfigDict, Field
from typing_extensions import TypedDict

__all__ = [
    "EMBEDDING_SUGGESTED_MAX_STRING_LENGTH",
    "EMBEDDING_LENGTH",
    "CompletionError",
    "Embeddings",
    "FunctionCallChunk",
    "StreamingMessageChunk",
    "SuccessMessageChunk",
    "ErrorMessageChunk",
    "is_reasoning_model",
]

EMBEDDING_SUGGESTED_MAX_STRING_LENGTH = 400
EMBEDDING_LENGTH = 1536


class Embeddings(BaseModel):
    model: str
    embeddings: list[list[float]]

    def average_embedding(self) -> Any:
        return np.mean(np.array(self.embeddings, dtype=np.float32), 0)


class FunctionCallChunk(BaseModel):
    kind: Literal["function_call"] = "function_call"
    status: Literal["streaming"] = "streaming"
    tool_call_id: str
    package: str
    function: str
    arguments_json: str


class StreamingMessageChunk(BaseModel):
    kind: Literal["streaming_message"] = "streaming_message"
    status: Literal["streaming"] = "streaming"
    text: str


class SuccessMessageChunk(BaseModel):
    kind: Literal["success_message"] = "success_message"
    status: Literal["success"] = "success"


class CompletionError(BaseModel):
    """
    Error from sampling a message completion
    """

    class ErrorType(StrEnum):
        AIBO_SERVER = "aibo_server"
        API = "api"
        AUTHENTICATION = "authentication"
        PERMISSION = "permission"
        RATE_LIMIT = "rate_limit"
        SERVICE = "service"
        TIMEOUT = "timeout"
        UNKNOWN = "UNKNOWN"

    error_type: ErrorType
    text: str

    @classmethod
    def from_openai(cls, error: openai.OpenAIError) -> Self:
        error_type = {
            openai.APIConnectionError: cls.ErrorType.SERVICE,
            openai.AuthenticationError: cls.ErrorType.AUTHENTICATION,
            openai.RateLimitError: cls.ErrorType.RATE_LIMIT,
            openai.Timeout: cls.ErrorType.TIMEOUT,
        }.get(error.__class__, cls.ErrorType.UNKNOWN)

        return cls(
            error_type=error_type,
            text=str(error),
        )

    def __str__(self) -> str:
        return f"Error {self.error_type}: {self.text}"


class ErrorMessageChunk(BaseModel):
    kind: Literal["error_message"] = "error_message"
    status: Literal["error"] = "error"
    source: str
    content: CompletionError


StreamingMessageResult = Annotated[
    Union[
        FunctionCallChunk,
        StreamingMessageChunk,
        SuccessMessageChunk,
        ErrorMessageChunk,
    ],
    Field(discriminator="kind"),
]


def is_reasoning_model(model: str) -> bool:
    return model.lower().startswith("o")
