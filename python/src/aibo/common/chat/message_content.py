from __future__ import annotations

import abc
import json
import typing
from enum import StrEnum
from typing import Annotated, Any, Literal, Self, Union
from uuid import UUID

import openai
from openai.types import chat as openai_chat
from openai.types.chat.chat_completion_content_part_image_param import (
    ImageURL as OpenAIImageURL,
)
from openai.types.chat.chat_completion_message_tool_call_param import (
    Function as OpenAIToolCallFunction,
)
from pydantic import BaseModel, Field, TypeAdapter

from aibo.common.constants import Env
from aibo.common.openai import CompletionError
from aibo.common.types import JsonValue

__all__ = [
    "CompletionErrorContent",
    "FunctionRequestContent",
    "FunctionResponseContent",
    "FunctionResponseStatus",
    "FunctionResponseErrorType",
    "TextMessageContent",
    "ImageMessageContent",
    "stringify_message_contents",
    "MessageContent",
    "MessageContentAdapter",
    "MessageContents",
    "MessageContentsAdapter",
]


class BaseMessageContent(BaseModel, abc.ABC):
    """
    Each message content must be derived from BaseMessageContent
    """

    # Unique to each message content type
    kind: str

    @abc.abstractmethod
    async def to_openai(self) -> openai_chat.ChatCompletionContentPartParam | None: ...

    @abc.abstractmethod
    def __str__(self) -> str: ...


class TextMessageContent(BaseMessageContent):
    """
    Regular text content
    """

    kind: Literal["text"] = "text"
    text: str

    async def to_openai(self) -> openai_chat.ChatCompletionContentPartParam | None:
        return openai_chat.ChatCompletionContentPartTextParam(
            type="text",
            text=self.text,
        )

    def __str__(self) -> str:
        return self.text


class ImageMessageContent(BaseMessageContent):
    """
    Image content
    """

    kind: Literal["image"] = "image"
    image_id: UUID

    async def to_openai(self) -> openai_chat.ChatCompletionContentPartParam | None:
        from aibo.db.models import ImageModel

        image = await ImageModel.by_id(self.image_id)
        if not image:
            return openai_chat.ChatCompletionContentPartTextParam(
                type="text",
                text="[Image missing]",
            )

        return openai_chat.ChatCompletionContentPartImageParam(
            type="image_url",
            image_url=OpenAIImageURL(
                url=f"data:image/{image.format};base64,{image.contents_b64}",
                detail=Env.get().OPENAI_IMAGE_DETAIL,
            ),
        )

    def __str__(self) -> str:
        return f"[Image:{self.image_id}]"


class CompletionErrorContent(BaseMessageContent):
    """
    Error from sampling a message completion
    """

    kind: Literal["completion_error"] = "completion_error"
    error_type: CompletionError.ErrorType
    text: str

    @classmethod
    def from_error(cls, error: CompletionError) -> Self:
        return cls(
            error_type=error.error_type,
            text=error.text,
        )

    @classmethod
    def from_openai(cls, error: openai.OpenAIError) -> Self:
        return cls.from_error(CompletionError.from_openai(error))

    async def to_openai(self) -> openai_chat.ChatCompletionContentPartParam | None:
        return None

    def __str__(self) -> str:
        return f"Error {self.error_type}: {self.text}"


class FunctionRequestContent(BaseMessageContent):
    """
    Message request to a function
    """

    kind: Literal["function_request"] = "function_request"
    tool_call_id: str = Field(
        title="tool_call_id", description="The ID to the tool call"
    )
    package: str = Field(
        title="package", description="This is the function package name"
    )
    function: str = Field(title="function", description="This is the function's name")
    arguments_json: str = Field(
        title="arguments_json", description="The JSON-formatted arguments string"
    )

    @property
    def arguments(self) -> dict[str, Any]:
        return typing.cast(dict[str, Any], json.loads(self.arguments_json))

    def get_openai_function_name(self) -> str:
        from aibo.core.package import Package

        return Package.get_openai_function_name(
            package=self.package,
            function=self.function,
        )

    def to_openai_tool_call(self) -> openai_chat.ChatCompletionMessageToolCallParam:
        return openai_chat.ChatCompletionMessageToolCallParam(
            type="function",
            id=self.tool_call_id,
            function=OpenAIToolCallFunction(
                name=self.get_openai_function_name(),
                arguments=self.arguments_json,
            ),
        )

    async def to_openai(self) -> openai_chat.ChatCompletionContentPartParam | None:
        return None

    def __str__(self) -> str:
        return f"{self.package}.{self.function}({self.arguments_json})"


class FunctionResponseStatus(StrEnum):
    SUCCESS = "success"
    ERROR = "error"


class FunctionResponseErrorType(StrEnum):
    UNKNOWN = "unknown"
    INVALID_PACKAGE_NAME = "invalid_package_name"
    INVALID_FUNCTION_NAME = "invalid_function_name"
    INVALID_REQUEST = "invalid_request"
    FUNCTION_ERROR = "function_error"


class FunctionResponseContent(BaseMessageContent):
    """
    Response from a function
    """

    kind: Literal["function_response"] = "function_response"
    tool_call_id: str
    package: str = Field(
        title="package", description="This is the function package name"
    )
    function: str = Field(title="function", description="This is the function's name")
    status: FunctionResponseStatus = Field(
        title="status",
        description='Returns if the action was successfully processed. Possible values: "success", "error"]',
    )
    error_type: FunctionResponseErrorType | None = None
    error_message: str | None = None
    arguments: dict[str, Any]
    response: JsonValue

    def get_openai_function_name(self) -> str:
        from aibo.core.package import Package

        return Package.get_openai_function_name(
            package=self.package,
            function=self.function,
        )

    async def to_openai(self) -> openai_chat.ChatCompletionContentPartParam | None:
        return openai_chat.ChatCompletionContentPartTextParam(
            type="text",
            text=json.dumps(self.response, indent=2),
        )

    def __str__(self) -> str:
        return json.dumps(self.response, indent=2)


def stringify_message_contents(contents: list[MessageContent]) -> str:
    return "\n\n".join(str(content) for content in contents)


FunctionResponseContent.model_rebuild()

MessageContent = Annotated[
    Union[
        CompletionErrorContent,
        FunctionRequestContent,
        FunctionResponseContent,
        TextMessageContent,
        ImageMessageContent,
    ],
    Field(discriminator="kind"),
]
MessageContentAdapter = TypeAdapter(MessageContent)  # type: ignore

MessageContents = list[MessageContent]
MessageContentsAdapter = TypeAdapter(MessageContents)
