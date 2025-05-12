from __future__ import annotations

import abc
import json
import typing
from enum import StrEnum
from typing import Annotated, Any, Literal, Self, Union
from uuid import UUID

import openai
from pydantic import BaseModel, Field, TypeAdapter

from aibo.common.chat.message_role import MessageRole
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
    "ReasoningContent",
]


OpenAIContent = str | openai.types.responses.ResponseInputMessageContentListParam
OpenAIInput = openai.types.responses.ResponseInputItemParam


class BaseMessageContent(BaseModel, abc.ABC):
    """
    Each message content must be derived from BaseMessageContent
    """

    # Unique to each message content type
    kind: str
    item_id: str | None = Field(
        title="item_id",
        description="The ID to the tool call 'item' (vs call itself?)",
        default=None,
    )

    @abc.abstractmethod
    async def to_openai_input(self, *, role: MessageRole) -> OpenAIInput | None: ...

    @abc.abstractmethod
    async def to_openai(self, *, role: MessageRole) -> OpenAIContent | None: ...

    @abc.abstractmethod
    def __str__(self) -> str: ...


class TextMessageContent(BaseMessageContent):
    """
    Regular text content
    """

    kind: Literal["text"] = "text"
    text: str

    async def to_openai_input(self, *, role: MessageRole) -> OpenAIInput | None:
        if role == MessageRole.ASSISTANT:
            return openai.types.responses.ResponseOutputMessage(
                type="message",
                id=self.item_id,
                role="assistant",
                status="completed",
                content=[
                    openai.types.responses.ResponseOutputText(
                        type="output_text",
                        text=self.text,
                        annotations=[],
                    ),
                ],
            )
        else:
            return None

    async def to_openai(self, *, role: MessageRole) -> OpenAIContent | None:
        if role == MessageRole.ASSISTANT:
            return None
        else:
            return openai.types.responses.ResponseInputTextParam(
                type="input_text",
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

    async def to_openai_input(self, *, role: MessageRole) -> OpenAIInput | None:
        return None

    async def to_openai(self, *, role: MessageRole) -> OpenAIContent | None:
        from aibo.db.models import ImageModel

        image = await ImageModel.by_id(self.image_id)
        if not image:
            return openai.types.responses.ResponseInputImageParam(
                detail=Env.get().OPENAI_IMAGE_DETAIL,
                image_url=f"data:image/{image.format};base64,{image.contents_b64}",
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

    async def to_openai_input(self, *, role: MessageRole) -> OpenAIInput | None:
        return None

    async def to_openai(self, *, role: MessageRole) -> OpenAIContent | None:
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

    async def to_openai_input(self, *, role: MessageRole) -> OpenAIInput | None:
        return openai.types.responses.ResponseFunctionToolCallParam(
            type="function_call",
            id=self.item_id,
            call_id=self.tool_call_id,
            status="completed",
            name=self.get_openai_function_name(),
            arguments=self.arguments_json,
        )

    async def to_openai(self, *, role: MessageRole) -> OpenAIContent | None:
        return None

    def __str__(self) -> str:
        return f"{self.package}.{self.function}({self.arguments_json})".strip()


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

    def get_response_string(self) -> str:
        if isinstance(self.response, str):
            return self.response
        return json.dumps(self.response, indent=2)

    def get_openai_function_name(self) -> str:
        from aibo.core.package import Package

        return Package.get_openai_function_name(
            package=self.package,
            function=self.function,
        )

    async def to_openai_input(self, *, role: MessageRole) -> OpenAIInput | None:
        return openai.types.responses.response_input_item_param.FunctionCallOutput(
            type="function_call_output",
            # This is for returned content like web
            # id=self.item_id,
            call_id=self.tool_call_id,
            status="completed",
            output=self.get_response_string(),
        )

    async def to_openai(self, *, role: MessageRole) -> OpenAIContent | None:
        return None

    def __str__(self) -> str:
        return self.get_response_string()


def stringify_message_contents(contents: list[MessageContent]) -> str:
    return "\n\n".join(
        [str_content for content in contents if (str_content := str(content))]
    )


class ReasoningContent(BaseMessageContent):
    """
    Message request to a function
    """

    kind: Literal["reasoning"] = "reasoning"
    item_id: str
    summaries: list[str]
    encrypted_reasoning: str | None = None

    async def to_openai_input(self, *, role: MessageRole) -> OpenAIInput | None:
        return openai.types.responses.ResponseReasoningItemParam(
            type="reasoning",
            id=self.item_id,
            summary=[
                openai.types.responses.response_reasoning_item_param.Summary(
                    type="summary_text", text=summary
                )
                for summary in self.summaries
            ],
            encrypted_content=self.encrypted_reasoning,
        )

    async def to_openai(self, *, role: MessageRole) -> OpenAIContent | None:
        return None

    def __str__(self) -> str:
        summary_str = (". ".join(summary for summary in self.summaries)).strip()
        return summary_str or "[No summaries found]"


FunctionResponseContent.model_rebuild()

MessageContent = Annotated[
    Union[
        CompletionErrorContent,
        FunctionRequestContent,
        FunctionResponseContent,
        TextMessageContent,
        ImageMessageContent,
        ReasoningContent,
    ],
    Field(discriminator="kind"),
]
MessageContentAdapter = TypeAdapter(MessageContent)

MessageContents = list[MessageContent]
MessageContentsAdapter = TypeAdapter(MessageContents)
