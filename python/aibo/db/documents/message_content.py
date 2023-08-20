from __future__ import annotations

import abc
import json
from typing import TYPE_CHECKING, Annotated, Any, Literal, Optional, Self, Union

import openai
import openai.error as oai_error
from pydantic import BaseModel, Field

from aibo.common.types import StrEnum

if TYPE_CHECKING:
    from aibo.core.chat import Conversation

__all__ = [
    "CompletionErrorContent",
    "ToolRequestContent",
    "ToolResponseContent",
    "ToolResponseStatus",
    "ToolResponseErrorType",
    "TextMessageContent",
    "MessageContent",
]


class BaseMessageContent(BaseModel, abc.ABC):
    """
    Each message content must be derived from BaseMessageContent
    """

    # Unique to each message content type
    kind: str

    @abc.abstractmethod
    def to_openai(self, *, conversation: Conversation) -> Optional[str]:
        ...

    @abc.abstractmethod
    def __str__(self) -> str:
        ...


class TextMessageContent(BaseMessageContent):
    """
    Regular text content
    """

    kind: Literal["text"] = "text"
    text: str

    def to_openai(self, *, conversation: Conversation) -> str:
        return self.text

    def __str__(self) -> str:
        return self.text


class CompletionErrorContent(BaseMessageContent):
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

    kind: Literal["completion_error"] = "completion_error"
    error_type: ErrorType
    text: str

    @classmethod
    def from_openai(cls, error: openai.OpenAIError) -> Self:
        error_type = {
            oai_error.APIConnectionError: cls.ErrorType.SERVICE,
            oai_error.AuthenticationError: cls.ErrorType.AUTHENTICATION,
            oai_error.InvalidAPIType: cls.ErrorType.API,
            oai_error.InvalidRequestError: cls.ErrorType.API,
            oai_error.PermissionError: cls.ErrorType.PERMISSION,
            oai_error.RateLimitError: cls.ErrorType.RATE_LIMIT,
            oai_error.ServiceUnavailableError: cls.ErrorType.SERVICE,
            oai_error.Timeout: cls.ErrorType.TIMEOUT,
        }.get(error.__class__, cls.ErrorType.UNKNOWN)

        return cls(
            error_type=error_type,
            text=str(error),
        )

    def to_openai(self, *, conversation: Conversation) -> None:
        return None

    def __str__(self) -> str:
        return f"Error {self.error_type}: {self.text}"


class ToolRequestContent(BaseMessageContent):
    """
    Message request to an tool. Check below for a few examples

    Get current timestamp:
        {
            "to": "clock.today",
            "request": {}
        }

    Create a reminder:
        {
            "to": "work.create_reminder",
            "request": {
                "timestamp": "2023-03-01T12:00:00",
                "reminder": "Check PR reviews",
            }
        }
    """

    kind: Literal["tool_request"] = "tool_request"
    to: str = Field(title="to", description="The <tool>.<action> recipient")
    request: dict[str, Any]

    def to_openai(self, *, conversation: Conversation) -> str:
        return self.json(indent=2, exclude={"kind"}, exclude_none=True)

    def __str__(self) -> str:
        return self.json(indent=2, exclude={"kind"}, exclude_none=True)


class ToolResponseStatus(StrEnum):
    SUCCESS = "success"
    ERROR = "error"


class ToolResponseErrorType(StrEnum):
    UNKNOWN = "unknown"
    INVALID_TOOL_NAME = "invalid_tool_name"
    INVALID_TOOL_ACTION = "invalid_tool_action"
    INVALID_REQUEST = "invalid_request"
    TOOL_ERROR = "tool_error"


class ToolResponseContent(BaseMessageContent):
    """
    Response from an tool. Check below for a few examples

    Get current timestamp:
        {
            "tool": "clock",
            "status": "success",
            "response": {
                "isodate": "2023-05-29",
                "human_date": "Monday - May 5, 2023",
            }
        }

    Create a reminder:
        {
            "tool": "work",
            "status": "error",
            "error_type": "tool_error",
            "error_message": "Reminder server is not running.",
            "response": {
                "status_code": 500,
                "error": "Reminder server is not running.",
            }
        }
    """

    kind: Literal["tool_response"] = "tool_response"
    tool: str = Field(title="tool", description="This is the tool's name")
    status: ToolResponseStatus = Field(
        title="status",
        description='Returns if the action was successfully processed. Possible values: "success", "error"]',
    )
    error_type: Optional[ToolResponseErrorType]
    error_message: Optional[str]
    response: dict[str, Any]

    def to_openai(self, *, conversation: Conversation) -> str:
        return self.json(indent=2, exclude={"kind"}, exclude_none=True)

    def __str__(self) -> str:
        return self.json(indent=2, exclude={"kind"}, exclude_none=True)


MessageContent = Annotated[
    Union[
        CompletionErrorContent,
        ToolRequestContent,
        ToolResponseContent,
        TextMessageContent,
    ],
    Field(discriminator="kind"),
]
