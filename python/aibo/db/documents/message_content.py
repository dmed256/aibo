import json
from typing import Annotated, Any, Literal, Optional, Union

from pydantic import BaseModel, Field

from aibo.common.types import StrEnum

__all__ = [
    "ToolRequestContent",
    "ToolResponseContent",
    "ToolResponseStatus",
    "ToolResponseErrorType",
    "TextMessageContent",
    "MessageContent",
]


class TextMessageContent(BaseModel):
    """
    Regular text content
    """

    kind: Literal["text"] = "text"
    text: str

    def to_openai(self, *, conversation: "Conversation"):
        return self.text

    def __str__(self):
        return self.text


class ToolRequestContent(BaseModel):
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

    def to_openai(self, *, conversation: "Conversation"):
        return self.json(indent=2, exclude={"kind"}, exclude_none=True)

    def __str__(self):
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


class ToolResponseContent(BaseModel):
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

    def to_openai(self, *, conversation: "Conversation"):
        return self.json(indent=2, exclude={"kind"}, exclude_none=True)

    def __str__(self):
        return self.json(indent=2, exclude={"kind"}, exclude_none=True)


MessageContent = Annotated[
    Union[
        ToolRequestContent,
        ToolResponseContent,
        TextMessageContent,
    ],
    Field(discriminator="kind"),
]
