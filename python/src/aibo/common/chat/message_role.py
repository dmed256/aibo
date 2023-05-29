from enum import StrEnum

__all__ = ["MessageRole"]


class MessageRole(StrEnum):
    SYSTEM = "system"
    USER = "user"
    ASSISTANT = "assistant"
    FUNCTION = "function"
    ERROR = "error"
