from typing import Any, Literal, Optional

from pydantic import BaseModel

OpenAIRole = Literal["system", "user", "assistant", "function"]


class OpenAIMessage(BaseModel):
    role: OpenAIRole
    content: str
    name: Optional[str] = None
    function_call: Optional[dict[str, Any]] = None


class OpenAIFunction(BaseModel):
    name: str
    description: str
    parameters: dict[str, Any]
