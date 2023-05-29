from typing import Any, Literal, Optional

from pydantic import BaseModel


class OpenAIMessage(BaseModel):
    role: Literal["system", "user", "assistant", "function"]
    content: str
    name: Optional[str]
    function_call: Optional[dict[str, Any]]


class OpenAIFunction(BaseModel):
    name: str
    description: str
    parameters: dict[str, Any]
