import os
from aibo.common.constants import Env
from typing import Any, Literal, Optional
import typing
import openai

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


def get_string_embedding(content: str) -> list[float]:
    env = Env.get()

    # From docs: Replace newlines, which can negatively affect performance
    content = content.replace("\n", " ")
    response = openai.Embedding.create( # type: ignore[no-untyped-call]
        input=content,
        model=env.OPENAI_EMBEDDING_MODEL_NAME,
    )
    return typing.cast(list[float], response["data"][0]["embedding"])


def get_file_embedding(filename: str) -> list[float]:
    with open(os.path.expanduser(filename), 'r') as fd:
        # From docs: Replace newlines, which can negatively affect performance
        content = fd.read()

    return get_string_embedding(content)
