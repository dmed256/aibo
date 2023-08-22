import os
import typing
from typing import Any, Literal, Optional

import openai
from pydantic import BaseModel

from aibo.common.constants import Env

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


def get_string_embedding(content: str, *, model: Optional[str] = None) -> list[float]:
    model = model or Env.get().OPENAI_EMBEDDING_MODEL_NAME

    # From docs: Replace newlines, which can negatively affect performance
    content = content.replace("\n", " ")
    response = openai.Embedding.create(  # type: ignore[no-untyped-call]
        input=content,
        model=model,
    )
    return typing.cast(list[float], response["data"][0]["embedding"])


def get_file_embedding(filename: str, *, model: Optional[str] = None) -> list[float]:
    with open(os.path.expanduser(filename), "r") as fd:
        # From docs: Replace newlines, which can negatively affect performance
        content = fd.read()

    return get_string_embedding(content, model=model)
