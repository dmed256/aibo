from typing import Annotated, Literal, Optional, Union

from pydantic import BaseModel, Field

from aibo.common.constants import Env

__all__ = [
    "HumanSource",
    "MessageSource",
    "OpenAIModelSource",
    "ProgrammaticSource",
]


class HumanSource(BaseModel):
    """
    The source of the message came from a human
    """

    kind: Literal["human"] = "human"
    user: str

    def __str__(self):
        return f"human:{self.user}"


class OpenAIModelSource(BaseModel):
    """
    The source of the message came from an OpenAI model
    """

    kind: Literal["openai_model"] = "openai_model"
    model: str
    temperature: float
    max_tokens: int

    @classmethod
    def build(
        cls,
        *,
        model: Optional[str] = None,
        temperature: Optional[float] = None,
        max_tokens: Optional[int] = None,
    ):
        env = Env.get()
        return cls(
            model=model or env.OPENAI_MODEL_NAME,
            temperature=env.OPENAI_TEMPERATURE if temperature is None else temperature,
            max_tokens=max_tokens or env.OPENAI_MAX_TOKENS,
        )

    def __str__(self):
        return f"model:{self.model}"


class ProgrammaticSource(BaseModel):
    """
    There was no explicit source of the message (e.g. tool-generated)
    """

    kind: Literal["programmatic"] = "programmatic"
    source: str

    def __str__(self):
        return f"programmatic:{self.source}"


MessageSource = Annotated[
    Union[
        HumanSource,
        OpenAIModelSource,
        ProgrammaticSource,
    ],
    Field(discriminator="kind"),
]
