from typing import Annotated, Literal, Optional, Self, Union

from pydantic import BaseModel, Field, TypeAdapter

from aibo.common.constants import Env
from aibo.common.openai import is_reasoning_model

__all__ = [
    "HumanSource",
    "MessageSource",
    "OpenAIModelSource",
    "ProgrammaticSource",
    "MessageSourceAdapter",
]


class HumanSource(BaseModel):
    """
    The source of the message came from a human
    """

    kind: Literal["human"] = "human"
    user: str

    def __str__(self) -> str:
        return f"human:{self.user}"


class OpenAIModelSource(BaseModel):
    """
    The source of the message came from an OpenAI model
    """

    kind: Literal["openai_model"] = "openai_model"
    model: str
    temperature: float

    @classmethod
    def build(
        cls,
        *,
        model: Optional[str] = None,
        temperature: Optional[float] = None,
    ) -> Self:
        env = Env.get()
        return cls(
            model=model or env.OPENAI_MODEL,
            temperature=env.OPENAI_TEMPERATURE if temperature is None else temperature,
        )

    @property
    def is_reasoning_model(self) -> bool:
        return is_reasoning_model(self.model)

    @property
    def is_codex_model(self) -> bool:
        return self.model == "codex" or self.model.startswith("codex:")

    @property
    def codex_model(self) -> str | None:
        if self.model.startswith("codex:"):
            return self.model[len("codex:") :]
        return None

    def __str__(self) -> str:
        return f"model:{self.model}"


class ProgrammaticSource(BaseModel):
    """
    There was no explicit source of the message (e.g. function-generated)
    """

    kind: Literal["programmatic"] = "programmatic"
    source: str

    def __str__(self) -> str:
        return f"programmatic:{self.source}"


MessageSource = Annotated[
    Union[
        HumanSource,
        OpenAIModelSource,
        ProgrammaticSource,
    ],
    Field(discriminator="kind"),
]
MessageSourceAdapter: TypeAdapter[MessageSource] = TypeAdapter(MessageSource)
