from typing import Annotated, Literal, Optional, Self, Union

from pydantic import BaseModel, Field, TypeAdapter

from aibo.common.constants import Env
from aibo.common.openai import OPENAI_MODELS_BY_MODEL, OpenAIModel

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
    def openai_model(self) -> OpenAIModel:
        if openai_model := OPENAI_MODELS_BY_MODEL.get(self.model):
            return openai_model
        return OpenAIModel(
            name=self.model,
            model=self.model,
            model_family="gpt-4",
            modalities={"text", "image"},
        )

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
MessageSourceAdapter = TypeAdapter(MessageSource)
