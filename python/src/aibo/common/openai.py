from enum import StrEnum
from functools import cache
from typing import Annotated, Any, Iterable, Literal, Optional, Self, Union

import numpy as np
import openai
from pydantic import BaseModel, ConfigDict, Field
from typing_extensions import TypedDict

__all__ = [
    "EMBEDDING_SUGGESTED_MAX_STRING_LENGTH",
    "EMBEDDING_LENGTH",
    "CompletionError",
    "OpenAIRole",
    "OpenAIMessage",
    "OpenAIFunction",
    "Embeddings",
    "FunctionCallStartChunk",
    "StreamingMessageChunk",
    "SuccessMessageChunk",
    "ErrorMessageChunk",
    "OpenAIModelFamily",
    "Modality",
    "OpenAIModel",
    "OPENAI_MODELS_BY_NAME",
    "OPENAI_MODELS_BY_MODEL",
    "OPENAI_MODELS_BY_MODEL_FAMILY",
    "OPENAI_MODELS_BY_MODALITY",
    "get_openai_model",
]

EMBEDDING_SUGGESTED_MAX_STRING_LENGTH = 400
EMBEDDING_LENGTH = 1536

OpenAIRole = Literal["system", "user", "assistant", "function"]


class OpenAITextContent(TypedDict):
    type: Literal["text"]
    text: str


class OpenAIImageUrlField(TypedDict):
    url: str
    detail: Literal["auto", "low", "high"] | None


class OpenAIImageUrlContent(TypedDict):
    type: Literal["image_url"]
    image_url: OpenAIImageUrlField


OpenAIContent = OpenAITextContent | OpenAIImageUrlContent


class OpenAIMessage(BaseModel):
    role: OpenAIRole
    content: list[OpenAIContent]
    name: Optional[str] = None
    function_call: Optional[dict[str, Any]] = None


class OpenAIFunction(BaseModel):
    name: str
    description: str
    parameters: dict[str, Any]


class Embeddings(BaseModel):
    model: str
    embeddings: list[list[float]]

    def average_embedding(self) -> Any:
        return np.mean(np.array(self.embeddings, dtype=np.float32), 0)


class FunctionCallStartChunk(BaseModel):
    kind: Literal["function_call_start"] = "function_call_start"
    status: Literal["streaming"] = "streaming"
    package: str
    function: str


class StreamingMessageChunk(BaseModel):
    kind: Literal["streaming_message"] = "streaming_message"
    status: Literal["streaming"] = "streaming"
    text: str


class SuccessMessageChunk(BaseModel):
    kind: Literal["success_message"] = "success_message"
    status: Literal["success"] = "success"


class CompletionError(BaseModel):
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

    error_type: ErrorType
    text: str

    @classmethod
    def from_openai(cls, error: openai.OpenAIError) -> Self:
        error_type = {
            openai.APIConnectionError: cls.ErrorType.SERVICE,
            openai.AuthenticationError: cls.ErrorType.AUTHENTICATION,
            openai.RateLimitError: cls.ErrorType.RATE_LIMIT,
            openai.Timeout: cls.ErrorType.TIMEOUT,
        }.get(error.__class__, cls.ErrorType.UNKNOWN)

        return cls(
            error_type=error_type,
            text=str(error),
        )

    def __str__(self) -> str:
        return f"Error {self.error_type}: {self.text}"


class ErrorMessageChunk(BaseModel):
    kind: Literal["error_message"] = "error_message"
    status: Literal["error"] = "error"
    source: str
    content: CompletionError


StreamingMessageResult = Annotated[
    Union[
        FunctionCallStartChunk,
        StreamingMessageChunk,
        SuccessMessageChunk,
        ErrorMessageChunk,
    ],
    Field(discriminator="kind"),
]

OpenAIModelFamily = Literal[
    "gpt-3.5",
    "gpt-4",
    "tts-1",
    "whisper-1",
    "text-embedding-ada-2",
]
Modality = Literal["text", "image", "audio", "speech"]


class OpenAIModel(BaseModel):
    model_config = ConfigDict(
        protected_namespaces=tuple(),
    )

    name: str
    model: str
    model_family: OpenAIModelFamily
    modalities: set[Modality]

    # For text-models, how many tokens can be in context
    context_length: Optional[int] = None

    # Approximate pricing, unit being
    # - Token (max of input + output)
    # - Image (for dall-e)
    # - Characters (TTS)
    # - Seconds (Whisper)
    cost_per_unit: float


OPENAI_MODELS = [
    # GPT 3.5
    OpenAIModel(
        name="gpt-3.5",
        model="gpt-3.5-turbo-16k",
        model_family="gpt-3.5",
        modalities={"text"},
        context_length=16_000,
        cost_per_unit=0.002 / 1_000,
    ),
    # GPT 4
    OpenAIModel(
        name="gpt-4",
        model="gpt-4",
        model_family="gpt-4",
        modalities={"text"},
        context_length=8_000,
        cost_per_unit=0.06 / 1_000,
    ),
    OpenAIModel(
        name="gpt-4-turbo",
        model="gpt-4-turbo-2024-04-09",
        model_family="gpt-4",
        modalities={"text", "image"},
        context_length=128_000,
        cost_per_unit=30_00 / 1_000_000,
    ),
    OpenAIModel(
        name="gpt-4-32k",
        model="gpt-4-32k",
        model_family="gpt-4",
        modalities={"text"},
        context_length=32_000,
        cost_per_unit=0.12 / 1_000,
    ),
    OpenAIModel(
        name="gpt-4-v",
        model="gpt-4-vision-preview",
        model_family="gpt-4",
        modalities={"text", "image"},
        context_length=128_000,
        cost_per_unit=0.03 / 1_000,
     ),
    # Whisper
    OpenAIModel(
        name="whisper",
        model="whisper-1",
        model_family="whisper-1",
        modalities={"audio"},
        cost_per_unit=0.006 / 60,
    ),
    # TTS
    OpenAIModel(
        name="tts",
        model="tts-1",
        model_family="tts-1",
        modalities={"speech"},
        cost_per_unit=0.015 / 1_000,
    ),
    OpenAIModel(
        name="tts-hd",
        model="tts-1-hd",
        model_family="tts-1",
        modalities={"speech"},
        cost_per_unit=0.03 / 1_000,
    ),
    # Embeddings
    OpenAIModel(
        name="embedding",
        model="text-embedding-ada-002",
        model_family="text-embedding-ada-2",
        modalities=set(),
        cost_per_unit=0.0001 / 1_000,
    ),
]

OPENAI_MODELS_BY_NAME = {model.name: model for model in OPENAI_MODELS}

OPENAI_MODELS_BY_MODEL = {model.model: model for model in OPENAI_MODELS}

OPENAI_MODELS_BY_MODEL_FAMILY = {
    model_family: [
        model for model in OPENAI_MODELS if model.model_family == model_family
    ]
    for model_family in [
        "gpt-3.5",
        "gpt-4",
        "tts-1",
        "whisper-1",
    ]
}

OPENAI_MODELS_BY_MODALITY = {
    modality: [model for model in OPENAI_MODELS if modality in model.modalities]
    for modality in ["text", "image", "audio", "speech"]
}


def get_openai_model(
    *,
    modalities: Iterable[Modality],
    # Optional filters
    name: Optional[str] = None,
    model: Optional[str] = None,
    model_family: Optional[OpenAIModelFamily] = None,
    context_length: Optional[int] = None,
) -> OpenAIModel:
    return _cached_get_openai_model(
        modalities=tuple(modalities),
        name=name,
        model=model,
        model_family=model_family,
        context_length=context_length,
    )


@cache
def _cached_get_openai_model(
    *,
    modalities: tuple[Modality],
    # Optional filters
    name: Optional[str] = None,
    model: Optional[str] = None,
    model_family: Optional[OpenAIModelFamily] = None,
    context_length: Optional[int] = None,
) -> OpenAIModel:
    modalities: set[Modality] = set(modalities)
    potential_models: list[OpenAIModel] = [
        model for model in OPENAI_MODELS if modalities.issubset(model.modalities)
    ]

    # The default model should prioritize modality capabilities followed by cost
    default_openai_model = min(
        potential_models,
        key=lambda model: model.cost_per_unit,
    )

    for field, value in [
        ("name", name),
        ("model", model),
        ("model_family", model_family),
    ]:
        if value is not None:
            potential_models = [
                potential_model
                for potential_model in potential_models
                if getattr(potential_model, field) == value
            ]

    if context_length is not None:
        potential_models = [
            model
            for model in potential_models
            if model.context_length and model.context_length >= context_length
        ]

    return min(
        potential_models,
        default=default_openai_model,
        key=lambda model: model.cost_per_unit,
    )
