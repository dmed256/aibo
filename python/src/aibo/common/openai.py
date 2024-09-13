from enum import StrEnum
from functools import cache
from typing import Annotated, Any, Iterable, Literal, Self, Union

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
    name: str | None = None
    function_call: dict[str, Any] | None = None


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
    "o1",
    "gpt-4",
    "gpt-3.5",
    "tts-1",
    "whisper-1",
    "text-embedding-ada-2",
]
Modality = Literal["text", "image", "audio", "speech", "embedding"]


class OpenAIModel(BaseModel):
    model_config = ConfigDict(
        protected_namespaces=tuple(),
    )

    name: str
    model: str
    model_family: OpenAIModelFamily
    modalities: set[Modality]


# Ordered by first = latest
OPENAI_MODELS = [
    # o1
    OpenAIModel(
        name="o1-preview",
        model="o1-preview",
        model_family="o1",
        modalities={"text"},
    ),
    OpenAIModel(
        name="o1-mini",
        model="o1-mini",
        model_family="o1",
        modalities={"text"},
    ),
    # GPT 4o
    OpenAIModel(
        name="gpt-4o-mini",
        model="gpt-4o-mini",
        model_family="gpt-4",
        modalities={"text", "image"},
    ),
    OpenAIModel(
        name="gpt-4o",
        model="gpt-4o",
        model_family="gpt-4",
        modalities={"text", "image"},
    ),
    # GPT 4
    OpenAIModel(
        name="gpt-4-v",
        model="gpt-4-vision-preview",
        model_family="gpt-4",
        modalities={"text", "image"},
    ),
    OpenAIModel(
        name="gpt-4-32k",
        model="gpt-4-32k",
        model_family="gpt-4",
        modalities={"text"},
    ),
    OpenAIModel(
        name="gpt-4-turbo",
        model="gpt-4-turbo-2024-04-09",
        model_family="gpt-4",
        modalities={"text", "image"},
    ),
    OpenAIModel(
        name="gpt-4",
        model="gpt-4",
        model_family="gpt-4",
        modalities={"text"},
    ),
    # GPT 3.5
    OpenAIModel(
        name="gpt-3.5",
        model="gpt-3.5-turbo-16k",
        model_family="gpt-3.5",
        modalities={"text"},
    ),
    # Whisper
    OpenAIModel(
        name="whisper",
        model="whisper-1",
        model_family="whisper-1",
        modalities={"audio"},
    ),
    # TTS
    OpenAIModel(
        name="tts",
        model="tts-1",
        model_family="tts-1",
        modalities={"speech"},
    ),
    OpenAIModel(
        name="tts-hd",
        model="tts-1-hd",
        model_family="tts-1",
        modalities={"speech"},
    ),
    # Embeddings
    OpenAIModel(
        name="embedding",
        model="text-embedding-ada-002",
        model_family="text-embedding-ada-2",
        modalities={"embedding"},
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
    name: str | None = None,
    model: str | None = None,
    model_family: OpenAIModelFamily | None = None,
) -> OpenAIModel:
    return _cached_get_openai_model(
        modalities=tuple(modalities),
        name=name,
        model=model,
        model_family=model_family,
    )


@cache
def _cached_get_openai_model(
    *,
    modalities: tuple[Modality],
    # Optional filters
    name: str | None = None,
    model: str | None = None,
    model_family: OpenAIModelFamily | None = None,
) -> OpenAIModel:
    modalities: set[Modality] = set(modalities)
    potential_models: list[OpenAIModel] = [
        model for model in OPENAI_MODELS if modalities.issubset(model.modalities)
    ]

    default_openai_model = potential_models[0]

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

    if potential_models:
        return potential_models[0]

    if name:
        return OpenAIModel(
            name=name,
            model=name,
            model_family="gpt-4",
            modalities=modalities or {"text", "image"},
        )

    return default_openai_model
