import functools
import os
from typing import Literal, Self
from uuid import UUID

from pydantic import BaseModel

__all__ = ["Env", "NULL_UUID"]

NULL_UUID = UUID("00000000-0000-0000-0000-000000000000")


class Env(BaseModel):
    # Env
    ENV: Literal["dev", "test"]

    # Mongo
    MONGO_URI: str
    MONGO_PORT: int
    MONGO_DATABASE: str

    # OpenAI
    OPENAI_MODEL_NAME: str
    OPENAI_EMBEDDING_MODEL_NAME: str
    OPENAI_TEMPERATURE: float

    # Misc
    CURRENT_USER: str
    MAX_AIBO_MESSAGES: int
    MAX_AIBO_MESSAGE_RETRIES: int

    @classmethod
    @functools.cache
    def get(cls) -> Self:
        env = os.environ.get("ENV", "dev")
        if env == "test":
            return cls._get_test()

        if env == "dev":
            return cls._get_dev()

        raise ValueError(f"Invalid environment: {env}")

    @classmethod
    def _get_dev(cls) -> Self:
        return cls(
            # Env
            ENV="dev",
            # Mongo
            MONGO_URI=os.environ.get("MONGO_URI", "localhost"),
            MONGO_PORT=int(os.environ.get("MONGO_PORT", "27017")),
            MONGO_DATABASE=os.environ.get("MONGO_DATABASE", "aibo-local"),
            # OpenAI
            OPENAI_MODEL_NAME=os.environ.get("OPENAI_MODEL_NAME", "gpt-3.5-turbo"),
            OPENAI_EMBEDDING_MODEL_NAME=os.environ.get(
                "OPENAI_EMBEDDING_MODEL_NAME", "text-embedding-ada-002"
            ),
            OPENAI_TEMPERATURE=float(os.environ.get("OPENAI_TEMPERATURE", "0.9")),
            # Misc
            CURRENT_USER=os.environ.get("USER", "unknown"),
            MAX_AIBO_MESSAGES=int(os.environ.get("MAX_AIBO_MESSAGES", "15")),
            MAX_AIBO_MESSAGE_RETRIES=int(
                os.environ.get("MAX_AIBO_MESSAGE_RETRIES", "5")
            ),
        )

    @classmethod
    def _get_test(cls) -> Self:
        database = os.environ.get("MONGO_DATABASE", "aibo-test")
        assert database.startswith("aibo-test")

        return cls(
            # Env
            ENV="test",
            # Mongo
            MONGO_URI=os.environ.get("MONGO_URI", "localhost"),
            MONGO_PORT=int(os.environ.get("MONGO_PORT", "27017")),
            MONGO_DATABASE=os.environ.get("MONGO_DATABASE", "aibo-test"),
            # OpenAI
            OPENAI_MODEL_NAME="fake-model",
            OPENAI_EMBEDDING_MODEL_NAME="fake-embedding-model",
            OPENAI_TEMPERATURE=float(os.environ.get("OPENAI_TEMPERATURE", "0.9")),
            # Misc
            CURRENT_USER="test",
            MAX_AIBO_MESSAGES=15,
            MAX_AIBO_MESSAGE_RETRIES=5,
        )
