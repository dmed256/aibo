import functools
import os
import typing
from typing import Literal, Optional, Self
from uuid import UUID

from pydantic import BaseModel

__all__ = ["PACKAGE_DIR", "MIGRATIONS_DIR", "Env"]

PACKAGE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
MIGRATIONS_DIR = os.path.join(PACKAGE_DIR, "__db_migrations")


class Env(BaseModel):
    # Env
    ENV: Literal["dev", "test"]

    # DB
    DB_DIR: str
    DB_NAME: str

    # OpenAI
    OPENAI_MODEL: str
    OPENAI_EMBEDDING_MODEL: str
    OPENAI_TEMPERATURE: float
    OPENAI_IMAGE_DETAIL: Literal["auto", "low", "high"]

    # Packages
    AIBO_CUSTOM_PACKAGES_FILE: Optional[str]

    # Misc
    CURRENT_USER: str
    MAX_AIBO_MESSAGES: int
    MAX_AIBO_MESSAGE_RETRIES: int

    @property
    def db_path(self) -> str:
        return os.path.abspath(os.path.join(self.DB_DIR, self.DB_NAME))

    def db_uri(self, *, sync: bool = False) -> str:
        if sync:
            return f"sqlite:///{self.db_path}"
        else:
            return f"sqlite+aiosqlite:///{self.db_path}"

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
            # DB
            DB_DIR=os.environ.get(
                "AIBO_DB_DIR",
                os.path.expanduser("~/.aibo"),
            ),
            DB_NAME=os.environ.get("AIBO_DB_NAME", "database.db"),
            # OpenAI
            OPENAI_MODEL=os.environ.get("OPENAI_MODEL", "gpt-3.5-turbo-16k"),
            OPENAI_EMBEDDING_MODEL=os.environ.get(
                "OPENAI_EMBEDDING_MODEL", "text-embedding-ada-002"
            ),
            OPENAI_TEMPERATURE=float(os.environ.get("OPENAI_TEMPERATURE", "0.3")),
            OPENAI_IMAGE_DETAIL=typing.cast(
                Literal["auto", "low", "high"],
                os.environ.get("OPENAI_IMAGE_DETAIL", "auto"),
            ),
            # Packages
            AIBO_CUSTOM_PACKAGES_FILE=os.environ.get("AIBO_CUSTOM_PACKAGES_FILE"),
            # Misc
            CURRENT_USER=os.environ.get("USER", "unknown"),
            MAX_AIBO_MESSAGES=int(os.environ.get("MAX_AIBO_MESSAGES", "15")),
            MAX_AIBO_MESSAGE_RETRIES=int(
                os.environ.get("MAX_AIBO_MESSAGE_RETRIES", "5")
            ),
        )

    @classmethod
    def _get_test(cls) -> Self:
        return cls(
            # Env
            ENV="test",
            # DB
            DB_DIR=os.environ.get(
                "AIBO_DB_DIR",
                os.path.expanduser("~/.aibo/testing"),
            ),
            DB_NAME=os.environ.get("AIBO_DB_NAME", "test_database.db"),
            # OpenAI
            OPENAI_MODEL="fake-model",
            OPENAI_EMBEDDING_MODEL="fake-embedding-model",
            OPENAI_TEMPERATURE=float(os.environ.get("OPENAI_TEMPERATURE", "0.3")),
            OPENAI_IMAGE_DETAIL="auto",
            # Packages
            AIBO_CUSTOM_PACKAGES_FILE=os.environ.get("AIBO_CUSTOM_PACKAGES_FILE"),
            # Misc
            CURRENT_USER="test",
            MAX_AIBO_MESSAGES=15,
            MAX_AIBO_MESSAGE_RETRIES=5,
        )
