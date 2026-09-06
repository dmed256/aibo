import functools
import os
from pathlib import Path
from typing import Literal, Self

from pydantic import BaseModel

__all__ = ["PACKAGE_DIR", "MIGRATIONS_DIR", "WEB_DIR", "Env"]

PACKAGE_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
MIGRATIONS_DIR = os.path.join(PACKAGE_DIR, "__db_migrations")
WEB_DIR = os.path.join(PACKAGE_DIR, "resources", "web")


class Env(BaseModel):
    # Env
    ENV: Literal["dev", "test"]

    DATABASE_URL: str

    # Runtime files and services
    CACHE_DIR: str
    BOUNCER_URL: str
    CODEX_COMMAND: str
    NGINX_COMMAND: str

    def db_uri(self, *, sync: bool = False) -> str:
        if sync:
            return self.DATABASE_URL.replace(
                "postgresql+asyncpg://", "postgresql+psycopg://", 1
            ).replace("sqlite+aiosqlite://", "sqlite://", 1)
        return self.DATABASE_URL

    @property
    def docs_dir(self) -> Path:
        return Path(self.CACHE_DIR) / "docs"

    @property
    def projects_dir(self) -> Path:
        return Path(self.CACHE_DIR) / "projects"

    @property
    def assets_dir(self) -> Path:
        return Path(self.CACHE_DIR) / "assets"

    @property
    def logs_dir(self) -> Path:
        return Path(self.CACHE_DIR) / "logs"

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
            DATABASE_URL=os.environ.get(
                "AIBO_DATABASE_URL",
                "postgresql+asyncpg://aibo:aibo@127.0.0.1/aibo",
            ),
            # Runtime files and services
            CACHE_DIR=os.environ.get(
                "AIBO_CACHE_DIR", os.path.expanduser("~/.cache/aibo")
            ),
            BOUNCER_URL=os.environ.get("AIBO_BOUNCER_URL", "http://127.0.0.1:5010"),
            CODEX_COMMAND=os.environ.get(
                "AIBO_CODEX_COMMAND", "codex app-server --stdio"
            ),
            NGINX_COMMAND=os.environ.get("AIBO_NGINX_COMMAND", "nginx"),
        )

    @classmethod
    def _get_test(cls) -> Self:
        return cls(
            # Env
            ENV="test",
            DATABASE_URL=os.environ.get(
                "AIBO_DATABASE_URL",
                "postgresql+asyncpg://aibo:aibo@127.0.0.1/aibo_test",
            ),
            # Runtime files and services
            CACHE_DIR=os.environ.get("AIBO_CACHE_DIR", "/tmp/aibo-tmp/test/cache"),
            BOUNCER_URL="http://127.0.0.1:5010",
            CODEX_COMMAND=os.environ.get(
                "AIBO_CODEX_COMMAND", "codex app-server --stdio"
            ),
            NGINX_COMMAND=os.environ.get("AIBO_NGINX_COMMAND", "nginx"),
        )
