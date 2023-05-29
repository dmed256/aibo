import asyncio
import functools
import os
from contextlib import asynccontextmanager
from typing import Any, AsyncGenerator

import sqlalchemy as sa
from sqlalchemy.ext.asyncio import (
    AsyncEngine,
    AsyncSession,
    async_scoped_session,
    async_sessionmaker,
    create_async_engine,
)

from aibo.common.constants import MIGRATIONS_DIR, Env

__all__ = [
    "get_sync_engine",
    "get_async_engine",
    "get_session_factory",
    "get_session",
    "migrate_db",
]


@functools.cache
def get_sync_engine() -> sa.Engine:
    env = Env.get()
    os.makedirs(
        os.path.dirname(env.db_path),
        exist_ok=True,
    )
    return sa.create_engine(env.db_uri(sync=True))


@functools.cache
def get_async_engine() -> AsyncEngine:
    env = Env.get()
    os.makedirs(
        os.path.dirname(env.db_path),
        exist_ok=True,
    )
    return create_async_engine(env.db_uri(sync=False))


@functools.cache
def get_session_factory() -> async_scoped_session:
    sessionmaker = async_sessionmaker(get_async_engine(), expire_on_commit=False)
    return async_scoped_session(sessionmaker, scopefunc=asyncio.current_task)


@asynccontextmanager
async def get_session() -> AsyncGenerator[AsyncSession, None]:
    session_factory = get_session_factory()
    async with session_factory() as session:
        yield session


def migrate_db() -> None:
    from alembic import command
    from alembic.config import Config

    env = Env.get()

    alembic_cfg = Config()
    alembic_cfg.set_main_option("script_location", MIGRATIONS_DIR)
    alembic_cfg.set_main_option("sqlalchemy.url", env.db_uri(sync=True))
    command.upgrade(alembic_cfg, "head")
