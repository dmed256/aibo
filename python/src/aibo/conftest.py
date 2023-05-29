# isort: off
import os
import functools

os.environ["ENV"] = "test"

# Remove caching for tests
functools.cache = lambda fn: fn  # type: ignore

# isort: on
import asyncio
import hashlib
import shutil
from typing import AsyncGenerator, Generator

import pytest

from aibo.common.constants import Env


@pytest.fixture(scope="session")
def event_loop() -> Generator[asyncio.AbstractEventLoop, None, None]:
    event_loop = asyncio.get_event_loop()
    yield event_loop
    event_loop.close()


@pytest.fixture(autouse=True, scope="session")
async def clean_test_database(worker_id: str) -> AsyncGenerator[None, None]:
    db_dir = Env.get().DB_DIR
    assert db_dir.endswith(".aibo/testing")

    if worker_id == "master":
        shutil.rmtree(db_dir, ignore_errors=True)
        os.makedirs(db_dir, exist_ok=True)
        yield
        shutil.rmtree(db_dir, ignore_errors=True)
    else:
        yield


@pytest.fixture(autouse=True, scope="function")
def migrate_test_db() -> None:
    """
    Each test has different collections, so run the migrations at the beginning
    """
    from aibo.db import (
        get_async_engine,
        get_session_factory,
        get_sync_engine,
        migrate_db,
    )

    test_name = os.environ.get("PYTEST_CURRENT_TEST")
    assert test_name, "Missing PYTEST_CURRENT_TEST environment variable"

    test_hash = hashlib.md5(test_name.encode("utf-8")).hexdigest()

    os.environ["AIBO_DB_DIR"] = os.environ.get(
        "AIBO_TEST_DB_DIR",
        os.path.expanduser("~/.aibo/testing"),
    )
    os.environ["AIBO_DB_NAME"] = f"{test_hash}.db"

    migrate_db()
