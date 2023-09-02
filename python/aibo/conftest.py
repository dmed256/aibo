# isort: off
import os

os.environ["ENV"] = "test"

# isort: on
import asyncio
from typing import AsyncGenerator, Generator

import pytest

from aibo.common.constants import Env


@pytest.fixture(scope="session")
def event_loop() -> Generator[asyncio.AbstractEventLoop, None, None]:
    event_loop = asyncio.get_event_loop()
    yield event_loop
    event_loop.close()


async def drop_test_database() -> None:
    from aibo.db.client import get_client, get_db

    database_name = Env.get().MONGO_DATABASE
    assert database_name.startswith("aibo-test")

    client = get_client()
    await client.drop_database(database_name)

    assert get_db().name.startswith("aibo-test")


@pytest.fixture(autouse=True, scope="session")
async def clean_mongo_database(worker_id: str) -> AsyncGenerator[None, None]:
    if worker_id == "master":
        await drop_test_database()
        yield
        await drop_test_database()
    else:
        yield


@pytest.fixture(autouse=True, scope="function")
async def migrate_mongo_collections() -> None:
    """
    Each test has different collections, so run the migrations at the beginning
    """
    from aibo.db import migrate_documents

    await migrate_documents()
