from collections.abc import AsyncIterator

from aibo.bouncer.client import BouncerClient
from aibo.common.constants import Env


async def get_bouncer() -> AsyncIterator[BouncerClient]:
    client = BouncerClient(Env.get().BOUNCER_URL)
    try:
        yield client
    finally:
        await client.close()
