import functools
from typing import Any

from bson.binary import UuidRepresentation
from bson.codec_options import CodecOptions
from motor.motor_asyncio import AsyncIOMotorClient, AsyncIOMotorDatabase

from aibo.common.constants import Env


@functools.cache
def get_client() -> AsyncIOMotorClient:
    env = Env.get()
    return AsyncIOMotorClient(env.MONGO_URI, env.MONGO_PORT)


@functools.cache
def get_db() -> AsyncIOMotorDatabase:
    env = Env.get()
    return get_client().get_database(
        env.MONGO_DATABASE,
        codec_options=CodecOptions(uuid_representation=UuidRepresentation.STANDARD),
    )
