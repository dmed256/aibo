import functools
from typing import Any

from bson.binary import UuidRepresentation
from bson.codec_options import CodecOptions
from pymongo import MongoClient, database
from pymongo.typings import _DocumentType

from aibo.common.constants import Env


@functools.cache
def get_client() -> MongoClient:
    env = Env.get()
    return MongoClient(env.MONGO_URI, env.MONGO_PORT)


@functools.cache
def get_db() -> database.Database[_DocumentType]:
    env = Env.get()
    return get_client().get_database(
        env.MONGO_DATABASE,
        codec_options=CodecOptions(uuid_representation=UuidRepresentation.STANDARD),
    )
