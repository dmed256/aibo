import abc
import asyncio
import datetime as dt
import functools
import os
import typing
from enum import StrEnum
from typing import Any, Literal, Optional, Self, Union, cast
from uuid import UUID, uuid4

import pymongo
from motor.motor_asyncio import AsyncIOMotorCollection
from pydantic import BaseModel, ConfigDict, Field

from aibo.common.classproperty import classproperty
from aibo.common.constants import Env
from aibo.common.time import now_utc

__all__ = ["BaseDocument", "Index", "Order"]


class Order(StrEnum):
    ASC = "asc"
    DESC = "desc"

    def to_pymongo(self) -> int:
        if self.value == Order.ASC:
            return pymongo.ASCENDING
        return pymongo.DESCENDING


IndexType = Union[int, str]


class Index(BaseModel):
    name: str
    fields: list[tuple[str, IndexType]]
    unique: bool


class BaseDocument(BaseModel, abc.ABC):
    model_config = ConfigDict(
        arbitrary_types_allowed=True,
        populate_by_name=True,
        ignored_types=(classproperty,),
    )

    id: UUID = Field(default_factory=uuid4, alias="_id")

    @classproperty
    @abc.abstractmethod
    def collection_name(cls) -> str:
        ...

    @classproperty
    def collection(cls) -> AsyncIOMotorCollection:
        from aibo.db.client import get_db

        collection_name = cls.collection_name
        if Env.get().ENV == "test":
            import hashlib

            test_name = os.environ.get("PYTEST_CURRENT_TEST")
            assert test_name, "Missing PYTEST_CURRENT_TEST environment variable"

            test_hash = hashlib.md5(test_name.encode("utf-8")).hexdigest()
            collection_name = f"{collection_name}-{test_hash}"

        return get_db()[collection_name]

    @classmethod
    def indices(cls) -> list[Index]:
        return []

    @classmethod
    async def migrate(cls) -> None:
        index_info = await cls.collection.index_information()
        existing_index_names = list(index_info.keys())
        unmigrated_indices = [
            index for index in cls.indices() if index.name not in existing_index_names
        ]
        await asyncio.gather(
            *[
                cls.collection.create_index(
                    [
                        (field_name, index_type)
                        for field_name, index_type in index.fields
                    ],
                    unique=index.unique,
                    name=index.name,
                )
                for index in unmigrated_indices
            ]
        )

    def safe_dict(self) -> dict[str, Any]:
        return self.model_dump(by_alias=True)

    @classmethod
    async def by_id(cls, id: UUID) -> Optional[Self]:
        doc_dict = await cls.collection.find_one({"_id": id})
        if not doc_dict:
            return None

        return cls(**doc_dict)

    async def insert(self) -> Self:
        await self.collection.insert_one(cast(Any, self.safe_dict()))
        return self

    @classmethod
    async def insert_many(self, docs: list[Self]) -> list[Self]:
        await self.collection.insert_many(
            [cast(Any, doc.model_dump(by_alias=True)) for doc in docs]
        )
        return docs

    @classmethod
    async def partial_update(cls, id: UUID, **updates: Any) -> Self:
        await cls.collection.update_one({"_id": id}, {"$set": updates})
        return cast(Self, await cls.by_id(id))

    async def soft_delete(self) -> Self:
        self.deleted_at: Optional[dt.datetime] = now_utc()
        await self.partial_update(id=self.id, deleted_at=self.deleted_at)
        return self

    @classmethod
    async def find_one(cls, query: Any, **kwargs: Any) -> Optional[Self]:
        maybe_doc_dict = await cls.collection.find_one(query, **kwargs)
        if maybe_doc_dict:
            return cls(**maybe_doc_dict)

        return None

    @classmethod
    async def find(cls, query: Any, *args: Any, **kwargs: Any) -> list[Self]:
        query_cursor = cls.collection.find(query, *args, **kwargs)
        return [cls(**doc_dict) async for doc_dict in query_cursor]

    @classmethod
    async def raw_find(cls, query: Any, *args: Any, **kwargs: Any) -> list[Any]:
        query_cursor = cls.collection.find(query, *args, **kwargs)
        return [row async for row in query_cursor]

    @classmethod
    async def aggregate(cls, query: Any, **kwargs: Any) -> list[Self]:
        return [
            cls(**doc_dict)
            async for doc_dict in cls.collection.aggregate(query, **kwargs)
        ]

    @classmethod
    async def count(cls, *args: Any, **kwargs: Any) -> int:
        return typing.cast(int, await cls.collection.count_documents(*args, **kwargs))
