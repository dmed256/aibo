import abc
import asyncio
import typing
from typing import Any, Generic, TypeVar

from faker import Faker

from aibo.db.documents.base_document import BaseDocument

__all__ = ["fake", "BaseFactory", "BaseMongoFactory"]

fake = Faker()

T = TypeVar("T")


class BaseFactory(Generic[T], abc.ABC):
    # Each method should define its own build with its own kwargs:
    #
    #     @staticmethod
    #     @abc.abstractmethod
    #     async def build(**kwargs: Any) -> T:
    #         ...
    #
    # mypy doesn't like the **kwargs and I prefer factories have strict **kwargs so it's
    # simpler to ignore the [attr-defined] errors just in this file

    @classmethod
    async def build_many(cls, count: int, **kwargs: Any) -> list[T]:
        return await asyncio.gather(*[cls.build(**kwargs) for _ in range(count)])  # type: ignore[attr-defined]


class BaseMongoFactory(BaseFactory[T]):
    @classmethod
    async def post_create(cls, obj: T) -> T:
        return obj

    @classmethod
    async def create(cls, **kwargs: Any) -> T:
        doc = typing.cast(BaseDocument, await cls.build(**kwargs)).insert()  # type: ignore[attr-defined]
        return await cls.post_create(typing.cast(T, doc))

    @classmethod
    async def create_many(cls, count: int, **kwargs: Any) -> list[T]:
        return await asyncio.gather(*[cls.create(**kwargs) for _ in range(count)])
