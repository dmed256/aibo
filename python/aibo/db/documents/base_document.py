import abc
import functools
from typing import Literal, Optional, Self
from uuid import UUID, uuid4

import pymongo
from pydantic import BaseModel, Field

from aibo.common.time import now_utc
from aibo.common.types import StrEnum

__all__ = ["BaseDocument", "Index", "Order"]


class Order(StrEnum):
    ASC = "asc"
    DESC = "desc"

    def to_pymongo(self):
        if order == Order.ASC:
            return pymongo.ASCENDING
        return pymongo.DESCENDING


IndexType = Literal[
    pymongo.ASCENDING,
    pymongo.DESCENDING,
    pymongo.TEXT,
]


class Index(BaseModel):
    name: str
    fields: list[tuple[str, IndexType]]
    unique: bool


class BaseDocument(BaseModel, abc.ABC):
    id: UUID = Field(default_factory=uuid4, alias="_id")

    class Config:
        allow_population_by_field_name = True

    @classmethod
    @property
    @abc.abstractmethod
    def collection_name(cls) -> str:
        ...

    @classmethod
    @property
    def collection(cls):
        from aibo.db.client import get_db

        return get_db()[cls.collection_name]

    @classmethod
    def indices(cls) -> list[Index]:
        return []

    @classmethod
    def migrate(cls):
        existing_index_names = list(cls.collection.index_information().keys())
        unmigrated_indices = [
            index for index in cls.indices() if index.name not in existing_index_names
        ]
        for index in unmigrated_indices:
            fields = [
                (field_name, index_type) for field_name, index_type in index.fields
            ]
            cls.collection.create_index(fields, unique=index.unique, name=index.name)

    def safe_dict(self):
        return self.dict(by_alias=True)

    @classmethod
    def by_id(cls, id: UUID) -> Optional[Self]:
        doc_dict = cls.collection.find_one({"_id": id})
        if not doc_dict:
            return None

        return cls(**doc_dict)

    def insert(self) -> Self:
        self.collection.insert_one(self.safe_dict())
        return self

    @classmethod
    def insert_many(self, docs: list[Self]) -> list[Self]:
        self.collection.insert_many([doc.dict(by_alias=True) for doc in docs])
        return docs

    def save(self):
        self.collection.save(self.safe_dict())

    @classmethod
    def partial_update(cls, id: UUID, **updates):
        cls.collection.update_one({"_id": id}, {"$set": updates})
        return cls.by_id(id)

    def soft_delete(self) -> Self:
        self.deleted_at = now_utc()
        self.partial_update(id=self.id, deleted_at=self.deleted_at)
        return self

    @classmethod
    def find(cls, query, **kwargs):
        return [cls(**doc_dict) for doc_dict in cls.collection.find(query, **kwargs)]

    @classmethod
    def aggregate(cls, query, **kwargs):
        return [
            cls(**doc_dict) for doc_dict in cls.collection.aggregate(query, **kwargs)
        ]

    @classmethod
    def count(cls, *args, **kwargs):
        return cls.collection.count_documents(*args, **kwargs)
