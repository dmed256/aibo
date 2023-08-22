import datetime as dt
from typing import Annotated, Literal, Optional, Union
from uuid import UUID, uuid4

import numpy as np
import pymongo
from pydantic import BaseModel, Field

from aibo.common.classproperty import classproperty
from aibo.common.time import now_utc
from aibo.common.types import StrEnum
from aibo.db.documents.base_document import BaseDocument, Index, Order

__all__ = [
    "GitFileDocument",
]


class FileImport(BaseModel):
    name: str
    alias: str
    imported_names: list[str]
    imports_all: bool


class FileVariable(BaseModel):
    name: str
    type: str
    summary: str


class FileFunction(BaseModel):
    name: str
    summary: str
    arguments: list[FileVariable]


class FileSchema(BaseModel):
    version: Literal["v1"] = "v1"
    filename: str
    language: str
    summary: str
    imports: list[FileImport]
    functions: list[FileFunction]
    variables: list[FileVariable]


class GitFileDocument(BaseDocument):
    git_repo_id: UUID
    filename: str
    git_commit: str
    embedding: Optional[np.ndarray] = None
    normalized_embedding: Optional[np.ndarray] = None
    file_schema: Optional[FileSchema] = None
    is_synced: bool = False
    created_at: dt.datetime = Field(default_factory=now_utc)
    deleted_at: Optional[dt.datetime] = None

    @classproperty
    def collection_name(cls) -> str:
        return "git_files"

    @classmethod
    def indices(cls) -> list[Index]:
        return [
            Index(
                name="unique_repo_file",
                fields=[
                    ("git_repo_id", pymongo.DESCENDING),
                    ("filename", pymongo.DESCENDING),
                ],
                unique=True,
            ),
            Index(
                name="git_repo_commit",
                fields=[
                    ("git_repo_id", pymongo.DESCENDING),
                    ("git_commit", pymongo.DESCENDING),
                ],
                unique=False,
            ),
            Index(
                name="created_at",
                fields=[("created_at", pymongo.DESCENDING)],
                unique=False,
            ),
        ]
