import pymongo
import datetime as dt
import json
from typing import Annotated, Any, Literal, Optional, Self, Union
from uuid import UUID, uuid4

from pydantic import BaseModel, Field

from aibo.common.time import now_utc
from aibo.db.documents.base_document import BaseDocument, Index, Order

__all__ = ["MessageEdgeDocument"]


class MessageEdgeDocument(BaseDocument):
    conversation_id: UUID
    parent_id: UUID
    child_id: UUID
    created_at: dt.datetime

    @classmethod
    @property
    def collection_name(cls) -> str:
        return "message_edges"

    @classmethod
    def indices(cls) -> list[Index]:
        return [
            Index(
                name="conversation_id",
                fields=[("conversation_id", pymongo.DESCENDING)],
                unique=False,
            ),
            Index(
                name="parent_id",
                fields=[("parent_id", pymongo.DESCENDING)],
                unique=False,
            ),
            Index(
                name="latest_parent_by_time",
                fields=[
                    ("conversation_id", pymongo.DESCENDING),
                    ("child_id", pymongo.DESCENDING),
                    ("created_at", pymongo.DESCENDING),
                ],
                unique=True,
            ),
        ]
