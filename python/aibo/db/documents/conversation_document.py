import datetime as dt
from typing import Any, Optional, Self
from uuid import UUID

import pymongo
from pydantic import BaseModel, Field

from aibo.common.classproperty import classproperty
from aibo.common.time import now_utc
from aibo.db.documents.base_document import BaseDocument, Index, Order
from aibo.db.documents.message_source import *

__all__ = ["ConversationDocument"]


class ConversationDocument(BaseDocument):
    title: str
    openai_model_source: OpenAIModelSource
    enabled_tool_names: list[str]
    root_message_id: UUID
    current_message_id: UUID

    # If the conversation spun off another conversation, track
    # which conversation and message in it
    origin_message_id: Optional[UUID] = None
    conversation_depth: int = 0

    created_at: dt.datetime = Field(default_factory=now_utc)
    deleted_at: Optional[dt.datetime] = None

    @classproperty
    def collection_name(cls) -> str:
        return "conversations"

    @classmethod
    def indices(cls) -> list[Index]:
        return [
            Index(name="title", fields=[("title", pymongo.DESCENDING)], unique=False),
            Index(
                name="created_at",
                fields=[("created_at", pymongo.DESCENDING)],
                unique=False,
            ),
        ]

    @classmethod
    def safe_find(cls, *, include_deletions: bool = False, **kwargs: Any) -> list[Self]:
        query = dict(kwargs)
        if not include_deletions:
            query["deleted_at"] = None

        return cls.find(query)
