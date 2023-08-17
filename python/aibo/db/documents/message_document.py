import datetime as dt
from typing import Optional, Self, Union
from uuid import UUID, uuid4

import pymongo
from pydantic import BaseModel, Field

from aibo.common.time import now_utc
from aibo.common.types import StrEnum
from aibo.db.documents.base_document import BaseDocument, Index, Order
from aibo.db.documents.message_content import *
from aibo.db.documents.message_source import *

__all__ = [
    "MessageDocument",
    "MessageRole",
]


class MessageRole(StrEnum):
    SYSTEM = "system"
    USER = "user"
    ASSISTANT = "assistant"
    FUNCTION = "function"
    ERROR = "error"


class MessageDocument(BaseDocument):
    conversation_id: UUID
    parent_id: Optional[UUID] = None
    source: MessageSource
    source_text: str
    role: MessageRole
    content: MessageContent
    content_text: str
    created_at: dt.datetime = Field(default_factory=now_utc)
    deleted_at: Optional[dt.datetime] = None

    @classmethod
    @property
    def collection_name(cls) -> str:
        return "messages"

    @classmethod
    def indices(cls) -> list[Index]:
        return [
            Index(
                name="conversation_id",
                fields=[("conversation_id", pymongo.DESCENDING)],
                unique=False,
            ),
            Index(
                name="children",
                fields=[
                    ("conversation_id", pymongo.DESCENDING),
                    ("parent_id", pymongo.DESCENDING),
                ],
                unique=False,
            ),
            Index(
                name="source_kind",
                fields=[("source.kind", pymongo.DESCENDING)],
                unique=False,
            ),
            Index(
                name="content_kind",
                fields=[("content.kind", pymongo.DESCENDING)],
                unique=False,
            ),
            Index(
                name="content_text",
                fields=[("content_text", pymongo.TEXT)],
                unique=False,
            ),
            Index(
                name="created_at",
                fields=[("created_at", pymongo.DESCENDING)],
                unique=False,
            ),
        ]

    def insert(self) -> Self:
        from aibo.db.documents.message_edge_document import MessageEdgeDocument

        self.created_at = now_utc()
        super().insert()

        if self.parent_id:
            MessageEdgeDocument(
                conversation_id=self.conversation_id,
                parent_id=self.parent_id,
                child_id=self.id,
                created_at=self.created_at,
            ).insert()

        return self

    def change_parent(self, parent_id: UUID, *, changed_at: dt.datetime) -> Self:
        from aibo.db.documents.message_edge_document import MessageEdgeDocument

        self.parent_id = parent_id
        if self.parent_id:
            MessageEdgeDocument(
                conversation_id=self.conversation_id,
                parent_id=self.parent_id,
                child_id=self.id,
                created_at=changed_at,
            ).insert()

        return self

    @classmethod
    def safe_find(
        cls, query, *, include_deletions: bool = False, **kwargs
    ) -> list[Self]:
        query = dict(query)
        if not include_deletions:
            query["deleted_at"] = None

        return cls.find(query, **kwargs)

    @classmethod
    def get_conversation_messages(
        cls, conversation_id: UUID, *, include_deletions: bool = False
    ) -> list[Self]:
        return cls.safe_find(
            {
                "conversation_id": conversation_id,
            },
            include_deletions=include_deletions,
        )

    def get_children(self):
        return self.get_message_children(
            conversation_id=self.conversation_id,
            parent_id=self.id,
        )

    @classmethod
    def get_message_children(
        cls,
        *,
        conversation_id: UUID,
        parent_id: Union[UUID, None],
        include_deletions: bool = False,
    ):
        return cls.safe_find(
            {
                "conversation_id": conversation_id,
                "parent_id": parent_id,
            },
            include_deletions=include_deletions,
        )
