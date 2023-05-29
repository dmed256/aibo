import json
import typing
from typing import Any, Optional, Type, TypeVar
from uuid import UUID

from pydantic import BaseModel
from sqlalchemy.ext.mutable import Mutable
from sqlalchemy.types import TEXT, TypeDecorator

from aibo.common.chat.message_content import (
    MessageContent,
    MessageContentAdapter,
    MessageContents,
    MessageContentsAdapter,
)
from aibo.common.chat.message_source import (
    MessageSource,
    MessageSourceAdapter,
    OpenAIModelSource,
)

__all__ = [
    "UUIDColumn",
    "StrListColumn",
    "MessageSourceColumn",
    "MessageContentColumn",
    "MessageContentsColumn",
    "OpenAIModelSourceColumn",
]

TPydanticModel = TypeVar("TPydanticModel", bound=BaseModel)


def create_pydantic_column_type(
    cls: Any, *, adapter: Optional[Any] = None
) -> Type[TypeDecorator]:
    class PydanticTypeColumn(TypeDecorator):
        impl = TEXT
        cache_ok = True

        def process_bind_param(
            self, value: Optional[TPydanticModel], dialect: Any
        ) -> Optional[str]:
            if value is None:
                return None

            if adapter:
                return typing.cast(str, adapter.dump_json(value))

            return value.model_dump_json()

        def process_result_value(
            self, value: Optional[str], dialect: Any
        ) -> Optional[Any]:
            if value is None:
                return None

            if adapter:
                return adapter.validate_json(value)
            else:
                return cls.model_validate_json(value)

    return PydanticTypeColumn


OpenAIModelSourceColumn = create_pydantic_column_type(OpenAIModelSource)
MessageSourceColumn = create_pydantic_column_type(
    MessageSource, adapter=MessageSourceAdapter
)
MessageContentColumn = create_pydantic_column_type(
    MessageContent, adapter=MessageContentAdapter
)
MessageContentsColumn = create_pydantic_column_type(
    MessageContents, adapter=MessageContentsAdapter
)


class UUIDColumn(TypeDecorator):
    impl = TEXT
    cache_ok = True

    def process_bind_param(self, value: Optional[UUID], dialect: Any) -> Optional[str]:
        if value is None:
            return None
        return str(value)

    def process_result_value(
        self, value: Optional[str], dialect: Any
    ) -> Optional[UUID]:
        if value is None:
            return None
        return UUID(value)


class StrListColumn(TypeDecorator):
    impl = TEXT
    cache_ok = True

    def process_bind_param(
        self, value: Optional[list[str]], dialect: Any
    ) -> Optional[str]:
        if value is None:
            return None
        return json.dumps(value)

    def process_result_value(
        self, value: Optional[str], dialect: Any
    ) -> Optional[list[str]]:
        if value is None:
            return None
        return typing.cast(list[str], json.loads(value))
