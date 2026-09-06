import datetime as dt
from typing import Optional, Self, cast
from uuid import UUID

import sqlalchemy as sa
import sqlalchemy.orm as orm

from aibo.common.time import now_utc
from aibo.db.client import get_session


class BaseDBModel(orm.DeclarativeBase):
    @classmethod
    async def by_id(cls, id: UUID) -> Optional[Self]:
        async with get_session() as session:
            query_result = await session.execute(sa.select(cls).where(cls.id == id))
        return cast(Optional[Self], query_result.scalars().first())

    async def insert(self) -> Self:
        async with get_session() as session:
            session.add(self)
            await session.commit()

        return self


class TimestampMixin:
    created_at: orm.Mapped[dt.datetime] = orm.mapped_column(
        sa.DateTime(timezone=True), default=now_utc
    )
    updated_at: orm.Mapped[dt.datetime] = orm.mapped_column(
        sa.DateTime(timezone=True), default=now_utc, onupdate=now_utc
    )
