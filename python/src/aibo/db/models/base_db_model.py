from typing import Optional, Self
from uuid import UUID

import sqlalchemy as sa
import sqlalchemy.orm as orm

from aibo.db.client import get_session


class BaseDBModel(orm.DeclarativeBase):
    @classmethod
    async def by_id(cls, id: UUID) -> Optional[Self]:
        async with get_session() as session:
            query_result = await session.execute(
                sa.select(cls).where(cls.id == id)  # type: ignore[attr-defined]
            )
        return query_result.scalars().first()

    async def insert(self) -> Self:
        async with get_session() as session:
            session.add(self)
            await session.commit()

        return self
