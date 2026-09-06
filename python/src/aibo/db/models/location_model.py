from __future__ import annotations

from uuid import UUID, uuid4

import sqlalchemy as sa
import sqlalchemy.orm as orm

from aibo.db.models.base_db_model import BaseDBModel, TimestampMixin


class LocationModel(TimestampMixin, BaseDBModel):
    __tablename__ = "locations"

    id: orm.Mapped[UUID] = orm.mapped_column(sa.Uuid, primary_key=True, default=uuid4)
    name: orm.Mapped[str] = orm.mapped_column(sa.String(80), unique=True)
    path: orm.Mapped[str] = orm.mapped_column(sa.Text)
