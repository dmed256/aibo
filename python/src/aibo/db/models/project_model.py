from __future__ import annotations

import datetime as dt
from uuid import UUID, uuid4

import sqlalchemy as sa
import sqlalchemy.orm as orm

from aibo.db.models.base_db_model import BaseDBModel, TimestampMixin


class ProjectModel(TimestampMixin, BaseDBModel):
    __tablename__ = "projects"

    id: orm.Mapped[UUID] = orm.mapped_column(sa.Uuid, primary_key=True, default=uuid4)
    name: orm.Mapped[str] = orm.mapped_column(sa.String(80), unique=True)
    description: orm.Mapped[str] = orm.mapped_column(sa.String(500))
    archived_at: orm.Mapped[dt.datetime | None] = orm.mapped_column(
        sa.DateTime(timezone=True)
    )
