import base64
import datetime as dt
import io
from typing import Optional, Self
from uuid import UUID, uuid4

import sqlalchemy as sa
import sqlalchemy.orm as orm
from PIL import ImageGrab
from PIL.Image import Image as ImageType

from aibo.common.time import now_utc
from aibo.db.models.base_db_model import BaseDBModel
from aibo.db.models.custom_types import UUIDColumn


class ImageModel(BaseDBModel):
    __tablename__ = "images"
    __table_args__ = (
        sa.Index("images_idx_trace_id", "trace_id"),
        sa.Index("images_idx_conversation_id", "conversation_id"),
        sa.Index("images_idx_created_at", "created_at"),
    )

    id: orm.Mapped[UUID] = orm.mapped_column(
        UUIDColumn, primary_key=True, default=uuid4
    )
    trace_id: orm.Mapped[UUID] = orm.mapped_column(UUIDColumn)

    conversation_id: orm.Mapped[Optional[UUID]] = orm.mapped_column(
        UUIDColumn,
        sa.ForeignKey("conversations.id"),
    )
    format: orm.Mapped[str]
    contents_b64: orm.Mapped[str]
    created_at: orm.Mapped[dt.datetime] = orm.mapped_column(default=now_utc)

    @classmethod
    async def from_image(cls, *, image: ImageType, trace_id: UUID) -> Self:
        jpeg_image = image.convert("RGB")
        jpeg_buffer = io.BytesIO()
        jpeg_image.save(jpeg_buffer, format="JPEG")
        jpeg_image_base64 = base64.b64encode(jpeg_buffer.getvalue()).decode("utf-8")

        return await cls(
            trace_id=trace_id,
            format="jpeg",
            contents_b64=jpeg_image_base64,
        ).insert()

    @classmethod
    async def from_clipboard(cls, *, trace_id: UUID) -> Optional[Self]:
        if clipboard_image := ImageGrab.grabclipboard():
            return await cls.from_image(
                image=clipboard_image,
                trace_id=trace_id,
            )

        return None

    @classmethod
    async def from_screen(cls, *, trace_id: UUID) -> Optional[Self]:
        if screen_image := ImageGrab.grab():
            return await cls.from_image(
                image=screen_image,
                trace_id=trace_id,
            )

        return None
