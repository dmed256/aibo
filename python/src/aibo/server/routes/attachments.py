import base64
import binascii
from pathlib import Path
from uuid import UUID, uuid4

import sqlalchemy as sa
from fastapi import APIRouter, Depends, HTTPException, status
from fastapi.responses import Response
from sqlalchemy.ext.asyncio import AsyncSession

from aibo.common.constants import Env
from aibo.common.images import MAX_IMAGE_BYTES, image_type, read_image
from aibo.core import workspace
from aibo.db.client import get_db
from aibo.db.models import ChatMessageModel, ChatModel
from aibo.server import schemas
from aibo.server.routes.common import not_found

router = APIRouter()


IMAGE_EXTENSIONS = {
    "image/png": ".png",
    "image/jpeg": ".jpg",
    "image/gif": ".gif",
    "image/webp": ".webp",
}


def validate_paths(paths: object) -> None:
    if not isinstance(paths, list) or not all(isinstance(path, str) for path in paths):
        raise HTTPException(
            status_code=422, detail="attachments must be a list of paths"
        )
    for index, path in enumerate(paths):
        try:
            if not Path(path).is_absolute():
                raise ValueError("attachment path must be absolute")
            read_image(Path(path))
        except (OSError, ValueError) as error:
            raise HTTPException(
                status_code=422,
                detail=f"attachment {index} must be a readable PNG, JPEG, GIF or WebP image",
            ) from error


def image_response(path: Path) -> Response:
    try:
        content, media_type = read_image(path)
    except OSError as error:
        raise not_found("attachment") from error
    except ValueError as error:
        raise HTTPException(status_code=422, detail=str(error)) from error
    # Serve precisely the bytes validated, even if the file changes afterward.
    return Response(
        content, media_type=media_type, headers={"X-Content-Type-Options": "nosniff"}
    )


@router.post(
    "/attachments",
    response_model=schemas.Attachment,
    status_code=status.HTTP_201_CREATED,
)
async def upload_attachment(body: schemas.UploadAttachment) -> schemas.Attachment:
    try:
        content = base64.b64decode(body.data, validate=True)
    except (binascii.Error, ValueError) as error:
        raise HTTPException(
            status_code=status.HTTP_422_UNPROCESSABLE_ENTITY,
            detail="invalid base64 attachment",
        ) from error
    if not content or len(content) > MAX_IMAGE_BYTES:
        raise HTTPException(
            status_code=status.HTTP_413_REQUEST_ENTITY_TOO_LARGE,
            detail="attachment must be between 1 byte and 20 MiB",
        )
    try:
        media_type = image_type(content)
    except ValueError as error:
        raise HTTPException(status_code=422, detail=str(error)) from error
    if media_type != body.media_type:
        raise HTTPException(
            status_code=422, detail="attachment media type does not match its contents"
        )
    attachment_id = uuid4()
    filename = f"{attachment_id}{IMAGE_EXTENSIONS[body.media_type]}"
    assets_dir = Env.get().assets_dir
    assets_dir.mkdir(parents=True, exist_ok=True)
    path = assets_dir / filename
    path.write_bytes(content)
    return schemas.Attachment(
        path=path,
        name=Path(body.name).name,
        size=len(content),
        url=f"/api/attachments/{filename}",
    )


@router.get("/attachments/{filename}", response_class=Response)
async def get_attachment(filename: str) -> Response:
    if Path(filename).name != filename:
        raise not_found("attachment")
    path = Env.get().assets_dir / filename
    return image_response(path)


@router.get("/messages/{message_id}/attachments/{index}", response_class=Response)
async def message_attachment(
    message_id: UUID, index: int, db: AsyncSession = Depends(get_db)
) -> Response:
    message = await db.scalar(
        sa.select(ChatMessageModel)
        .join(ChatModel)
        .where(
            ChatMessageModel.id == message_id,
            ChatModel.kind != workspace.ChatKind.SHADOW,
        )
    )
    paths = message.data.get("attachments") if message else None
    if (
        not isinstance(paths, list)
        or not 0 <= index < len(paths)
        or not isinstance(paths[index], str)
    ):
        raise not_found("attachment")
    path = Path(paths[index])
    return image_response(path)
