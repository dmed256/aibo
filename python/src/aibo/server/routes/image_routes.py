import base64
from uuid import UUID, uuid4

from fastapi import APIRouter, Response
from pydantic import BaseModel

from aibo.db.models import ImageModel

router = APIRouter(prefix="/images")


class CreateImageFromClipboardResponse(BaseModel):
    image_id: UUID


@router.get("/{image_id}")
async def get_image(
    image_id: UUID,
) -> Response:
    image = await ImageModel.by_id(image_id)
    if not image:
        raise Exception(f"Image not found: {image_id}")

    return Response(
        content=base64.b64decode(image.contents_b64), media_type=f"image/{image.format}"
    )


@router.post("/clipboard")
async def create_image_from_clipboard() -> CreateImageFromClipboardResponse:
    trace_id = uuid4()
    image = await ImageModel.from_clipboard(trace_id=trace_id)
    if not image:
        raise ValueError("No image in clipboard")
    return CreateImageFromClipboardResponse(image_id=image.id)
