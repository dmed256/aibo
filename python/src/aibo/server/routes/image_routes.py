import base64
from uuid import UUID

from fastapi import APIRouter, Response

from aibo.db.models import ImageModel

router = APIRouter(prefix="/images")


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
