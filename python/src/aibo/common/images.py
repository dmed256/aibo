"""Validate raster attachments from their bytes, not their filenames."""

import os
import stat
import warnings
from io import BytesIO
from pathlib import Path

from PIL import Image

MAX_IMAGE_BYTES = 20 * 1024 * 1024
IMAGE_TYPES = {
    "PNG": "image/png",
    "JPEG": "image/jpeg",
    "GIF": "image/gif",
    "WEBP": "image/webp",
}


def image_type(content: bytes) -> str:
    if not content or len(content) > MAX_IMAGE_BYTES:
        raise ValueError("attachment must be between 1 byte and 20 MiB")
    try:
        with warnings.catch_warnings():
            warnings.simplefilter("error", Image.DecompressionBombWarning)
            with Image.open(BytesIO(content), formats=list(IMAGE_TYPES)) as image:
                media_type = IMAGE_TYPES[image.format]
                image.verify()
            # Some formats only discover truncated pixel data during decoding.
            with Image.open(BytesIO(content), formats=list(IMAGE_TYPES)) as image:
                image.load()
    except (
        OSError,
        ValueError,
        SyntaxError,
        Image.DecompressionBombError,
        Image.DecompressionBombWarning,
    ) as error:
        raise ValueError(
            "attachment must be a valid PNG, JPEG, GIF or WebP image"
        ) from error
    return media_type


def read_image(path: Path) -> tuple[bytes, str]:
    # Check the opened file, not a racy pathname stat; never block on a FIFO.
    with os.fdopen(os.open(path, os.O_RDONLY | os.O_NONBLOCK), "rb") as stream:
        if not stat.S_ISREG(os.fstat(stream.fileno()).st_mode):
            raise OSError("attachment is not a regular file")
        content = stream.read(MAX_IMAGE_BYTES + 1)
    return content, image_type(content)
