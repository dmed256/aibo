import os
from io import BytesIO
from pathlib import Path

import pytest
from PIL import Image

from aibo.common import images


@pytest.mark.parametrize("format", ["PNG", "JPEG", "GIF", "WEBP"])
def test_image_bytes(format: str, tmp_path: Path) -> None:
    output = BytesIO()
    Image.new("RGB", (2, 2)).save(output, format=format)
    content = output.getvalue()
    # The extension must not control validation or the response Content-Type.
    path = tmp_path / "wrong-extension.html"
    path.write_bytes(content)
    assert images.read_image(path) == (content, images.IMAGE_TYPES[format])
    with pytest.raises(ValueError):
        images.image_type(content[:16])


def test_limits(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    output = BytesIO()
    Image.new("RGB", (2, 2)).save(output, format="PNG")
    content = output.getvalue()
    path = tmp_path / "image.png"
    path.write_bytes(content)
    with monkeypatch.context() as patch:
        patch.setattr(images, "MAX_IMAGE_BYTES", len(content) - 1)
        with pytest.raises(ValueError, match="20 MiB"):
            images.read_image(path)
    monkeypatch.setattr(Image, "MAX_IMAGE_PIXELS", 1)
    with pytest.raises(ValueError, match="valid PNG"):
        images.read_image(path)


def test_non_files(tmp_path: Path) -> None:
    fifo = tmp_path / "pipe.png"
    os.mkfifo(fifo)
    for path in (fifo, tmp_path, tmp_path / "missing.png"):
        with pytest.raises(OSError):
            images.read_image(path)
