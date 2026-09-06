import base64
import re
from collections.abc import AsyncIterator
from pathlib import Path

import pytest
from fastapi import FastAPI
from httpx import ASGITransport, AsyncClient
from sqlalchemy.ext.asyncio import AsyncSession

import aibo.server.main as server_main
from aibo.common.constants import Env
from aibo.core import workspace
from aibo.core.tests.fakes import FakeBouncer
from aibo.db.client import get_db
from aibo.server.dependencies import get_bouncer
from aibo.server.routes.attachments import image_response
from aibo.server.routes.workspace_routes import router


async def test_reject_non_images(db: AsyncSession, tmp_path: Path) -> None:
    app = FastAPI()
    app.include_router(router)
    bouncer = FakeBouncer()

    async def test_db() -> AsyncIterator[AsyncSession]:
        yield db

    app.dependency_overrides[get_db] = test_db
    app.dependency_overrides[get_bouncer] = lambda: bouncer
    path = tmp_path / "looks-like-image.png"
    path.write_text("SYNTHETIC NOT AN IMAGE")
    assets = Env.get().assets_dir
    assets.mkdir(parents=True)
    uploaded_path = assets / "old.png"
    uploaded_path.write_bytes(path.read_bytes())
    chat = await workspace.create_chat(db, kind=workspace.ChatKind.MANAGER)
    # Legacy stored paths also need validation at retrieval and resubmission.
    message = await workspace.append_message(
        db,
        chat,
        kind=workspace.MessageKind.USER,
        content="Inspect",
        data={"attachments": [str(path)]},
    )
    await db.commit()
    before = len(chat.messages)
    async with AsyncClient(
        transport=ASGITransport(app=app), base_url="http://test"
    ) as client:
        for url in (
            "/api/attachments/old.png",
            f"/api/messages/{message.id}/attachments/0",
        ):
            response = await client.get(url)
            assert response.status_code == 422
            assert "SYNTHETIC" not in response.text
        for body in (
            {"text": "Inspect", "attachments": [str(path)]},
            {"message_id": str(message.id)},
        ):
            response = await client.post(f"/api/chats/{chat.id}/submit", json=body)
            assert response.status_code == 422
        response = await client.post(
            f"/api/chats/{chat.id}/messages",
            json={
                "kind": "user",
                "content": "Inspect",
                "data": {"attachments": [str(path)]},
            },
        )
        assert response.status_code == 422
    assert len(chat.messages) == before
    assert not bouncer.started and not bouncer.turns


async def test_upload_validation(db: AsyncSession, png: bytes) -> None:
    app = FastAPI()
    app.include_router(router)
    async with AsyncClient(
        transport=ASGITransport(app=app), base_url="http://test"
    ) as client:
        for content, media_type in (
            (b"not an image", "image/png"),
            (b"<svg xmlns='http://www.w3.org/2000/svg'></svg>", "image/png"),
            (png[:32], "image/png"),
            (png, "image/jpeg"),
        ):
            response = await client.post(
                "/api/attachments",
                json={
                    "name": "image.png",
                    "media_type": media_type,
                    "data": base64.b64encode(content).decode(),
                },
            )
            assert response.status_code == 422
    assert not Env.get().assets_dir.exists()


def test_response_snapshot(tmp_path: Path, png: bytes) -> None:
    path = tmp_path / "wrong.html"
    path.write_bytes(png)
    response = image_response(path)
    path.write_text("SYNTHETIC REPLACEMENT")
    assert response.body == png
    assert response.headers["content-type"] == "image/png"


async def test_message_images(db: AsyncSession, tmp_path: Path, png: bytes) -> None:
    app = FastAPI()
    app.include_router(router)

    async def test_db() -> AsyncIterator[AsyncSession]:
        yield db

    app.dependency_overrides[get_db] = test_db
    image = tmp_path / "Emacs clipboard.png"
    image.write_bytes(png)
    chat = await workspace.create_chat(db, kind=workspace.ChatKind.MANAGER)
    message = await workspace.append_message(
        db,
        chat,
        kind=workspace.MessageKind.USER,
        content="",
        data={"attachments": [str(image)]},
    )
    shadow = await workspace.create_chat(db, kind=workspace.ChatKind.SHADOW)
    private = await workspace.append_message(
        db,
        shadow,
        kind=workspace.MessageKind.USER,
        content="",
        data={"attachments": [str(image)]},
    )
    await db.commit()
    async with AsyncClient(
        transport=ASGITransport(app=app), base_url="http://test"
    ) as client:
        response = (await client.get(f"/api/chats/{chat.id}")).json()
        saved = next(
            item for item in response["messages"] if item["id"] == str(message.id)
        )
        assert saved["data"]["attachments"] == [str(image)]
        preview = saved["attachments"][0]
        assert preview["name"] == image.name
        assert preview["size"] == len(png)
        downloaded = await client.get(preview["url"])
        assert downloaded.content == png
        assert downloaded.headers["content-type"] == "image/png"
        assert downloaded.headers["x-content-type-options"] == "nosniff"
        for url in (
            f"/api/messages/{message.id}/attachments/-1",
            f"/api/messages/{message.id}/attachments/1",
            f"/api/messages/{private.id}/attachments/0",
        ):
            assert (await client.get(url)).status_code == 404
        image.unlink()
        assert (await client.get(preview["url"])).status_code == 404
        assert (await client.get(f"/api/chats/{chat.id}")).status_code == 200


async def test_web_assets_and_attachment_upload(
    db: AsyncSession, monkeypatch: pytest.MonkeyPatch, png: bytes
) -> None:
    app = FastAPI()
    app.include_router(router)

    async def test_db() -> AsyncIterator[AsyncSession]:
        yield db

    app.dependency_overrides[get_db] = test_db
    async with AsyncClient(
        transport=ASGITransport(app=app), base_url="http://test"
    ) as client:
        uploaded = await client.post(
            "/api/attachments",
            json={
                "name": "clipboard.png",
                "media_type": "image/png",
                "data": base64.b64encode(png).decode(),
            },
        )
        attachment = uploaded.json()
        downloaded = await client.get(attachment["url"])

    assert uploaded.status_code == 201
    assert attachment["name"] == "clipboard.png"
    assert attachment["size"] == len(png)
    assert downloaded.content == png

    monkeypatch.setattr(server_main, "migrate_db", lambda: None)
    web_app = server_main.create_app()
    async with AsyncClient(
        transport=ASGITransport(app=web_app), base_url="http://test"
    ) as client:
        page = await client.get("/")
        tutorial = await client.get("/tutorial")
        assert tutorial.status_code == 200 and tutorial.content == page.content
        for path in (
            "/chats/chat-id",
            "/chats?q=sidebar",
            "/projects/project-id",
            "/locations",
            "/help",
            "/customization",
        ):
            linked = await client.get(path)
            assert linked.status_code == 200 and linked.content == page.content
            assert "default-src 'self'" in linked.headers["content-security-policy"]
        assert (await client.get("/api/missing-route")).status_code == 404
        assert (await client.get("/assets/missing.js")).status_code == 404
        script_path = re.search(r'src="(/assets/[^"]+\.js)"', page.text)
        assert script_path
        script = await client.get(script_path.group(1))
        style_path = re.search(r'href="(/assets/[^"]+\.css)"', page.text)
        assert style_path
        style = await client.get(style_path.group(1))

    assert page.status_code == 200
    assert "<title>Aibo</title>" in page.text
    assert "default-src 'self'" in page.headers["content-security-policy"]
    assert script.status_code == 200
    assert "javascript" in script.headers["content-type"]
    assert (
        script.content
        == (Path(server_main.WEB_DIR) / script_path.group(1).lstrip("/")).read_bytes()
    )
    assert style.status_code == 200
    assert "text/css" in style.headers["content-type"]
    assert (
        style.content
        == (Path(server_main.WEB_DIR) / style_path.group(1).lstrip("/")).read_bytes()
    )
