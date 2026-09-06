import base64
from collections.abc import AsyncIterator
from pathlib import Path
from uuid import uuid4

import pytest
from fastapi import FastAPI
from httpx import ASGITransport, AsyncClient
from sqlalchemy.ext.asyncio import AsyncSession

from aibo.core import workspace
from aibo.core.tests.fakes import FakeBouncer
from aibo.db.client import get_db
from aibo.db.models import LocationModel
from aibo.server.dependencies import get_bouncer
from aibo.server.routes.workspace_routes import router


@pytest.fixture
async def client(db: AsyncSession) -> AsyncIterator[tuple[AsyncClient, FakeBouncer]]:
    app = FastAPI()
    app.include_router(router)
    bouncer = FakeBouncer()

    async def test_db() -> AsyncIterator[AsyncSession]:
        yield db

    app.dependency_overrides[get_db] = test_db
    app.dependency_overrides[get_bouncer] = lambda: bouncer
    async with AsyncClient(
        transport=ASGITransport(app=app), base_url="http://test"
    ) as http:
        yield http, bouncer


@pytest.mark.parametrize("text", ["  Fix 猫.\nKeep this verbatim.\n", ""])
@pytest.mark.parametrize("by_source", [True, False])
async def test_handoff(
    db: AsyncSession,
    client: tuple[AsyncClient, FakeBouncer],
    tmp_path: Path,
    png: bytes,
    text: str,
    by_source: bool,
) -> None:
    http, bouncer = client
    location = LocationModel(name="work", path=str(tmp_path))
    db.add(location)
    manager = await workspace.create_chat(db, kind=workspace.ChatKind.MANAGER)
    await db.commit()
    local = tmp_path / "Clipboard image.png"
    local.write_bytes(png)
    uploaded = await http.post(
        "/api/attachments",
        json={
            "name": "web.png",
            "media_type": "image/png",
            "data": base64.b64encode(png).decode(),
        },
    )
    assert uploaded.status_code == 201
    paths = [str(local), uploaded.json()["path"]]
    original = await http.post(
        f"/api/chats/{manager.id}/submit", json={"text": text, "attachments": paths}
    )
    assert original.status_code == 200
    assert "source_message_id" in str(bouncer.turns[-1]["context"])
    assert f"/api/chats/{manager.id}" in str(bouncer.turns[-1]["context"])
    source = next(m for m in original.json()["chat"]["messages"] if m["kind"] == "user")
    # Pick the requested message, not whichever manager message happens to be latest.
    await http.post(
        f"/api/chats/{manager.id}/submit", json={"text": "An unrelated follow-up"}
    )
    content = (
        {"source_message_id": source["id"]}
        if by_source
        else {"user_message": text, "attachments": paths}
    )
    created = await http.post(
        "/api/chats",
        json={
            "kind": "bot",
            "location_id": str(location.id),
            "m_context": "Inspect the screenshots",
            **content,
        },
    )
    assert created.status_code == 201, created.text
    chat = created.json()
    user = chat["messages"][-1]
    expected = workspace.delegation_message(text, "Inspect the screenshots")
    assert user["content"] == expected
    assert user["data"]["user_message"] == text
    assert user["data"]["attachments"] == paths
    if by_source:
        assert user["data"]["source_message_id"] == source["id"]
    assert len(user["attachments"]) == 2
    for preview in user["attachments"]:
        assert user["id"] in preview["url"]
        assert (await http.get(preview["url"])).content == png
    endpoint = f"/api/chats/{chat['id']}"
    sent = await http.post(endpoint + "/submit", json={"message_id": user["id"]})
    assert sent.status_code == 200, sent.text
    assert bouncer.turns[-1]["text"] == expected
    assert bouncer.turns[-1]["attachments"] == paths
    assert sum(m["kind"] == "user" for m in sent.json()["chat"]["messages"]) == 1
    if text:
        assert sent.json()["chat"]["goal"]["objective"] == text
    if by_source:
        resumed = await http.post(
            endpoint + "/submit",
            json={"source_message_id": source["id"], "m_context": ""},
        )
        assert resumed.status_code == 200, resumed.text
        assert bouncer.turns[-1]["text"] == workspace.delegation_message(text, "")
        assert bouncer.turns[-1]["attachments"] == paths
        users = [m for m in resumed.json()["chat"]["messages"] if m["kind"] == "user"]
        assert len(users) == 2 and users[-1]["data"]["user_message"] == text
        assert users[-1]["data"]["attachments"] == paths
    stored = (await http.get(f"/api/chats/{manager.id}")).json()
    stored_source = next(m for m in stored["messages"] if m["id"] == source["id"])
    for field in ("content", "data", "attachments"):
        assert stored_source[field] == source[field]


async def test_invalid_handoff(
    db: AsyncSession, client: tuple[AsyncClient, FakeBouncer], tmp_path: Path
) -> None:
    http, bouncer = client
    location = LocationModel(name="work", path=str(tmp_path))
    db.add(location)
    await db.flush()
    bot = await workspace.create_chat(
        db, kind=workspace.ChatKind.BOT, location_id=location.id
    )
    manager = await workspace.create_chat(db, kind=workspace.ChatKind.MANAGER)
    shadow = await workspace.create_chat(
        db, kind=workspace.ChatKind.SHADOW, user_message="Private"
    )
    bad = await workspace.append_message(
        db,
        manager,
        kind=workspace.MessageKind.USER,
        content="Broken image",
        data={"attachments": [str(tmp_path / "missing.png")]},
    )
    await db.commit()
    before = (await http.get("/api/chats")).json()
    for source_id, code in (
        (uuid4(), 404),
        (manager.messages[0].id, 404),
        (shadow.messages[-1].id, 404),
        (bad.id, 422),
    ):
        body = {"source_message_id": str(source_id), "m_context": ""}
        endpoints: dict[str, dict[str, str]] = {
            "/api/chats": {"kind": "bot"},
            f"/api/chats/{bot.id}/submit": {},
        }
        for endpoint, fields in endpoints.items():
            response = await http.post(endpoint, json={**fields, **body})
            assert response.status_code == code, response.text
    create_overrides: list[dict[str, object]] = [
        {"user_message": "Rewritten"},
        {"attachments": ["/image.png"]},
        {"kind": "manager"},
        {"m_context": None},
    ]
    for extra in create_overrides:
        response = await http.post(
            "/api/chats",
            json={
                "kind": "bot",
                "source_message_id": str(bad.id),
                "m_context": "",
                **extra,
            },
        )
        assert response.status_code == 422
    submit_overrides: list[dict[str, object]] = [
        {"text": "Rewritten"},
        {"attachments": ["/image.png"]},
        {"message_id": str(bad.id)},
        {"m_context": None},
    ]
    for extra in submit_overrides:
        response = await http.post(
            f"/api/chats/{bot.id}/submit",
            json={"source_message_id": str(bad.id), "m_context": "", **extra},
        )
        assert response.status_code == 422
    response = await http.post(
        "/api/chats",
        json={
            "kind": "bot",
            "attachments": [str(tmp_path / "missing.png")],
            "m_context": "",
        },
    )
    assert response.status_code == 422
    assert (await http.get("/api/chats")).json() == before
    assert not bouncer.started and not bouncer.turns
