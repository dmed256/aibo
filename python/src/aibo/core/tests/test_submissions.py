from collections.abc import AsyncIterator
from pathlib import Path
from typing import Any

import pytest
from fastapi import FastAPI
from httpx import ASGITransport, AsyncClient
from sqlalchemy.ext.asyncio import AsyncSession

from aibo.bouncer.client import BouncerError
from aibo.common.runtime_files import role_instructions
from aibo.core import workspace
from aibo.core.tests.fakes import FakeBouncer
from aibo.db.client import get_db
from aibo.db.models import LocationModel
from aibo.server.dependencies import get_bouncer
from aibo.server.routes.workspace_routes import router


@pytest.mark.parametrize("image_only", [False, True])
async def test_submit_and_move_chat(
    db: AsyncSession, tmp_path: Path, image_only: bool, png: bytes
) -> None:
    path = tmp_path / "layout.png"
    path.write_bytes(png)
    text = "" if image_only else "Inspect the tests"
    attachments = [str(path)] if image_only else []
    location = LocationModel(name="aibo", path=str(tmp_path))
    destination_path = tmp_path / "other"
    destination_path.mkdir()
    destination = LocationModel(name="other", path=str(destination_path))
    db.add_all([location, destination])
    await db.commit()
    bouncer = FakeBouncer()
    app = FastAPI()
    app.include_router(router)

    async def test_db() -> AsyncIterator[AsyncSession]:
        yield db

    async def test_bouncer() -> AsyncIterator[FakeBouncer]:
        yield bouncer

    app.dependency_overrides[get_db] = test_db
    app.dependency_overrides[get_bouncer] = test_bouncer
    async with AsyncClient(
        transport=ASGITransport(app=app), base_url="http://test"
    ) as client:
        created = await client.post(
            "/api/chats",
            json={"kind": "bot", "location_id": str(location.id)},
        )
        response = await client.post(
            f"/api/chats/{created.json()['id']}/submit",
            json={"text": text, "attachments": attachments},
        )
        moved = await client.patch(
            f"/api/chats/{created.json()['id']}/location",
            json={"location_id": str(destination.id)},
        )
        resumed = await client.post(
            f"/api/chats/{created.json()['id']}/submit",
            json={"text": "Continue in the new directory"},
        )

    assert response.status_code == 200
    assert response.json()["turn_id"] == "turn-1"
    assert response.json()["chat"]["status"] == "running"
    assert response.json()["chat"]["codex_thread_id"] == "thread-1"
    assert bouncer.turns[0]["text"] == text
    assert bouncer.turns[0]["attachments"] == attachments
    assert bouncer.started[0]["cwd"] == str(tmp_path)
    assert bouncer.started[0]["instructions"] == role_instructions("bot")
    assert moved.status_code == 200
    assert moved.json()["status"] == "interrupted"
    assert moved.json()["messages"][-1]["content"] == (
        f"Moved to the other location ({destination_path})"
    )
    assert moved.json()["notice"] == moved.json()["messages"][-1]["content"]
    assert bouncer.interrupted == [("thread-1", "turn-1")]
    assert resumed.status_code == 200 and resumed.json()["chat"]["notice"] is None
    assert bouncer.resumed == ["thread-1"]
    assert [turn["cwd"] for turn in bouncer.turns] == [
        str(tmp_path),
        str(destination_path),
    ]
    assert len(bouncer.started) == 1


async def test_blocked_retry(
    db: AsyncSession, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    directory = tmp_path / "later"
    location = LocationModel(name="later", path=str(directory))
    db.add(location)
    await db.commit()
    bouncer = FakeBouncer()
    app = FastAPI()
    app.include_router(router)

    async def test_db() -> AsyncIterator[AsyncSession]:
        yield db

    app.dependency_overrides[get_db] = test_db
    app.dependency_overrides[get_bouncer] = lambda: bouncer
    async with AsyncClient(
        transport=ASGITransport(app=app), base_url="http://test"
    ) as client:
        created = await client.post(
            "/api/chats", json={"kind": "bot", "location_id": str(location.id)}
        )
        path = f"/api/chats/{created.json()['id']}"
        for _ in range(2):
            response = await client.post(
                path + "/submit", json={"text": "Keep this request"}
            )
            assert response.status_code == 422
        blocked = (await client.get(path)).json()
        notice = "Blocked: the selected working directory does not exist."
        assert blocked["status"] == "blocked" and not blocked["active"]
        assert blocked["notice"] == notice
        assert blocked["notice_message_id"] == blocked["messages"][-1]["id"]
        assert not any(message["kind"] == "user" for message in blocked["messages"])
        assert not bouncer.started and not bouncer.turns
        unread = (await client.get("/api/notifications")).json()["unread"]
        assert [item["body"] for item in unread] == [notice]

        directory.mkdir()
        sent = await client.post(path + "/submit", json={"text": "Keep this request"})
        assert sent.status_code == 200
        assert sent.json()["chat"]["notice"] is None
        assert [turn["text"] for turn in bouncer.turns] == ["Keep this request"]
        for status, reason in [
            ("cancelled", "Cancelled before the next operation."),
            (
                "interrupted",
                "Interrupted by the user. The conversation can be continued.",
            ),
            ("blocked", "Blocked: waiting for the build prerequisite."),
        ]:
            body = {"status": status}
            if status == "blocked":
                body["reason"] = reason
            for _ in range(2):
                assert (
                    await client.patch(path + "/status", json=body)
                ).status_code == 200
            updated = (await client.get(path)).json()
            assert updated["notice"] == reason
            assert updated["notice_message_id"] == updated["messages"][-1]["id"]
        assert len((await client.get("/api/notifications")).json()["unread"]) == 4

        async def fail_turn(**_kwargs: object) -> dict[str, Any]:
            raise BouncerError("app-server disconnected")

        monkeypatch.setattr(bouncer, "start_turn", fail_turn)
        failed = await client.post(path + "/submit", json={"text": "Retry the work"})
        assert failed.status_code == 502
        persisted = (await client.get(path)).json()
        assert persisted["status"] == "error"
        assert (
            persisted["notice"]
            == "The turn could not continue: app-server disconnected."
        )
        assert persisted["notice_message_id"] == persisted["messages"][-1]["id"]
        unread = (await client.get("/api/notifications")).json()["unread"]
        assert unread[0]["body"] == persisted["notice"]


def test_empty_submission_requires_image() -> None:
    from aibo.bouncer.main import StartTurn
    from aibo.server.schemas import SubmitMessage

    models: tuple[type[StartTurn] | type[SubmitMessage], ...] = (
        StartTurn,
        SubmitMessage,
    )
    for model in models:
        with pytest.raises(ValueError, match="Provide text"):
            model(text="  ")
        assert model(text="", attachments=["/tmp/layout.png"]).text == ""


@pytest.mark.parametrize("prior_turn", [False, True])
async def test_missing_rollout(
    db: AsyncSession, tmp_path: Path, prior_turn: bool
) -> None:
    class OlderBouncer(FakeBouncer):
        async def resume_thread(self, thread_id: str) -> dict[str, Any]:
            raise BouncerError(
                f"thread/resume: no rollout found for thread id {thread_id}",
                status_code=502,
            )

        async def set_goal(
            self, thread_id: str, objective: str | None
        ) -> dict[str, Any]:
            raise BouncerError("Not Found", status_code=404)

    location = LocationModel(name="local", path=str(tmp_path))
    db.add(location)
    await db.flush()
    chat = await workspace.create_chat(
        db, kind=workspace.ChatKind.BOT, location_id=location.id
    )
    chat.codex_thread_id = "empty-thread"
    if prior_turn:
        await workspace.append_message(
            db,
            chat,
            kind=workspace.MessageKind.EVENT,
            content="Codex turn started",
            data={"event": "turn_started"},
        )
    await db.commit()
    bouncer = OlderBouncer()
    app = FastAPI()
    app.include_router(router)

    async def test_db() -> AsyncIterator[AsyncSession]:
        yield db

    app.dependency_overrides[get_db] = test_db
    app.dependency_overrides[get_bouncer] = lambda: bouncer
    async with AsyncClient(
        transport=ASGITransport(app=app), base_url="http://test"
    ) as client:
        path = f"/api/chats/{chat.id}"
        result = await client.post(path + "/submit", json={"text": "Repair the UI"})
        persisted = (await client.get(path)).json()
        if prior_turn:
            assert result.status_code == 502
            assert not bouncer.started and not bouncer.turns
            assert persisted["messages"][-1]["kind"] == "error"
            assert "no rollout found" in persisted["messages"][-1]["content"]
        else:
            assert result.status_code == 200
            assert len(bouncer.started) == 1 and len(bouncer.turns) == 1
            assert persisted["codex_thread_id"] == "thread-1"
            assert persisted["goal"] == {
                "objective": "Repair the UI",
                "status": "unavailable",
            }
