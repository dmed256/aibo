import asyncio
from collections.abc import AsyncIterator
from pathlib import Path
from typing import Any
from unittest.mock import AsyncMock

import pytest
from fastapi import FastAPI
from httpx import ASGITransport, AsyncClient
from sqlalchemy.ext.asyncio import AsyncSession, async_sessionmaker

from aibo.core import workspace
from aibo.core.settings import ModelSettings, save_models
from aibo.core.tests.fakes import FakeBouncer
from aibo.core.titles import clean_title
from aibo.db.client import get_db
from aibo.server.dependencies import get_bouncer
from aibo.server.events import EventHub
from aibo.server.routes.workspace_routes import router
from aibo.server.titles import TitleGenerator


@pytest.mark.parametrize("kind", ["manager", "bot"])
async def test_settings_and_submit(db: AsyncSession, tmp_path: Path, kind: str) -> None:
    app = FastAPI()
    app.include_router(router)
    bouncer: Any = FakeBouncer()
    bouncer.generate_title = AsyncMock(return_value="Fix the sidebar #emacs #ui")
    bouncer.close = AsyncMock()
    hub = EventHub()
    titles = TitleGenerator(
        bouncer, hub, async_sessionmaker(db.bind, expire_on_commit=False)
    )
    app.state.titles = titles
    app.state.event_hub = hub

    async def test_db() -> AsyncIterator[AsyncSession]:
        yield db

    app.dependency_overrides[get_db] = test_db
    app.dependency_overrides[get_bouncer] = lambda: bouncer
    try:
        async with AsyncClient(
            transport=ASGITransport(app=app), base_url="http://test"
        ) as client:
            models = (await client.get("/api/settings/models")).json()
            assert models == {
                "manager_model": None,
                "bot_model": None,
                "title_model": "gpt-5.6-luna",
                "manager_model_reasoning_effort": None,
                "bot_model_reasoning_effort": None,
                "title_model_reasoning_effort": "low",
            }
            models.update(
                manager_model="chosen-manager",
                bot_model="chosen-bot",
                manager_model_reasoning_effort="high",
                bot_model_reasoning_effort="medium",
            )
            assert (
                await client.put("/api/settings/models", json=models)
            ).json() == models
            assert (await client.get("/api/settings/models")).json() == models
            location = (
                await client.post(
                    "/api/locations", json={"name": "aibo", "path": str(tmp_path)}
                )
            ).json()
            created = (
                await client.post(
                    "/api/chats", json={"kind": kind, "location_id": location["id"]}
                )
            ).json()
            path = f"/api/chats/{created['id']}/submit"
            async with hub.subscribe() as events:
                response = await client.post(path, json={"text": "Fix the sidebar"})
                assert response.status_code == 200
                await asyncio.gather(*titles.tasks.values())
                updates = []
                while not events.empty():
                    updates.append(events.get_nowait())
                assert any(
                    event
                    == {
                        "kind": "chat_title_updated",
                        "chat_id": created["id"],
                        "title": "Fix the sidebar",
                    }
                    for event in updates
                )
            assert bouncer.started[-1]["model"] == f"chosen-{kind}"
            assert bouncer.turns[-1]["model"] == f"chosen-{kind}"
            assert (
                bouncer.turns[-1]["model_reasoning_effort"]
                == models[f"{kind}_model_reasoning_effort"]
            )
            bouncer.generate_title.assert_awaited_once_with(
                "Fix the sidebar", "gpt-5.6-luna", "low"
            )
            await client.put(
                "/api/settings/models",
                json={
                    **models,
                    f"{kind}_model": None,
                    f"{kind}_model_reasoning_effort": None,
                    "title_model": None,
                },
            )
            assert (
                await client.post(path, json={"text": "Continue"})
            ).status_code == 200
            await asyncio.gather(*titles.tasks.values())
            assert bouncer.turns[-1]["model"] is None
            assert bouncer.turns[-1]["model_reasoning_effort"] is None
            assert bouncer.generate_title.await_count == 1
    finally:
        await titles.close()


async def test_title_race(db: AsyncSession) -> None:
    chat = await workspace.create_chat(db, kind=workspace.ChatKind.MANAGER)
    await db.commit()
    await save_models(
        db, ModelSettings(title_model=None, title_model_reasoning_effort=None)
    )
    started, release = asyncio.Event(), asyncio.Event()

    async def generate(text: str, model: str | None, effort: str | None) -> str:
        assert text == "User request" and model is None and effort is None
        started.set()
        await release.wait()
        return '"Generated title #aibo #ui"'

    client = AsyncMock()
    client.generate_title.side_effect = generate
    hub = EventHub()
    titles = TitleGenerator(
        client, hub, async_sessionmaker(db.bind, expire_on_commit=False)
    )
    async with hub.subscribe() as events:
        titles.schedule(chat.id, "User request")
        titles.schedule(chat.id, "Duplicate")
        await started.wait()
        assert len(titles.tasks) == 1
        chat.title = "Manually named"
        await db.commit()
        release.set()
        await asyncio.gather(*titles.tasks.values())
        await db.refresh(chat)
        assert chat.title == "Manually named"
        assert events.empty()
        await titles.generate(chat.id, "User request")
        assert client.generate_title.await_count == 1
    await titles.close()


async def test_title_failure_can_retry(db: AsyncSession) -> None:
    chat = await workspace.create_chat(db, kind=workspace.ChatKind.MANAGER)
    await db.commit()
    client = AsyncMock()
    client.generate_title.side_effect = [
        RuntimeError("Disconnected"),
        "  `Small\n title #aibo #ui`  ",
    ]
    titles = TitleGenerator(
        client, EventHub(), async_sessionmaker(db.bind, expire_on_commit=False)
    )
    for _ in range(2):
        titles.schedule(chat.id, "User request")
        await asyncio.gather(*titles.tasks.values())
    await db.refresh(chat)
    assert chat.title == "Small title"
    assert chat.status == "idle"
    await titles.close()


def test_title_keeps_csharp() -> None:
    assert clean_title("Fix C# parsing #csharp #parser") == "Fix C# parsing"
    assert clean_title("Learn to use C#") == "Learn to use C#"
