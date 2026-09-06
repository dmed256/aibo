import asyncio
from typing import Any

import pytest

from aibo.core import model_catalog


class FakeCodex:
    def __init__(self) -> None:
        self.starts = 0
        self.stops = 0
        self.calls: list[dict[str, Any]] = []
        self.fail = False

    async def start(self) -> None:
        self.starts += 1
        await asyncio.sleep(0)
        if self.fail:
            raise TimeoutError()

    async def shutdown(self, timeout: float) -> None:
        self.stops += 1

    async def call(self, method: str, params: dict[str, Any]) -> dict[str, Any]:
        assert method == "model/list"
        self.calls.append(params)
        if params["cursor"] is None:
            return {
                "data": [{"model": "one", "displayName": "One"}],
                "nextCursor": "next",
            }
        return {
            "data": [
                {"model": "two", "displayName": "Two"},
                {"model": "hidden", "displayName": "Hidden", "hidden": True},
            ],
            "nextCursor": None,
        }


async def test_catalog_pages_and_cache(monkeypatch: pytest.MonkeyPatch) -> None:
    codex = FakeCodex()
    monkeypatch.setattr(model_catalog, "CodexAppServer", lambda _: codex)
    catalog = model_catalog.ModelCatalog()
    first, second = await asyncio.gather(catalog.read(), catalog.read())
    assert first == second
    assert [item.model for item in first] == ["one", "two"]
    assert codex.starts == codex.stops == 1
    assert [call["cursor"] for call in codex.calls] == [None, "next"]


async def test_catalog_failure_can_retry(monkeypatch: pytest.MonkeyPatch) -> None:
    codex = FakeCodex()
    codex.fail = True
    monkeypatch.setattr(model_catalog, "CodexAppServer", lambda _: codex)
    catalog = model_catalog.ModelCatalog()
    with pytest.raises(TimeoutError):
        await catalog.read()
    assert codex.stops == 1
    codex.fail = False
    assert len(await catalog.read()) == 2
    assert codex.starts == codex.stops == 2


async def test_catalog_http_failure(monkeypatch: pytest.MonkeyPatch) -> None:
    from fastapi import FastAPI
    from httpx import ASGITransport, AsyncClient

    from aibo.server.routes import settings

    codex = FakeCodex()
    codex.fail = True
    monkeypatch.setattr(model_catalog, "CodexAppServer", lambda _: codex)
    monkeypatch.setattr(settings, "_catalog", model_catalog.ModelCatalog())
    app = FastAPI()
    app.include_router(settings.router, prefix="/api")
    async with AsyncClient(
        transport=ASGITransport(app), base_url="http://test"
    ) as client:
        response = await client.get("/api/settings/models/catalog")
        assert response.status_code == 503
        assert response.json() == {"detail": "Could not load Codex models. Try again."}
        codex.fail = False
        response = await client.get("/api/settings/models/catalog")
        assert response.status_code == 200
        assert response.json() == [
            {"model": "one", "display_name": "One"},
            {"model": "two", "display_name": "Two"},
        ]
