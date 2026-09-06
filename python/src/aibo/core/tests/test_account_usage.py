from unittest.mock import AsyncMock

import pytest
from httpx import ASGITransport, AsyncClient
from sqlalchemy.ext.asyncio import AsyncSession

from aibo.bouncer.client import BouncerClient, BouncerError
from aibo.bouncer.codex import AppServerError, CodexAppServer
from aibo.bouncer.main import create_app
from aibo.server.account_usage import account_usage
from aibo.server.events import BouncerEvents, EventHub
from aibo.server.routes.notifications import get_sidebar


def test_remaining_and_earned_resets() -> None:
    snapshot = {
        "rateLimits": {"primary": {"usedPercent": 99}},
        "rateLimitsByLimitId": {
            "codex": {
                "primary": {"usedPercent": 25},
                "secondary": {"usedPercent": 81.2},
            },
            "other": {"primary": {"usedPercent": 100}},
        },
        "rateLimitResetCredits": {"availableCount": 3, "credits": []},
    }
    assert account_usage(snapshot).model_dump() == {
        "remaining_percent": 18,
        "resets": 3,
    }
    snapshot["rateLimitResetCredits"] = {"availableCount": 0, "credits": None}
    assert account_usage(snapshot).resets == 0


@pytest.mark.parametrize(
    ("used", "remaining"),
    [(0, 100), (100, 0), (110, 0), (-5, 100), (None, None), (True, None)],
)
def test_legacy_window(used: object, remaining: int | None) -> None:
    usage = account_usage({"rateLimits": {"primary": {"usedPercent": used}}})
    assert usage.remaining_percent == remaining
    assert usage.resets is None
    assert account_usage({}).remaining_percent is None


async def test_limits_cache(monkeypatch: pytest.MonkeyPatch) -> None:
    codex = CodexAppServer("unused")
    calls = AsyncMock(return_value={"rateLimitResetCredits": {"availableCount": 2}})
    monkeypatch.setattr(codex, "call", calls)
    clock = 10.0
    monkeypatch.setattr("aibo.bouncer.codex.time.monotonic", lambda: clock)
    app = create_app(codex)
    async with AsyncClient(
        transport=ASGITransport(app=app), base_url="http://test"
    ) as client:
        for _ in range(2):
            assert (
                await client.get("/account/rate-limits")
            ).json() == calls.return_value
        calls.assert_awaited_once_with("account/rateLimits/read", timeout=3)
        clock += 31
        await client.get("/account/rate-limits")
        assert calls.await_count == 2
        for method in ("account/rateLimits/updated", "account/updated"):
            codex._publish_event({"method": method, "params": {}})
            await client.get("/account/rate-limits")
        assert calls.await_count == 4
        calls.side_effect = AppServerError("Unsupported authentication")
        codex._publish_event({"method": "account/updated"})
        assert (await client.get("/account/rate-limits")).json() == {}
        assert (await client.get("/account/rate-limits")).json() == {}
        assert calls.await_count == 5


async def test_sidebar_survives_unavailable_limits(
    db: AsyncSession, monkeypatch: pytest.MonkeyPatch
) -> None:
    bouncer = BouncerClient("http://unused")
    limits = AsyncMock(side_effect=BouncerError("Unavailable"))
    monkeypatch.setattr(bouncer, "rate_limits", limits)
    try:
        sidebar = await get_sidebar(db, bouncer)
        assert sidebar.account_usage.model_dump() == {
            "remaining_percent": None,
            "resets": None,
        }
        limits.side_effect = None
        limits.return_value = {
            "rateLimits": {"primary": {"usedPercent": 25}},
            "rateLimitResetCredits": {"availableCount": 2},
        }
        assert (await get_sidebar(db, bouncer)).model_dump()["account_usage"] == {
            "remaining_percent": 75,
            "resets": 2,
        }
    finally:
        await bouncer.close()


async def test_account_events_reach_clients() -> None:
    events = BouncerEvents("http://unused", EventHub())
    try:
        event = await events._persist(
            {"method": "account/rateLimits/updated", "params": {"rateLimits": {}}}
        )
        assert event and event["method"] == "account/rateLimits/updated"
    finally:
        await events.client.close()
