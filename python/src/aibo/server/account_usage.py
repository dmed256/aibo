"""Project Codex account limits into the sidebar's two badges."""

import math
from typing import Any

from aibo.server.schemas import AccountUsage


def account_usage(snapshot: dict[str, Any]) -> AccountUsage:
    buckets = snapshot.get("rateLimitsByLimitId") or {}
    bucket = buckets.get("codex") or snapshot.get("rateLimits") or {}
    remaining = []
    for key in ("primary", "secondary"):
        used = (bucket.get(key) or {}).get("usedPercent")
        if (
            isinstance(used, (int, float))
            and not isinstance(used, bool)
            and math.isfinite(used)
        ):
            remaining.append(max(0, min(100, math.floor(100 - used))))
    resets = (snapshot.get("rateLimitResetCredits") or {}).get("availableCount")
    return AccountUsage(
        remaining_percent=min(remaining) if remaining else None,
        resets=resets if type(resets) is int and resets >= 0 else None,
    )
