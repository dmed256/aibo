from __future__ import annotations

from typing import Any

__all__ = ["JsonValue"]

JsonValue = None | int | float | str | bool | list[Any] | dict[str, Any]
