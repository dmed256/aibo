"""Read the installed Codex catalog without touching active bouncer sessions."""

import asyncio
import time

from pydantic import BaseModel

from aibo.bouncer.codex import CodexAppServer
from aibo.common.constants import Env


class CodexModel(BaseModel):
    model: str
    display_name: str


class ModelCatalog:
    def __init__(self) -> None:
        self._lock = asyncio.Lock()
        self._expires = 0.0
        self._models: list[CodexModel] = []

    async def read(self) -> list[CodexModel]:
        async with self._lock:
            if time.monotonic() < self._expires:
                return self._models
            codex = CodexAppServer(Env.get().CODEX_COMMAND)
            models: dict[str, CodexModel] = {}
            cursor = None
            seen: set[str] = set()
            try:
                async with asyncio.timeout(20):
                    await codex.start()
                    while True:
                        page = await codex.call(
                            "model/list", {"limit": 100, "cursor": cursor}
                        )
                        for item in page["data"]:
                            if not item.get("hidden", False):
                                model = CodexModel(
                                    model=item["model"],
                                    display_name=item["displayName"],
                                )
                                models[model.model] = model
                        cursor = page.get("nextCursor")
                        if not cursor:
                            break
                        if cursor in seen:
                            raise ValueError("Codex repeated a model catalog cursor")
                        seen.add(cursor)
            finally:
                await codex.shutdown(timeout=2)
            self._models = list(models.values())
            self._expires = time.monotonic() + 300
            return self._models
