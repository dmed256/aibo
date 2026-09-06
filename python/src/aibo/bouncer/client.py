from __future__ import annotations

from typing import Any, cast

import httpx


class BouncerError(RuntimeError):
    def __init__(self, message: str, *, status_code: int | None = None) -> None:
        super().__init__(message)
        self.status_code = status_code


class BouncerClient:
    def __init__(self, base_url: str, timeout: float = 30.0) -> None:
        self._client = httpx.AsyncClient(base_url=base_url, timeout=timeout)

    async def close(self) -> None:
        await self._client.aclose()

    async def health(self) -> bool:
        try:
            response = await self._client.get("/health")
            return cast(bool, response.status_code == 200)
        except httpx.HTTPError:
            return False

    async def rate_limits(self) -> dict[str, Any]:
        return await self._request("GET", "/account/rate-limits", timeout=4)

    async def start_thread(
        self, *, cwd: str, instructions: str, model: str | None = None
    ) -> dict[str, Any]:
        body = {"cwd": cwd, "instructions": instructions, "model": model}
        return await self._request("POST", "/threads", json=body)

    async def resume_thread(self, thread_id: str) -> dict[str, Any]:
        return await self._request("POST", f"/threads/{thread_id}/resume")

    async def get_goal(self, thread_id: str) -> dict[str, Any]:
        return await self._request("GET", f"/threads/{thread_id}/goal")

    async def set_goal(self, thread_id: str, objective: str | None) -> dict[str, Any]:
        return await self._request(
            "PUT", f"/threads/{thread_id}/goal", json={"objective": objective}
        )

    async def start_turn(
        self,
        *,
        thread_id: str,
        text: str,
        attachments: list[str],
        cwd: str | None = None,
        context: str | None = None,
        model: str | None = None,
        model_reasoning_effort: str | None = None,
    ) -> dict[str, Any]:
        return await self._request(
            "POST",
            f"/threads/{thread_id}/turns",
            json={
                "text": text,
                "attachments": attachments,
                "cwd": cwd,
                "context": context,
                "model": model,
                "model_reasoning_effort": model_reasoning_effort,
            },
        )

    async def generate_title(
        self, text: str, model: str | None, model_reasoning_effort: str | None = None
    ) -> str:
        response = await self._request(
            "POST",
            "/titles",
            json={
                "text": text,
                "model": model,
                "model_reasoning_effort": model_reasoning_effort,
            },
        )
        return str(response["title"])

    async def interrupt(self, *, thread_id: str, turn_id: str) -> None:
        await self._request("POST", f"/threads/{thread_id}/turns/{turn_id}/interrupt")

    async def _request(self, method: str, path: str, **kwargs: Any) -> dict[str, Any]:
        try:
            response = await self._client.request(method, path, **kwargs)
            response.raise_for_status()
        except httpx.HTTPStatusError as error:
            try:
                detail = error.response.json().get("detail")
            except (ValueError, AttributeError):
                detail = None
            raise BouncerError(
                f"bouncer {method} {path}: {detail or error.response.reason_phrase}",
                status_code=error.response.status_code,
            ) from error
        except httpx.HTTPError as error:
            raise BouncerError(f"bouncer request failed: {error}") from error
        return dict(response.json())
