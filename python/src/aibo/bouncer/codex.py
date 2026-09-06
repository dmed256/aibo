from __future__ import annotations

import asyncio
import json
import logging
import resource
import shlex
import time
from collections.abc import AsyncIterator
from contextlib import asynccontextmanager, suppress
from pathlib import Path
from typing import Any, cast
from weakref import WeakValueDictionary

logger = logging.getLogger(__name__)


class AppServerError(RuntimeError):
    pass


def raise_file_limit() -> None:
    """Give the persistent Codex process room for session files, pipes and sockets."""
    soft, hard = resource.getrlimit(resource.RLIMIT_NOFILE)
    target = 8192 if hard == resource.RLIM_INFINITY else min(8192, hard)
    if soft == resource.RLIM_INFINITY or soft >= target:
        return
    try:
        resource.setrlimit(resource.RLIMIT_NOFILE, (target, hard))
    except (OSError, ValueError):
        logger.warning(
            "Could not raise open-file limit from %d to %d", soft, target, exc_info=True
        )
    else:
        logger.info("Raised open-file limit from %d to %d", soft, target)


async def read_lines(stream: asyncio.StreamReader) -> AsyncIterator[bytes]:
    """Read newline-delimited Codex frames without StreamReader's line-size limit."""
    pending = bytearray()
    while chunk := await stream.read(64 * 1024):
        lines = chunk.split(b"\n")
        pending.extend(lines[0])
        if len(lines) == 1:
            continue
        yield bytes(pending)
        pending.clear()
        for line in lines[1:-1]:
            yield line
        pending.extend(lines[-1])
    if pending:
        yield bytes(pending)


class CodexAppServer:
    def __init__(self, command: str) -> None:
        self.command = shlex.split(command)
        self.process: asyncio.subprocess.Process | None = None
        self._next_id = 0
        self._pending: dict[int, asyncio.Future[dict[str, Any]]] = {}
        self._subscribers: set[asyncio.Queue[dict[str, Any]]] = set()
        self._write_lock = asyncio.Lock()
        self._reader_task: asyncio.Task[None] | None = None
        self._stderr_task: asyncio.Task[None] | None = None
        self._cleanup_task: asyncio.Task[None] | None = None
        self._thread_activity: dict[str, float] = {}
        self._thread_locks: WeakValueDictionary[
            str, asyncio.Lock
        ] = WeakValueDictionary()
        self.initialized = False
        self._rate_limits: dict[str, Any] = {}
        self._rate_limits_until = 0.0
        self._rate_limits_revision = 0
        self._rate_limits_lock = asyncio.Lock()

    @property
    def ready(self) -> bool:
        return bool(
            self.initialized
            and self.process
            and self.process.returncode is None
            and self._reader_task
            and not self._reader_task.done()
        )

    async def start(self) -> None:
        if self.process and self.process.returncode is None:
            return
        # launchd/Emacs may supply a soft limit of only 256. Raise it before
        # spawning so Codex inherits it, regardless of the service entry point.
        raise_file_limit()
        self.process = await asyncio.create_subprocess_exec(
            *self.command,
            stdin=asyncio.subprocess.PIPE,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
        )
        self._reader_task = asyncio.create_task(self._read_messages())
        self._stderr_task = asyncio.create_task(self._read_stderr())
        await self.call(
            "initialize",
            {
                "clientInfo": {
                    "name": "aibo",
                    "title": "Aibo",
                    "version": "2.0.0",
                },
                "capabilities": {"experimentalApi": True},
            },
        )
        await self.notify("initialized")
        self.initialized = True
        self._rate_limits_until = 0.0
        self._cleanup_task = asyncio.create_task(self._clean_idle_threads())

    async def rate_limits(self) -> dict[str, Any]:
        """Share a short-lived account snapshot across sidebar readers."""
        async with self._rate_limits_lock:
            if time.monotonic() < self._rate_limits_until:
                return self._rate_limits
            revision = self._rate_limits_revision
            try:
                result = await self.call("account/rateLimits/read", timeout=3)
            except (AppServerError, TimeoutError):
                # Older servers and non-ChatGPT auth may not expose account limits.
                result = {}
            self._rate_limits = result
            self._rate_limits_until = (
                time.monotonic() + 30 if revision == self._rate_limits_revision else 0.0
            )
            return result

    async def shutdown(self, timeout: float = 15.0) -> None:
        self.initialized = False
        if self._cleanup_task:
            self._cleanup_task.cancel()
            with suppress(asyncio.CancelledError):
                await self._cleanup_task
        self._thread_activity.clear()
        if not self.process or self.process.returncode is not None:
            return
        self.process.terminate()
        try:
            await asyncio.wait_for(self.process.wait(), timeout)
        except TimeoutError:
            self.process.kill()
            await self.process.wait()
        if self._reader_task:
            await self._reader_task
        if self._stderr_task:
            await self._stderr_task

    async def call(
        self, method: str, params: dict[str, Any] | None = None, timeout: float = 30.0
    ) -> dict[str, Any]:
        thread_id = (params or {}).get("threadId")
        if thread_id:
            async with self._thread_lock(thread_id):
                if (
                    method in {"turn/start", "thread/goal/set"}
                    and thread_id not in self._thread_activity
                ):
                    await self._call("thread/resume", {"threadId": thread_id}, timeout)
                    self._thread_activity[thread_id] = time.monotonic()
                if thread_id in self._thread_activity:
                    self._thread_activity[thread_id] = time.monotonic()
                result = await self._call(method, params, timeout)
                if method == "thread/resume":
                    self._thread_activity[thread_id] = time.monotonic()
                elif method == "thread/unsubscribe":
                    self._thread_activity.pop(thread_id, None)
                return result
        result = await self._call(method, params, timeout)
        if method == "thread/start":
            self._thread_activity[result["thread"]["id"]] = time.monotonic()
        return result

    def _thread_lock(self, thread_id: str) -> asyncio.Lock:
        return self._thread_locks.setdefault(thread_id, asyncio.Lock())

    async def _clean_idle_threads(self) -> None:
        while True:
            await asyncio.sleep(30)
            await self.release_idle_threads()

    async def release_idle_threads(self, idle_seconds: float = 60) -> None:
        """Drop idle subscriptions so Codex can unload sessions after its grace period."""
        for thread_id in list(self._thread_activity):
            async with self._thread_lock(thread_id):
                touched = self._thread_activity.get(thread_id)
                if touched is None or time.monotonic() - touched < idle_seconds:
                    continue
                try:
                    response = await self._call("thread/read", {"threadId": thread_id})
                    status = response["thread"].get("status", {}).get("type")
                    if status == "notLoaded":
                        self._thread_activity.pop(thread_id, None)
                        continue
                    if status != "idle":
                        continue
                    try:
                        response = await self._call(
                            "thread/goal/get", {"threadId": thread_id}
                        )
                    except AppServerError as error:
                        if "goals feature is disabled" not in str(error):
                            raise
                        response = {}
                    if (response.get("goal") or {}).get("status") == "active":
                        continue
                    # Events can arrive while either read is in flight.
                    if self._thread_activity.get(thread_id) != touched:
                        continue
                    await self._call("thread/unsubscribe", {"threadId": thread_id})
                    self._thread_activity.pop(thread_id, None)
                    logger.info("Released idle Codex thread %s", thread_id)
                except (AppServerError, TimeoutError, KeyError):
                    logger.warning(
                        "Could not release idle thread %s", thread_id, exc_info=True
                    )

    async def _call(
        self, method: str, params: dict[str, Any] | None = None, timeout: float = 30.0
    ) -> dict[str, Any]:
        request_id = self._next_id
        self._next_id += 1
        future = asyncio.get_running_loop().create_future()
        self._pending[request_id] = future
        try:
            await self._send(
                {"method": method, "id": request_id, "params": params or {}}
            )
            message = await asyncio.wait_for(future, timeout)
        finally:
            self._pending.pop(request_id, None)
        if "error" in message:
            error = message["error"]
            raise AppServerError(f"{method}: {error.get('message', error)}")
        return cast(dict[str, Any], message.get("result", {}))

    async def notify(self, method: str, params: dict[str, Any] | None = None) -> None:
        message: dict[str, Any] = {"method": method}
        if params is not None:
            message["params"] = params
        await self._send(message)

    async def default_model(self, cwd: str | None) -> str:
        response = await self.call("config/read", {"includeLayers": False, "cwd": cwd})
        if model := response["config"].get("model"):
            return str(model)
        response = await self.call("model/list", {"limit": 100})
        for model in response["data"]:
            if model.get("isDefault"):
                return str(model["model"])
        raise AppServerError("Codex did not report a default model")

    async def default_effort(self, cwd: str | None, model: str | None) -> str:
        response = await self.call("config/read", {"includeLayers": False, "cwd": cwd})
        config = response["config"]
        if effort := config.get("model_reasoning_effort"):
            return str(effort)
        selected = model or config.get("model")
        response = await self.call("model/list", {"limit": 100})
        for candidate in response["data"]:
            if candidate["model"] == selected or (
                not selected and candidate.get("isDefault")
            ):
                return str(candidate["defaultReasoningEffort"])
        raise AppServerError("Codex did not report a default reasoning effort")

    async def generate_text(
        self,
        *,
        instructions: str,
        text: str,
        model: str | None,
        model_reasoning_effort: str | None = None,
    ) -> str:
        """Run a temporary text task and await its final event, without polling."""
        thread_id = None
        turn_id = None
        finished = False
        try:
            async with asyncio.timeout(120), self.events() as events:
                params: dict[str, Any] = {
                    "cwd": str(Path.home()),
                    "ephemeral": True,
                    "baseInstructions": instructions,
                    "developerInstructions": "",
                    "sandbox": "read-only",
                    "approvalPolicy": "never",
                }
                if model is not None:
                    params["model"] = model
                if model_reasoning_effort is not None:
                    params["config"] = {
                        "model_reasoning_effort": model_reasoning_effort
                    }
                thread = await self.call("thread/start", params)
                thread_id = thread["thread"]["id"]
                turn = await self.call(
                    "turn/start",
                    {
                        "threadId": thread_id,
                        "input": [{"type": "text", "text": text}],
                    },
                )
                turn_id = turn["turn"]["id"]
                result = ""
                while True:
                    event = await events.get()
                    params = event.get("params") or {}
                    if params.get("threadId") != thread_id:
                        continue
                    if (
                        event.get("method") == "item/completed"
                        and params.get("turnId") == turn_id
                    ):
                        item = params["item"]
                        if (
                            item.get("type") == "agentMessage"
                            and item.get("phase") != "commentary"
                        ):
                            result = item.get("text", "")
                    if (
                        event.get("method") == "turn/completed"
                        and params["turn"]["id"] == turn_id
                    ):
                        finished = True
                        if params["turn"].get("status") != "completed":
                            raise AppServerError("Title generation did not complete")
                        return result
        except TimeoutError as error:
            raise AppServerError("Title generation timed out") from error
        finally:
            if thread_id:
                if turn_id and not finished:
                    with suppress(AppServerError, TimeoutError):
                        await self.call(
                            "turn/interrupt",
                            {"threadId": thread_id, "turnId": turn_id},
                            timeout=5,
                        )
                with suppress(AppServerError, TimeoutError):
                    await self.call(
                        "thread/unsubscribe", {"threadId": thread_id}, timeout=5
                    )

    async def respond(
        self,
        request_id: int | str,
        *,
        result: dict[str, Any] | None = None,
        error: dict[str, Any] | None = None,
    ) -> None:
        if (result is None) == (error is None):
            raise ValueError("provide exactly one of result or error")
        message: dict[str, Any] = {"id": request_id}
        if result is not None:
            message["result"] = result
        else:
            message["error"] = error
        await self._send(message)

    @asynccontextmanager
    async def events(self) -> AsyncIterator[asyncio.Queue[dict[str, Any]]]:
        queue: asyncio.Queue[dict[str, Any]] = asyncio.Queue()
        self._subscribers.add(queue)
        try:
            yield queue
        finally:
            self._subscribers.discard(queue)

    async def _send(self, message: dict[str, Any]) -> None:
        if (
            not self.process
            or not self.process.stdin
            or self.process.returncode is not None
            or (self._reader_task is not None and self._reader_task.done())
        ):
            raise AppServerError("Codex app-server is not running")
        encoded = json.dumps(message, separators=(",", ":")).encode() + b"\n"
        async with self._write_lock:
            self.process.stdin.write(encoded)
            await self.process.stdin.drain()

    async def _read_messages(self) -> None:
        assert self.process and self.process.stdout
        try:
            async for line in read_lines(self.process.stdout):
                try:
                    message = json.loads(line)
                except json.JSONDecodeError:
                    logger.warning("Ignoring invalid app-server JSON: %r", line)
                    continue
                request_id = message.get("id")
                if request_id in self._pending and (
                    "result" in message or "error" in message
                ):
                    future = self._pending[request_id]
                    if not future.done():
                        future.set_result(message)
                    continue
                self._publish_event(message)
        finally:
            error = AppServerError("Codex app-server stopped")
            for future in self._pending.values():
                if not future.done():
                    future.set_exception(error)

    def _publish_event(self, message: dict[str, Any]) -> None:
        params = message.get("params") or {}
        thread_id = params.get("threadId")
        if thread_id in self._thread_activity:
            if message.get("method") == "thread/closed":
                self._thread_activity.pop(thread_id, None)
            else:
                self._thread_activity[thread_id] = time.monotonic()
        if message.get("method") in {"account/rateLimits/updated", "account/updated"}:
            self._rate_limits_revision += 1
            self._rate_limits_until = 0.0
        # Clients receive complete items; token deltas never leave the bouncer.
        if str(message.get("method", "")).lower().endswith("delta"):
            return
        for queue in tuple(self._subscribers):
            queue.put_nowait(message)

    async def _read_stderr(self) -> None:
        assert self.process and self.process.stderr
        async for line in read_lines(self.process.stderr):
            logger.info("codex: %s", line.decode(errors="replace").rstrip())
