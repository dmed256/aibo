from __future__ import annotations

import asyncio
import base64
import logging
import os
from typing import TYPE_CHECKING, Annotated, AsyncGenerator, Literal, Union

from pydantic import BaseModel, Field, TypeAdapter

from aibo.common.chat import ImageMessageContent, MessageRole
from aibo.common.constants import Env
from aibo.db.models import ImageModel

if TYPE_CHECKING:
    from aibo.core.chat import Message

logger = logging.getLogger(__name__)

__all__ = [
    "CodexAgentMessageItem",
    "CodexCommandExecutionItem",
    "CodexEvent",
    "CodexErrorEvent",
    "CodexFileChangeItem",
    "CodexItem",
    "CodexItemCompletedEvent",
    "CodexItemStartedEvent",
    "CodexReasoningItem",
    "CodexThreadStartedEvent",
    "CodexTurnCompletedEvent",
    "CodexTurnStartedEvent",
    "CodexUsage",
    "build_codex_prompt",
    "stream_codex_events",
]

IMAGE_CACHE_DIR = os.path.expanduser("~/.cache/aibo/images")


class CodexUsage(BaseModel):
    input_tokens: int
    cached_input_tokens: int
    output_tokens: int


class CodexThreadStartedEvent(BaseModel):
    type: Literal["thread.started"] = "thread.started"
    thread_id: str


class CodexTurnStartedEvent(BaseModel):
    type: Literal["turn.started"] = "turn.started"


class CodexCommandExecutionItem(BaseModel):
    id: str
    type: Literal["command_execution"] = "command_execution"
    command: str
    aggregated_output: str
    exit_code: int | None
    status: Literal["in_progress", "completed", "failed"]

    def to_summary(self) -> str:
        """
        Format a command execution like Codex CLI output.
        """
        if len(self.aggregated_output) <= 6_000:
            output = self.aggregated_output
        else:
            prefix = self.aggregated_output[:3000]
            suffix = self.aggregated_output[-3000:]
            removed_chars = len(self.aggregated_output) - 6_000
            output = f"{prefix}\n\n... {removed_chars} chars truncated ...\n\n{suffix}"

        line_count = len(self.aggregated_output.splitlines())

        return "\n".join(
            [
                f"> {self.command}",
                "-" * 60,
                output.strip("\n"),
                "-" * 60,
                f"[exit_code={self.exit_code} {line_count=}]",
            ]
        )


class CodexReasoningItem(BaseModel):
    id: str
    type: Literal["reasoning"] = "reasoning"
    text: str


class CodexAgentMessageItem(BaseModel):
    id: str
    type: Literal["agent_message"] = "agent_message"
    text: str


class CodexFileChange(BaseModel):
    path: str
    kind: str


class CodexFileChangeItem(BaseModel):
    id: str
    type: Literal["file_change"] = "file_change"
    changes: list[CodexFileChange]
    status: Literal["in_progress", "completed", "failed"]


CodexItem = Annotated[
    Union[
        CodexCommandExecutionItem,
        CodexFileChangeItem,
        CodexReasoningItem,
        CodexAgentMessageItem,
    ],
    Field(discriminator="type"),
]


class CodexItemStartedEvent(BaseModel):
    type: Literal["item.started"] = "item.started"
    item: CodexItem


class CodexItemCompletedEvent(BaseModel):
    type: Literal["item.completed"] = "item.completed"
    item: CodexItem


class CodexTurnCompletedEvent(BaseModel):
    type: Literal["turn.completed"] = "turn.completed"
    usage: CodexUsage


class CodexErrorEvent(BaseModel):
    type: Literal["error"] = "error"
    message: str


CodexEvent = Annotated[
    Union[
        CodexThreadStartedEvent,
        CodexTurnStartedEvent,
        CodexItemStartedEvent,
        CodexItemCompletedEvent,
        CodexTurnCompletedEvent,
        CodexErrorEvent,
    ],
    Field(discriminator="type"),
]

CodexEventAdapter: TypeAdapter[CodexEvent] = TypeAdapter(CodexEvent)


async def build_codex_prompt(messages: list["Message"]) -> tuple[str, list[str]]:
    # If the last message isn't a user message, we're resuming some conversation
    if not messages or messages[-1].role != MessageRole.USER:
        return "", []

    user_message = messages[-1]
    image_paths: list[str] = []
    content_parts: list[str] = []
    for content in user_message.contents:
        if isinstance(content, ImageMessageContent):
            image = await ImageModel.by_id(content.image_id)
            if not image:
                raise ValueError(f"Image not found: {content.image_id}")
            image_path = _write_cached_image(image)
            image_index = len(image_paths)
            image_paths.append(image_path)
            content_parts.append(f"(images[{image_index}])")
        else:
            content_text = str(content).strip()
            if content_text:
                content_parts.append(content_text)
    if not content_parts:
        return "", image_paths
    prompt = "\n\n".join(content_parts).strip()
    return prompt, image_paths


async def stream_codex_events(
    *,
    prompt: str,
    model_name: str | None,
    cwd: str | None = None,
    image_paths: list[str] | None = None,
    session_id: str | None = None,
) -> AsyncGenerator[CodexEvent, None]:
    process, stderr_task = await _spawn_codex_process(
        prompt=prompt,
        model_name=model_name,
        cwd=cwd,
        image_paths=image_paths,
        session_id=session_id,
    )
    try:
        assert process.stdout is not None
        async for line in _iter_stream_lines(process.stdout):
            text = line.decode("utf-8").strip()
            if not text:
                continue
            yield CodexEventAdapter.validate_json(text)

        return_code = await process.wait()
        stderr_output = b""
        if stderr_task is not None:
            stderr_output = await stderr_task

        if return_code != 0:
            stderr_text = stderr_output.decode("utf-8")
            raise RuntimeError(
                f"codex exec failed with code {return_code}: {stderr_text}".strip()
            )
    except asyncio.CancelledError:
        return
    finally:
        await asyncio.shield(_shutdown_process(process, stderr_task))


async def _spawn_codex_process(
    *,
    prompt: str,
    model_name: str | None,
    cwd: str | None = None,
    image_paths: list[str] | None = None,
    session_id: str | None = None,
) -> tuple[asyncio.subprocess.Process, asyncio.Task[bytes] | None]:
    if session_id:
        command = ["codex", "exec", "resume", "--json"]
    else:
        command = ["codex", "exec", "--json"]

    codex_approval_policy = Env.get().CODEX_APPROVAL_POLICY
    if codex_approval_policy:
        command.append(codex_approval_policy)

    if image_paths:
        command.extend(["--image", ",".join(image_paths)])

    if model_name is not None:
        command.extend(["--model", model_name])

    if session_id:
        command.append(session_id)

    command.append("-")

    process = await asyncio.create_subprocess_exec(
        *command,
        stdin=asyncio.subprocess.PIPE,
        stdout=asyncio.subprocess.PIPE,
        stderr=asyncio.subprocess.PIPE,
        cwd=cwd,
    )

    assert process.stdin is not None
    process.stdin.write(prompt.encode("utf-8"))
    await process.stdin.drain()
    process.stdin.close()

    stderr_task: asyncio.Task[bytes] | None = None
    if process.stderr is not None:
        stderr_task = asyncio.create_task(_collect_stream(process.stderr))

    return process, stderr_task


async def _collect_stream(stream: asyncio.StreamReader) -> bytes:
    chunks: list[bytes] = []
    while True:
        chunk = await stream.read(4096)
        if not chunk:
            break
        chunks.append(chunk)
    return b"".join(chunks)


async def _iter_stream_lines(
    stream: asyncio.StreamReader,
    *,
    chunk_size: int = 4096,
) -> AsyncGenerator[bytes, None]:
    unyielded_buffer = bytearray()
    while True:
        chunk = await stream.read(chunk_size)

        # We're done iterating
        if not chunk:
            if unyielded_buffer:
                yield bytes(unyielded_buffer)
            break

        # Iterate line-by-line
        unyielded_buffer.extend(chunk)
        while True:
            newline_index = unyielded_buffer.find(b"\n")
            if newline_index == -1:
                break
            line = unyielded_buffer[: newline_index + 1]
            del unyielded_buffer[: newline_index + 1]
            yield bytes(line)


async def _shutdown_process(
    process: asyncio.subprocess.Process,
    stderr_task: asyncio.Task[bytes] | None,
) -> None:
    if process.returncode is None:
        process.terminate()
        await asyncio.sleep(0.2)
        if process.returncode is None:
            process.kill()
        await process.wait()

    if stderr_task is not None and not stderr_task.done():
        stderr_task.cancel()
        await asyncio.gather(stderr_task, return_exceptions=True)


def _write_cached_image(image: ImageModel) -> str:
    os.makedirs(IMAGE_CACHE_DIR, exist_ok=True)
    image_path = os.path.join(IMAGE_CACHE_DIR, f"{image.id}.{image.format}")
    image_bytes = base64.b64decode(image.contents_b64)
    with open(image_path, "wb") as image_file:
        image_file.write(image_bytes)
    return image_path
