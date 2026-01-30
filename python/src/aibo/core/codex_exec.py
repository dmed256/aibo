from __future__ import annotations

import asyncio
import base64
import os
from typing import TYPE_CHECKING, Annotated, AsyncGenerator, Literal, Union

from pydantic import BaseModel, Field, TypeAdapter, ValidationError

from aibo.common.chat import ImageMessageContent, MessageRole
from aibo.common.constants import Env
from aibo.db.models import ImageModel

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

if TYPE_CHECKING:
    from aibo.core.chat import Message


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

CODEX_EVENT_ADAPTER: TypeAdapter[CodexEvent] = TypeAdapter(CodexEvent)


async def build_codex_prompt(messages: list["Message"]) -> tuple[str, list[str]]:
    user_message = None
    for message in reversed(messages):
        if message.role == MessageRole.USER:
            user_message = message
            break
    if user_message is None:
        return "", []

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

    assert process.stdout is not None
    stderr_task: asyncio.Task[bytes] | None = None
    if process.stderr is not None:
        stderr_task = asyncio.create_task(_collect_stream(process.stderr))

    async for line in _iter_stream_lines(process.stdout):
        text = line.decode("utf-8").strip()
        if not text:
            continue
        try:
            yield CODEX_EVENT_ADAPTER.validate_json(text)
        except ValidationError as exc:
            raise ValueError(f"Invalid codex event: {text}") from exc

    return_code = await process.wait()
    stderr_output = b""
    if stderr_task is not None:
        stderr_output = await stderr_task

    if return_code != 0:
        stderr_text = stderr_output.decode("utf-8")
        raise RuntimeError(
            f"codex exec failed with code {return_code}: {stderr_text}".strip()
        )


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


def _write_cached_image(image: ImageModel) -> str:
    os.makedirs(IMAGE_CACHE_DIR, exist_ok=True)
    image_path = os.path.join(IMAGE_CACHE_DIR, f"{image.id}.{image.format}")
    image_bytes = base64.b64decode(image.contents_b64)
    with open(image_path, "wb") as image_file:
        image_file.write(image_bytes)
    return image_path
