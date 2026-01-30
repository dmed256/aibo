import glob
import logging
import os
import re
from functools import cache
from typing import Sequence
from uuid import UUID

from aibo.core import chat
from aibo.db.models import ImageModel

__all__ = [
    "message_content_expands_image",
    "expand_messages_shorthands_inplace",
    "expand_contents_shorthands_inplace",
]

logger = logging.getLogger(__name__)

CLIPBOARD_IMAGE_SHORTHANDS_PATTERN_STR = "(?:im|sc)"
CLIPBOARD_IMAGE_SHORTHANDS_PATTERN = re.compile(CLIPBOARD_IMAGE_SHORTHANDS_PATTERN_STR)

IMAGE_SHORTHAND_PATTERN_STR = r"\\i\[([^]]+)\]"
IMAGE_SHORTHAND_PATTERN = re.compile(IMAGE_SHORTHAND_PATTERN_STR)

DIR_SHORTHAND_PATTERN_STR = r"\\d\[([^]]+)\]"
DIR_SHORTHAND_PATTERN = re.compile(DIR_SHORTHAND_PATTERN_STR)

FILE_SHORTHAND_PATTERN_STR = r"\\f\[([^]]+)\]"
FILE_SHORTHAND_PATTERN = re.compile(FILE_SHORTHAND_PATTERN_STR)


@cache
def _shorthand_re_sub_pattern(shorthand: str) -> re.Pattern:
    r"""
    Match \{shorthand} in the beginning, middle, or end:
    - "\r hello world"
    - "hello \r world"
    - "hello world \r"

    Builtins:
    - \im: Clipboard image
    - \sc: Monitor screenshot
    - \f: Inject the file contents
        - \f[<filename>]: How to add the filename argument
        - \f[<filename>:<lineno>]: Inject just that one line
        - \f[<filename>:<start>:<end>]: Inject the line region

    - \d: Inject the directory contents
        - \d[<directory-or-glob>]: Injects all non-binary files in the directory (or glob)

    - \i[<uuid>]: Inject an image by its ID

    Common custom shorthands:
    - \r: Emacs region
    - \b: Emacs buffer
    """
    return re.compile(rf"((?:^|\s)\\{shorthand}(?:\s|$))")


def message_content_expands_image(
    contents: list[chat.MessageContent],
) -> bool:
    image_pattern = _shorthand_re_sub_pattern(CLIPBOARD_IMAGE_SHORTHANDS_PATTERN_STR)
    return any(
        image_pattern.search(content.text)
        for content in contents
        if isinstance(content, chat.TextMessageContent)
    )


async def expand_messages_shorthands_inplace(
    *,
    trace_id: UUID,
    messages: Sequence[chat.Message | chat.CreateMessageInputs],
    shorthands: dict[str, str],
) -> None:
    message_contents = [message.contents for message in messages]
    await expand_contents_shorthands_inplace(
        trace_id=trace_id,
        message_contents=message_contents,
        shorthands=shorthands,
    )
    for message, contents in zip(messages, message_contents):
        message.contents = contents


async def expand_contents_shorthands_inplace(
    *,
    trace_id: UUID,
    message_contents: list[list[chat.MessageContent]],
    shorthands: dict[str, str],
) -> None:
    # Replace custom-shorthands
    text_contents = {
        (message_index, content_index): content
        for message_index, contents in enumerate(message_contents)
        for content_index, content in enumerate(contents)
        if isinstance(content, chat.TextMessageContent)
    }

    replace_custom_shorthands(
        text_contents=text_contents,
        shorthands=shorthands,
    )
    await replace_clipboard_image_shorthands(
        trace_id=trace_id,
        message_contents=message_contents,
        text_contents=text_contents,
    )
    replace_image_shorthands(
        message_contents=message_contents,
        text_contents=text_contents,
    )
    replace_directory_shorthands(
        text_contents=text_contents,
    )
    replace_file_shorthands(
        text_contents=text_contents,
    )


def replace_custom_shorthands(
    *,
    text_contents: dict[tuple[int, int], chat.TextMessageContent],
    shorthands: dict[str, str],
) -> None:
    r"""
    Common custom shorthands:
    - \r: Emacs region
    - \b: Emacs buffer
    """
    for shorthand, replacement in shorthands.items():
        for text_content in text_contents.values():
            p = _shorthand_re_sub_pattern(shorthand)
            text_content.text = p.sub(lambda m: replacement, text_content.text)


def replace_image_shorthands(
    *,
    message_contents: list[list[chat.MessageContent]],
    text_contents: dict[tuple[int, int], chat.TextMessageContent],
) -> None:
    r"""
    Builtins:
    - \i[<uuid>]: Inject an image by its ID
    """
    pattern = IMAGE_SHORTHAND_PATTERN
    for (msg_idx, content_idx), text_content in list(text_contents.items()):
        parts = re.split(pattern, text_content.text)
        if len(parts) == 1:
            continue
        new_parts: list[chat.MessageContent] = []
        for idx, part in enumerate(parts):
            if idx % 2:
                new_parts.append(chat.ImageMessageContent(image_id=UUID(part)))
            elif part:
                new_parts.append(chat.TextMessageContent(text=part))
        message_contents[msg_idx][content_idx : content_idx + 1] = new_parts


async def replace_clipboard_image_shorthands(
    *,
    trace_id: UUID,
    message_contents: list[list[chat.MessageContent]],
    text_contents: dict[tuple[int, int], chat.TextMessageContent],
) -> None:
    r"""
    Builtins:
    - \im: Clipboard image
    - \sc: Monitor screenshot
    """
    # Replace image shorthand
    image_pattern = _shorthand_re_sub_pattern(CLIPBOARD_IMAGE_SHORTHANDS_PATTERN_STR)
    message_content_indices_with_image = {
        indices: {
            "is_im": r"\im" in re_match.group(),
            "is_sc": r"\sc" in re_match.group(),
        }
        for indices, content in text_contents.items()
        if (re_match := image_pattern.search(content.text))
    }
    if not message_content_indices_with_image:
        return

    has_clipboard_image = any(
        matches["is_im"] for matches in message_content_indices_with_image.values()
    )
    has_screen_image = any(
        matches["is_sc"] for matches in message_content_indices_with_image.values()
    )

    clipboard_image: ImageModel | None = None
    screen_image: ImageModel | None = None

    if has_clipboard_image and not (
        clipboard_image := await ImageModel.from_clipboard(trace_id=trace_id)
    ):
        raise ValueError("No image in clipboard")

    if has_screen_image and not (
        screen_image := await ImageModel.from_screen(trace_id=trace_id)
    ):
        raise ValueError("Unable to screenshot screen")

    for message_index, contents in enumerate(message_contents):
        new_contents: list[chat.MessageContent] = []
        for content_index, content in enumerate(contents):
            maybe_match = message_content_indices_with_image.get(
                (message_index, content_index)
            )
            if not maybe_match:
                new_contents.append(content)
                continue

            if not isinstance(content, chat.TextMessageContent):
                new_contents.append(content)
                continue

            for sub_content_index, text in enumerate(image_pattern.split(content.text)):
                if sub_content_index % 2:
                    image = clipboard_image if maybe_match["is_im"] else screen_image
                    if not image:
                        if maybe_match["is_im"]:
                            raise ValueError("Expected clipboard image")
                        else:
                            raise ValueError("Missing display screenshot image")

                    new_contents.append(chat.ImageMessageContent(image_id=image.id))
                elif text:
                    new_contents.append(chat.TextMessageContent(text=text))

        message_contents[message_index] = new_contents


def replace_directory_shorthands(
    *,
    text_contents: dict[tuple[int, int], chat.TextMessageContent],
) -> None:
    r"""
    Builtins:
    - \d: Inject the directory contents
        - \d[<directory-or-glob>]: Injects all non-binary files in the directory (or glob)
    """
    for text_content in text_contents.values():
        parts = re.split(DIR_SHORTHAND_PATTERN, text_content.text)
        if len(parts) <= 1:
            continue

        text_content.text = "".join(
            [
                _maybe_expand_directory(part) if part_index % 2 else part
                for part_index, part in enumerate(parts)
            ]
        )


def _maybe_expand_directory(dir_glob: str) -> str:
    """
    Expand the directory expression into the aggregated contents of all matching
    non-binary files. If nothing is found, returns the original shorthand.
    """
    dirpath = os.path.expanduser(dir_glob)
    if "*" not in dirpath:
        dirpath += "**/*"

    files = sorted(glob.glob(dirpath, recursive=True))

    if not files:
        return f"(Missing directory contents: {dir_glob})"

    merged_content = ""
    for file_path in files:
        if not os.path.isfile(file_path):
            continue

        try:
            # Read a sample in binary to determine if the file contains null bytes.
            with open(file_path, "rb") as f:
                sample = f.read(1024)

            if sample and b"\0" not in sample:
                merged_content += _maybe_expand_file(file_path)
        except Exception:
            continue

    return merged_content


def replace_file_shorthands(
    *,
    text_contents: dict[tuple[int, int], chat.TextMessageContent],
) -> None:
    """
    Builtins:
    - \f: Inject the file contents
        - \f[<filename>]: How to add the filename argument
        - \f[<filename>:<lineno>]: Inject just that one line
        - \f[<filename>:<start>:<end>]: Inject the line region
    """
    for text_content in text_contents.values():
        parts = re.split(FILE_SHORTHAND_PATTERN, text_content.text)
        if len(parts) <= 1:
            continue

        text_content.text = "".join(
            [
                _maybe_expand_file(part) if part_index % 2 else part
                for part_index, part in enumerate(parts)
            ]
        )


def _maybe_expand_file(content: str) -> str:
    """
    Check for one of these formats:
    - <filename>
    - <filename>:<lineno>
    - <filename>:<start>:<end>

    If it doesn't exist, replace back with \f[{content}]
    """
    parts = content.split(":")

    def maybe_int(index: int) -> int | None:
        try:
            return int(parts[index])
        except:
            return None

    filename = parts[0]
    start = maybe_int(1)
    end = maybe_int(2)

    file_contents: str | None = None
    try:
        with open(os.path.expanduser(filename), "r") as file:
            lines = file.readlines()
            if start is not None and end is not None:
                file_contents = "".join(lines[start - 1 : end])
            elif start is not None:
                file_contents = lines[start - 1]
            else:
                file_contents = "".join(lines)
    except:
        pass

    if file_contents is None:
        return f"\\f[{content}]"

    header = f"---[ {content} ]"
    header += "-" * max(10, 50 - len(header))
    footer = "-" * len(header)

    return f"\n\n{header}\n{file_contents}\n{footer}\n\n"
