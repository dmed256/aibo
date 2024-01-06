import re
from functools import cache
from uuid import UUID

from aibo.core import chat
from aibo.db.models import ImageModel

__all__ = [
    "message_content_expands_image",
    "expand_messages_shorthands_inplace",
    "expand_contents_shorthands_inplace",
]

IMAGE_SHORTHANDS_PATTERN = "(?:im|sc)"


def message_content_expands_image(
    contents: list[chat.MessageContent],
) -> bool:
    image_pattern = _shorthand_pattern(IMAGE_SHORTHANDS_PATTERN)
    return any(
        image_pattern.search(content.text)
        for content in contents
        if isinstance(content, chat.TextMessageContent)
    )


async def expand_messages_shorthands_inplace(
    *,
    trace_id: UUID,
    messages: list[chat.Message] | list[chat.CreateMessageInputs],
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

    for shorthand, replacement in shorthands.items():
        for text_content in text_contents.values():
            text_content.text = _shorthand_replace(
                content=text_content.text,
                shorthand=shorthand,
                replacement=replacement,
            )

    # Replace image shorthand (Requires vision model!)
    image_pattern = _shorthand_pattern(IMAGE_SHORTHANDS_PATTERN)
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


def _shorthand_replace(*, content: str, shorthand: str, replacement: str) -> str:
    p = _shorthand_pattern(shorthand)
    return p.sub(replacement, content)


@cache
def _shorthand_pattern(shorthand: str) -> re.Pattern:
    """
    Match \{shorthand} in the beginning, middle, or end:
    - "\r hello world"
    - "hello \r world"
    - "hello world \r"

    Builtins:
    - \im: Clipboard image
    - \sc: Monitor screenshot

    Common:
    - \r: Emacs region
    - \b: Emacs buffer
    """
    return re.compile(rf"((?:^|\s)\\{shorthand}(?:\s|$))")
