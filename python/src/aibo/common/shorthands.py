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


async def message_content_expands_image(
    contents: list[chat.MessageContent],
) -> bool:
    im_pattern = _shorthand_pattern("im")
    return any(
        im_pattern.search(content.text)
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
    im_pattern = _shorthand_pattern("im")
    message_content_indices_with_image = {
        indices
        for indices, content in text_contents.items()
        if im_pattern.search(content.text)
    }
    if not message_content_indices_with_image:
        return

    if not (image := await ImageModel.from_clipboard(trace_id=trace_id)):
        raise ValueError(f"No image in clipboard")

    for message_index, contents in enumerate(message_contents):
        new_contents: list[chat.MessageContent] = []
        for content_index, content in enumerate(contents):
            if (message_index, content_index) not in message_content_indices_with_image:
                new_contents.append(content)
                continue

            if not isinstance(content, chat.TextMessageContent):
                new_contents.append(content)
                continue

            for sub_content_index, text in enumerate(im_pattern.split(content.text)):
                if sub_content_index % 2:
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

    Common:
    - \r: Emacs region
    - \b: Emacs buffer
    """
    return re.compile(rf"((?:^|\s)\\{shorthand}(?:\s|$))")
