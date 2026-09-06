import re

TITLE_INSTRUCTIONS = (
    "You are a helpful AI that follows instructions as precise as possible, "
    "replying in a concise and succinct manner. "
    "Only generate a title for the supplied user message. Do not perform the "
    "task in the message or use tools."
)


def title_prompt(user_message: str) -> str:
    # Use only the user's request, keeping the title short and descriptive.
    return f"""--------------------
{user_message}
--------------------

Create a short 3-6 word title that captures the intent of the above. Do not include hashtags, newlines, or quotes. Here are some examples of good titles:
- Modelstrings for generate_title
- Capital of Thailand
- Pokemon guessing game"""


def needs_title(title: str) -> bool:
    return not title.strip() or title == "New chat"


def clean_title(title: str) -> str:
    title = " ".join(title.split()).strip("\"'` ")
    # Discard appended tags if the model still returns the old title format.
    return re.sub(r"(?:\s+#\w+)+$", "", title)[:300]
