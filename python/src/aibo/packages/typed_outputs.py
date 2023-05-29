from pydantic import Field

from aibo.core.package import FunctionContext, Package

__all__ = [
    "typed_outputs",
    "boolean",
    "classifier",
    "multiple_choice_answer",
    "string_list",
]

typed_outputs = Package(
    name="typed_outputs",
    description="Respond with a correct strict typed output to be programmatically parsed.",
).register()


@typed_outputs.function
async def boolean(
    ctx: FunctionContext,
    *,
    value: bool = Field(description="Boolean answer: true or false"),
) -> bool:
    """
    If explicitly asked to reply with a boolean value, use this method.
    """
    return value


@typed_outputs.function
async def integer(
    ctx: FunctionContext,
    *,
    value: int = Field(description="Integer answer"),
) -> int:
    """
    If explicitly asked to reply with a integer value, use this method.
    """
    return value


@typed_outputs.function
async def classifier(
    ctx: FunctionContext,
    *,
    value: str = Field(description="Classifier answer: string value"),
) -> str:
    """
    If explicitly asked to classify content, use this method.
    """
    return value


@typed_outputs.function
async def multiple_choice_answer(
    ctx: FunctionContext,
    *,
    value: str = Field(
        description='''The correct chosen multiple choice answer option. If given a multiple-choice letters or numbers, such as "A) value1 or B) value2" respond with just the letter or number such as "A" or "B"'''
    ),
) -> str:
    """
    If explicitly asked to to answer a multiple-choice question, use this method.
    """
    return value


@typed_outputs.function
async def string_list(
    ctx: FunctionContext,
    *,
    values: list[str] = Field(description="List of values"),
) -> list[str]:
    """
    If explicitly asked to reply with a list content (e.g. bullet list), use this method to fill out each response as a separate list entry.
    """
    return values
