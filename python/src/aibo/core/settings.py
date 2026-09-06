from pydantic import BaseModel, ConfigDict, Field, field_validator
from sqlalchemy.ext.asyncio import AsyncSession

from aibo.db.models.workspace_model import ModelSettingsModel


class ModelSettings(BaseModel):
    model_config = ConfigDict(extra="forbid")
    manager_model: str | None = Field(default=None, max_length=200)
    bot_model: str | None = Field(default=None, max_length=200)
    title_model: str | None = Field(default="gpt-5.6-luna", max_length=200)

    manager_model_reasoning_effort: str | None = Field(default=None, max_length=200)
    bot_model_reasoning_effort: str | None = Field(default=None, max_length=200)
    title_model_reasoning_effort: str | None = Field(default="low", max_length=200)

    @field_validator(
        "manager_model",
        "bot_model",
        "title_model",
        "manager_model_reasoning_effort",
        "bot_model_reasoning_effort",
        "title_model_reasoning_effort",
    )
    @classmethod
    def normalize(cls, value: str | None) -> str | None:
        return (value.strip() or None) if value is not None else None


async def get_models(db: AsyncSession) -> ModelSettings:
    row = await db.get(ModelSettingsModel, 1)
    return ModelSettings(**row.models) if row else ModelSettings()


async def save_models(db: AsyncSession, models: ModelSettings) -> None:
    await db.merge(ModelSettingsModel(id=1, models=models.model_dump()))
    await db.commit()
