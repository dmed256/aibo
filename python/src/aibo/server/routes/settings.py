import logging

from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.ext.asyncio import AsyncSession

from aibo.core.model_catalog import CodexModel, ModelCatalog
from aibo.core.settings import ModelSettings, get_models, save_models
from aibo.db.client import get_db

router = APIRouter()


@router.get("/settings/models", response_model=ModelSettings)
async def model_settings(db: AsyncSession = Depends(get_db)) -> ModelSettings:
    return await get_models(db)


@router.put("/settings/models", response_model=ModelSettings)
async def update_model_settings(
    body: ModelSettings, db: AsyncSession = Depends(get_db)
) -> ModelSettings:
    await save_models(db, body)
    return body


_catalog = ModelCatalog()


@router.get("/settings/models/catalog", response_model=list[CodexModel])
async def model_catalog() -> list[CodexModel]:
    try:
        return await _catalog.read()
    except Exception as exc:
        logging.getLogger(__name__).warning(
            "Codex model catalog unavailable", exc_info=True
        )
        raise HTTPException(503, "Could not load Codex models. Try again.") from exc
