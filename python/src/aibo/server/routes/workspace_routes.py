"""Public API assembled from resource-specific routers."""

from fastapi import APIRouter

from aibo.server.routes import (
    attachments,
    chats,
    context,
    notifications,
    settings,
    system,
    turns,
)

router = APIRouter(prefix="/api")
router.include_router(settings.router)
router.include_router(attachments.router)
router.include_router(system.router)
router.include_router(context.router)
router.include_router(chats.router)
router.include_router(turns.router)
router.include_router(notifications.router)
