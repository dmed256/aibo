import importlib
import os

from fastapi import FastAPI

# Import our packages
import aibo.packages
from aibo.common.constants import Env
from aibo.db import migrate_db
from aibo.server.routes import (
    chat_routes,
    healthcheck_routes,
    image_routes,
    websocket_routes,
)


def create_app() -> FastAPI:
    env = Env.get()
    # Import external packages if defined
    if env.AIBO_CUSTOM_PACKAGES_MODULE:
        importlib.import_module(env.AIBO_CUSTOM_PACKAGES_MODULE)

    app = FastAPI()

    app.include_router(chat_routes.router)
    app.include_router(healthcheck_routes.router)
    app.include_router(image_routes.router)
    app.include_router(websocket_routes.router)

    # Apply DB migrations
    migrate_db()

    return app
