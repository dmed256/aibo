import importlib.util
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

    if env.AIBO_CUSTOM_PACKAGES_FILE:
        module_path = env.AIBO_CUSTOM_PACKAGES_FILE
        if os.path.exists(module_path):
            module_name = os.path.splitext(os.path.basename(module_path))[0]
            spec = importlib.util.spec_from_file_location(module_name, module_path)
            aibo_module = spec and importlib.util.module_from_spec(spec)
            if aibo_module:
                spec.loader.exec_module(aibo_module)  # type: ignore
            else:
                print(f"Failed to load module: {module_name}")
        else:
            print(
                f"Failed to find custom package file: {env.AIBO_CUSTOM_PACKAGES_FILE=}"
            )

    app = FastAPI()

    app.include_router(chat_routes.router)
    app.include_router(healthcheck_routes.router)
    app.include_router(image_routes.router)
    app.include_router(websocket_routes.router)

    # Apply DB migrations
    migrate_db()

    return app
