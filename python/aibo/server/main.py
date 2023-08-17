from fastapi import FastAPI

from aibo.db import migrate_documents
from aibo.server.routes import chat_routes, healthcheck_routes, websocket_routes

migrate_documents()

app = FastAPI()

app.include_router(chat_routes.router)
app.include_router(healthcheck_routes.router)
app.include_router(websocket_routes.router)
