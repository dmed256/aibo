from fastapi import FastAPI

from aibo.db import migrate_documents
from aibo.server.routes import chat_routes
from aibo.server.routes import healthcheck_routes

migrate_documents()

app = FastAPI()

app.include_router(healthcheck_routes.router)
app.include_router(chat_routes.router)
