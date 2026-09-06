from __future__ import annotations

import asyncio
import json
import logging
from collections.abc import Callable
from typing import AsyncContextManager
from uuid import UUID

import sqlalchemy as sa
from sqlalchemy.ext.asyncio import AsyncSession

from aibo.bouncer.client import BouncerClient
from aibo.core.settings import get_models
from aibo.core.titles import clean_title, needs_title
from aibo.core.workspace import NOTIFICATION_CHANNEL
from aibo.db.client import get_session
from aibo.db.models import ChatModel
from aibo.server.events import EventHub

logger = logging.getLogger(__name__)


async def notify_title(db: AsyncSession, event: dict[str, str]) -> bool:
    """Broadcast on commit through the existing cross-worker event listener."""
    if db.get_bind().dialect.name != "postgresql":
        return False
    await db.execute(
        sa.select(sa.func.pg_notify(NOTIFICATION_CHANNEL, json.dumps(event)))
    )
    return True


class TitleGenerator:
    def __init__(
        self,
        client: BouncerClient,
        hub: EventHub,
        session: Callable[[], AsyncContextManager[AsyncSession]] = get_session,
    ) -> None:
        self.client = client
        self.hub = hub
        self.session = session
        self.tasks: dict[UUID, asyncio.Task[None]] = {}

    def schedule(self, chat_id: UUID, user_message: str) -> None:
        if not user_message.strip() or chat_id in self.tasks:
            return
        task = asyncio.create_task(self.generate(chat_id, user_message))
        self.tasks[chat_id] = task
        task.add_done_callback(lambda _: self.tasks.pop(chat_id, None))

    async def generate(self, chat_id: UUID, user_message: str) -> None:
        try:
            async with self.session() as db:
                original = await db.scalar(
                    sa.select(ChatModel.title).where(ChatModel.id == chat_id)
                )
                if original is None or not needs_title(original):
                    return
                models = await get_models(db)
            title = clean_title(
                await self.client.generate_title(
                    user_message,
                    models.title_model,
                    models.title_model_reasoning_effort,
                )
            )
            if not title:
                return
            async with self.session() as db:
                # Never overwrite a title assigned while generation was running.
                updated = await db.scalar(
                    sa.update(ChatModel)
                    .where(ChatModel.id == chat_id, ChatModel.title == original)
                    .values(title=title)
                    .returning(ChatModel.id)
                )
                event = {
                    "kind": "chat_title_updated",
                    "chat_id": str(chat_id),
                    "title": title,
                }
                broadcast = await notify_title(db, event) if updated else False
                await db.commit()
            if updated and not broadcast:
                self.hub.publish(event)
        except Exception:
            logger.exception("Could not generate title for %s", chat_id)

    async def close(self) -> None:
        tasks = list(self.tasks.values())
        for task in tasks:
            task.cancel()
        await asyncio.gather(*tasks, return_exceptions=True)
        await self.client.close()
