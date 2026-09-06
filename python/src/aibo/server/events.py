from __future__ import annotations

import asyncio
import datetime as dt
import json
import logging
from collections.abc import AsyncIterator
from contextlib import asynccontextmanager, suppress
from typing import Any
from uuid import UUID

import sqlalchemy as sa
from sqlalchemy.ext.asyncio import AsyncEngine, AsyncSession
from sqlalchemy.orm import aliased, selectinload
from websockets.client import connect

from aibo.bouncer.client import BouncerClient, BouncerError
from aibo.core import workspace
from aibo.db.client import get_async_engine, get_session
from aibo.db.models import ChatMessageModel, ChatModel, NotificationModel

logger = logging.getLogger(__name__)


def latest_completion(chat: ChatModel) -> ChatMessageModel | None:
    return next(
        (
            m
            for m in reversed(chat.messages)
            if m.data.get("event") == "turn/completed"
            and m.data.get("current_turn", True)
        ),
        None,
    )


class EventHub:
    def __init__(self) -> None:
        self._subscribers: set[asyncio.Queue[dict[str, Any]]] = set()

    def publish(self, event: dict[str, Any]) -> None:
        for queue in tuple(self._subscribers):
            queue.put_nowait(event)

    @asynccontextmanager
    async def subscribe(self) -> AsyncIterator[asyncio.Queue[dict[str, Any]]]:
        queue: asyncio.Queue[dict[str, Any]] = asyncio.Queue()
        self._subscribers.add(queue)
        try:
            yield queue
        finally:
            self._subscribers.discard(queue)


class BouncerEvents:
    def __init__(self, bouncer_url: str, hub: EventHub) -> None:
        self.url = (
            bouncer_url.replace("http://", "ws://", 1).replace("https://", "wss://", 1)
            + "/events"
        )
        self.hub = hub
        self.client = BouncerClient(bouncer_url)

    async def run(self) -> None:
        delay = 0.25
        notifications = asyncio.create_task(self._watch_notifications())
        maintenance = asyncio.create_task(self._maintain_notifications())
        try:
            while True:
                try:
                    async with connect(self.url, max_size=None) as websocket:
                        delay = 0.25
                        await self._reconcile()
                        await self._repair_notifications()
                        async for raw_event in websocket:
                            event = json.loads(raw_event)
                            visible_event = await self._persist(event)
                            if visible_event:
                                self.hub.publish(visible_event)
                except asyncio.CancelledError:
                    raise
                except Exception:
                    logger.exception("Bouncer event stream disconnected")
                    await asyncio.sleep(delay)
                    delay = min(delay * 2, 10.0)
        finally:
            notifications.cancel()
            maintenance.cancel()
            with suppress(asyncio.CancelledError):
                await notifications
            with suppress(asyncio.CancelledError):
                await maintenance
            await self.client.close()

    @asynccontextmanager
    async def _notification_listener(
        self, engine: AsyncEngine
    ) -> AsyncIterator[asyncio.Event]:
        """LISTEN on a dedicated connection; delivery occurs only after commit."""
        async with engine.connect() as connection:
            channel = await connection.scalar(sa.select(workspace.NOTIFICATION_CHANNEL))
            await connection.commit()
            raw = await connection.get_raw_connection()
            driver = raw.driver_connection
            closed = asyncio.Event()

            def received(
                _connection: Any, _pid: int, _channel: str, payload: str
            ) -> None:
                self.hub.publish(json.loads(payload))

            def disconnected(_connection: Any) -> None:
                closed.set()

            driver.add_termination_listener(disconnected)
            try:
                await driver.add_listener(channel, received)
                yield closed
            finally:
                driver.remove_termination_listener(disconnected)
                if not driver.is_closed():
                    await driver.remove_listener(channel, received)

    async def _watch_notifications(self) -> None:
        delay = 0.25
        while True:
            try:
                async with self._notification_listener(get_async_engine()) as closed:
                    # Reload persisted state after establishing LISTEN, closing its reconnect gap.
                    self.hub.publish({"kind": "workspace_changed"})
                    delay = 0.25
                    await closed.wait()
            except asyncio.CancelledError:
                raise
            except Exception:
                logger.exception("Notification listener disconnected")
            await asyncio.sleep(delay)
            delay = min(delay * 2, 10.0)

    async def _reconcile(self) -> None:
        async with get_session() as db:
            active_chats = list(
                (
                    await db.scalars(
                        sa.select(ChatModel).where(
                            sa.or_(
                                ChatModel.status.in_(
                                    [
                                        workspace.ChatStatus.QUEUED,
                                        workspace.ChatStatus.RUNNING,
                                    ]
                                ),
                                sa.and_(
                                    ChatModel.kind == workspace.ChatKind.SHADOW,
                                    ChatModel.status == workspace.ChatStatus.IDLE,
                                ),
                                ChatModel.goal["status"].as_string() == "active",
                            ),
                            ChatModel.codex_thread_id.is_not(None),
                        )
                    )
                ).all()
            )

        for chat in active_chats:
            thread_id = chat.codex_thread_id
            if not thread_id:
                continue
            try:
                response = await self.client.resume_thread(thread_id)
                if "goal" in response and response["goal"] != chat.goal:
                    goal = response["goal"]
                    visible = await self._persist(
                        {
                            "method": "thread/goal/updated"
                            if goal
                            else "thread/goal/cleared",
                            "params": {"threadId": thread_id, "goal": goal},
                        }
                    )
                    if visible:
                        self.hub.publish(visible)
                thread = response["thread"]
                turns = thread.get("turns") or []
                last_turn = turns[-1] if turns else None
                finished = last_turn and last_turn.get("status") in {
                    "completed",
                    "interrupted",
                    "failed",
                }
                if finished:
                    for item in last_turn.get("items") or []:
                        item_event = {
                            "method": "item/completed",
                            "params": {
                                "threadId": thread_id,
                                "turnId": last_turn.get("id"),
                                "item": item,
                            },
                        }
                        visible_event = await self._persist(item_event)
                        if visible_event:
                            self.hub.publish(visible_event)
                if finished:
                    event = {
                        "method": "turn/completed",
                        "params": {"threadId": thread_id, "turn": last_turn},
                    }
                    visible_event = await self._persist(event)
                    if visible_event:
                        self.hub.publish(visible_event)
                elif (thread.get("status") or {}).get("type") == "active":
                    async with get_session() as db:
                        current = await db.scalar(
                            sa.select(ChatModel)
                            .where(ChatModel.id == chat.id)
                            .with_for_update()
                        )
                        turn_id = last_turn.get("id") if last_turn else None
                        finished_elsewhere = turn_id and await self._has_message(
                            db, chat.id, f"turn:{turn_id}:completed"
                        )
                        if (
                            current
                            and not finished_elsewhere
                            and current.active_turn_id in (None, turn_id)
                        ):
                            if last_turn:
                                current.active_turn_id = last_turn.get("id")
                            await workspace.set_status(
                                db, current, workspace.ChatStatus.RUNNING
                            )
                            await db.commit()
            except (BouncerError, KeyError, TypeError) as error:
                logger.warning("Could not reconcile %s: %s", thread_id, error)

    async def _maintain_notifications(self) -> None:
        while True:
            await asyncio.sleep(30)
            try:
                await self._reconcile()
                await self._repair_notifications()
            except asyncio.CancelledError:
                raise
            except Exception:
                logger.exception("Could not repair notifications")

    async def _repair_notifications(self) -> None:
        """Recover committed completions and abandoned shadow startup claims."""
        shadow = aliased(ChatModel)
        async with get_session() as db:
            missing = (
                await db.execute(
                    sa.select(ChatMessageModel.chat_id, ChatMessageModel.data)
                    .join(ChatModel, ChatMessageModel.chat_id == ChatModel.id)
                    .where(
                        ChatModel.kind != workspace.ChatKind.SHADOW,
                        ChatMessageModel.external_id.is_not(None),
                        ChatMessageModel.data["event"].as_string() == "turn/completed",
                        ChatMessageModel.data["notification_ready"]
                        .as_boolean()
                        .is_(True),
                        ~sa.exists().where(
                            shadow.shadow_for_turn_id
                            == ChatMessageModel.data["turn"]["id"].as_string()
                        ),
                    )
                    .order_by(ChatMessageModel.created_at.desc())
                    .limit(100)
                )
            ).all()
            abandoned = list(
                (
                    await db.scalars(
                        sa.select(ChatModel.id)
                        .where(
                            ChatModel.kind == workspace.ChatKind.SHADOW,
                            ChatModel.origin_chat_id.is_not(None),
                            ChatModel.shadow_for_turn_id.is_not(None),
                            ~sa.exists().where(
                                NotificationModel.source_chat_id == ChatModel.id
                            ),
                            ChatModel.status.in_(
                                [workspace.ChatStatus.IDLE, workspace.ChatStatus.QUEUED]
                            ),
                            ChatModel.last_status_change_at
                            < workspace.now_utc() - dt.timedelta(minutes=2),
                        )
                        .order_by(ChatModel.last_status_change_at)
                        .limit(100)
                    )
                ).all()
            )
        for chat_id, data in missing:
            await self._launch_shadow(chat_id, data["turn"])
        for shadow_id in abandoned:
            async with get_session() as db:
                await self._fail_shadow(
                    db, shadow_id, "Shadow startup was interrupted", abandoned=True
                )
                await db.commit()

    async def _fail_shadow(
        self, db: AsyncSession, shadow_id: UUID, reason: str, *, abandoned: bool = False
    ) -> NotificationModel | None:
        query = sa.select(ChatModel).where(ChatModel.id == shadow_id)
        if abandoned:
            query = query.where(
                ChatModel.status.in_(
                    [workspace.ChatStatus.IDLE, workspace.ChatStatus.QUEUED]
                ),
                ChatModel.last_status_change_at
                < workspace.now_utc() - dt.timedelta(minutes=2),
            )
        shadow = await db.scalar(
            query.with_for_update().execution_options(populate_existing=True)
        )
        if not shadow or not shadow.origin_chat_id:
            return None
        existing: NotificationModel | None = await db.scalar(
            sa.select(NotificationModel).where(
                NotificationModel.source_chat_id == shadow.id
            )
        )
        if existing:
            return existing
        if shadow.status == workspace.ChatStatus.RUNNING:
            return None
        origin = await db.get(ChatModel, shadow.origin_chat_id)
        if not origin:
            return None
        request = next(
            (
                message
                for message in shadow.messages
                if message.kind == workspace.MessageKind.USER
            ),
            None,
        )
        outcome = request.data.get("outcome", "") if request else ""
        if not isinstance(outcome, str):
            outcome = ""
        body = next(
            (
                line.strip().lstrip("# ")
                for line in outcome.splitlines()
                if line.strip()
            ),
            "",
        )[:240]
        if not body:
            body = (
                f"{workspace.chat_label(origin)} notification could not be summarized"
            )
        source_status = request.data.get("source_status") if request else None
        if source_status and source_status != "completed":
            body = f"{workspace.chat_label(origin)} {source_status}: {body}"
        await workspace.set_status(db, shadow, workspace.ChatStatus.ERROR)
        await workspace.append_message(
            db,
            shadow,
            kind=workspace.MessageKind.EVENT,
            content=reason,
            data={"event": "notification_failed"},
        )
        return await workspace.create_notification(
            db, chat=origin, body=body, source_chat_id=shadow.id
        )

    async def _persist(self, event: dict[str, Any]) -> dict[str, Any] | None:
        method = event.get("method")
        # Also reject deltas from an older bouncer during rolling upgrades.
        if method and method.lower().endswith("delta"):
            return None
        params = event.get("params") or {}
        thread_id = params.get("threadId")
        if not method or not thread_id:
            return {"kind": "codex", "method": method, "params": params}

        async with get_session() as db:
            chat = await db.scalar(
                sa.select(ChatModel)
                .where(ChatModel.codex_thread_id == thread_id)
                .limit(1)
                .with_for_update()
            )
            if not chat:
                return None
            notification = await self._apply(
                db,
                chat,
                method,
                params,
            )
            await db.commit()
            if chat.kind == workspace.ChatKind.SHADOW:
                if not notification and method == "turn/completed":
                    notification = await db.scalar(
                        sa.select(NotificationModel).where(
                            NotificationModel.source_chat_id == chat.id
                        )
                    )
                if notification:
                    return {
                        "kind": "notification_created",
                        "notification_id": str(notification.id),
                        "chat_id": str(notification.chat_id),
                    }
                return None
            visible_event = {
                "kind": "codex",
                "method": method,
                "chat_id": str(chat.id),
                "params": params,
            }
            if method == "turn/completed":
                await self._launch_shadow(chat.id, params["turn"])
            elif method == "thread/goal/updated" and not chat.active_turn_id:
                completed = latest_completion(chat)
                if completed and completed.data.get("notification_ready"):
                    await self._launch_shadow(chat.id, completed.data["turn"])
            return visible_event

    async def _apply(
        self,
        db: AsyncSession,
        chat: ChatModel,
        method: str,
        params: dict[str, Any],
    ) -> NotificationModel | None:
        if method == "thread/goal/updated":
            chat.goal = params["goal"]
            chat.goal_enabled = True
            # A goal can finish after its last turn, including during reconnect.
            # Promote only that goal's latest turn, never its intermediate history.
            if (
                chat.goal.get("status") in ("complete", "blocked")
                and not chat.active_turn_id
            ):
                completed = latest_completion(chat)
                previous_goal = (completed.data.get("goal") or {}) if completed else {}
                if (
                    completed
                    and previous_goal.get("status") in ("active", "pending", "paused")
                    and previous_goal.get("objective") == chat.goal.get("objective")
                    and previous_goal.get("createdAt") == chat.goal.get("createdAt")
                ):
                    completed.data = {**completed.data, "notification_ready": True}
            return None
        if method == "thread/goal/cleared":
            chat.goal = None
            chat.goal_enabled = False
            return None
        if method == "thread/tokenUsage/updated":
            chat.tokens_used = params["tokenUsage"]["total"]["totalTokens"]
            return None
        if method == "turn/started":
            if await self._has_message(
                db, chat.id, f"turn:{params['turn']['id']}:completed"
            ):
                return None
            chat.active_turn_id = params["turn"]["id"]
            await workspace.set_status(db, chat, workspace.ChatStatus.RUNNING)
            return None

        if method == "turn/completed":
            turn = params["turn"]
            status = {
                "completed": workspace.ChatStatus.COMPLETED,
                "interrupted": workspace.ChatStatus.INTERRUPTED,
                "failed": workspace.ChatStatus.ERROR,
            }.get(turn.get("status"), workspace.ChatStatus.ERROR)
            turn_external_id = f"turn:{turn.get('id')}:completed"
            if not await self._has_message(db, chat.id, turn_external_id):
                current_turn = chat.active_turn_id or next(
                    (
                        message.data.get("turn_id")
                        for message in reversed(chat.messages)
                        if message.data.get("event") == "turn_started"
                    ),
                    None,
                )
                if current_turn is None or current_turn == turn.get("id"):
                    chat.active_turn_id = None
                    await workspace.set_status(db, chat, status)
                kind = workspace.MessageKind.EVENT
                content = f"Codex turn {turn.get('status', 'ended')}"
                if status == workspace.ChatStatus.ERROR:
                    kind = workspace.MessageKind.ERROR
                    detail = (turn.get("error") or {}).get("message")
                    content = (
                        f"The turn could not continue: {str(detail).rstrip('.')}."
                        if detail
                        else "The turn could not continue."
                    )
                await workspace.append_message(
                    db,
                    chat,
                    kind=kind,
                    content=content,
                    data={
                        "event": method,
                        "turn": turn,
                        "goal": chat.goal,
                        "current_turn": current_turn is None
                        or current_turn == turn.get("id"),
                        "notification_ready": (
                            (current_turn is None or current_turn == turn.get("id"))
                            and (
                                status != workspace.ChatStatus.COMPLETED
                                or not chat.goal
                                or chat.goal.get("status")
                                not in ("active", "pending", "paused")
                            )
                        ),
                    },
                    external_id=turn_external_id,
                )
            if chat.kind == workspace.ChatKind.SHADOW:
                return await self._notification_from_shadow(db, chat)
            return None

        if method != "item/completed":
            return None
        item = params.get("item") or {}
        item_type = item.get("type", "unknown")
        if item_type == "userMessage":
            return None
        item_external_id = f"item:{item['id']}" if item.get("id") else None
        if item_external_id:
            existing = await db.scalar(
                sa.select(ChatMessageModel).where(
                    ChatMessageModel.chat_id == chat.id,
                    ChatMessageModel.external_id == item_external_id,
                )
            )
            if existing:
                if existing.data.get("streaming") and item_type == "agentMessage":
                    existing.content = item.get("text", "")
                    existing.data = {
                        "event": method,
                        "completed_at": workspace.now_utc().isoformat(),
                        "turn_id": params.get("turnId"),
                        "item": item,
                    }
                return None
        if item_type == "agentMessage":
            kind = workspace.MessageKind.ASSISTANT
            content = item.get("text", "")
        else:
            kind = workspace.MessageKind.EVENT
            content = item_type
        await workspace.append_message(
            db,
            chat,
            kind=kind,
            content=content,
            data={"event": method, "turn_id": params.get("turnId"), "item": item},
            external_id=item_external_id,
        )
        return None

    async def _has_message(
        self, db: AsyncSession, chat_id: UUID, external_id: str
    ) -> bool:
        return bool(
            await db.scalar(
                sa.select(ChatMessageModel.id).where(
                    ChatMessageModel.chat_id == chat_id,
                    ChatMessageModel.external_id == external_id,
                )
            )
        )

    async def _notification_from_shadow(
        self, db: AsyncSession, shadow: ChatModel
    ) -> NotificationModel | None:
        if not shadow.origin_chat_id:
            return None
        existing = await db.scalar(
            sa.select(NotificationModel).where(
                NotificationModel.source_chat_id == shadow.id
            )
        )
        if existing:
            return None
        origin = await db.get(ChatModel, shadow.origin_chat_id)
        if not origin:
            return None
        body = await db.scalar(
            sa.select(ChatMessageModel.content)
            .where(
                ChatMessageModel.chat_id == shadow.id,
                ChatMessageModel.kind == workspace.MessageKind.ASSISTANT,
            )
            .order_by(ChatMessageModel.created_at.desc())
            .limit(1)
        )
        if not body:
            body = (
                f"{workspace.chat_label(origin)} notification could not be summarized"
            )
        return await workspace.create_notification(
            db, chat=origin, body=body, source_chat_id=shadow.id
        )

    async def _launch_shadow(self, source_chat_id: UUID, turn: dict[str, Any]) -> None:
        turn_id = str(turn["id"])
        async with get_session() as db:
            source = await db.scalar(
                sa.select(ChatModel)
                .options(
                    selectinload(ChatModel.messages), selectinload(ChatModel.location)
                )
                .where(ChatModel.id == source_chat_id)
                .with_for_update()
            )
            if not source:
                return
            completed = next(
                (
                    m
                    for m in source.messages
                    if m.external_id == f"turn:{turn_id}:completed"
                ),
                None,
            )
            if not completed or not completed.data.get("notification_ready"):
                return
            existing = await db.scalar(
                sa.select(ChatModel.id).where(ChatModel.shadow_for_turn_id == turn_id)
            )
            if existing:
                return
            final_message = next(
                (
                    message.content
                    for message in reversed(source.messages)
                    if message.kind == workspace.MessageKind.ASSISTANT
                    and message.data.get("turn_id") == turn_id
                ),
                "",
            )
            prompt = (
                f"{workspace.chat_label(source)} ended with status "
                f"{turn.get('status', 'unknown')}. Write its notification.\n\n"
                f"Final assistant message:\n{final_message or 'No final assistant message was recorded.'}"
            )
            shadow = await workspace.create_chat(
                db,
                kind=workspace.ChatKind.SHADOW,
                title=f"Notification for {workspace.chat_label(source)}",
                user_message=prompt,
                location_id=source.location_id,
                project_id=source.project_id,
            )
            shadow.origin_chat_id = source.id
            shadow.shadow_for_turn_id = turn_id
            shadow.messages[-1].data = {
                "outcome": final_message,
                "source_status": turn.get("status"),
            }
            await workspace.set_status(db, shadow, workspace.ChatStatus.QUEUED)
            await db.commit()

            try:
                if not source.location:
                    raise BouncerError("source chat has no location")
                response = await asyncio.wait_for(
                    self.client.start_thread(
                        cwd=source.location.path,
                        instructions=shadow.messages[0].content,
                    ),
                    timeout=30,
                )
                await db.refresh(shadow, with_for_update=True)
                if shadow.status != workspace.ChatStatus.QUEUED:
                    return
                shadow.codex_thread_id = response["thread"]["id"]
                await db.commit()
                thread_id = shadow.codex_thread_id
                if not thread_id:
                    raise KeyError("thread.id")
                response = await asyncio.wait_for(
                    self.client.start_turn(
                        thread_id=thread_id, text=prompt, attachments=[]
                    ),
                    timeout=30,
                )
                await db.refresh(shadow, with_for_update=True)
                if shadow.status != workspace.ChatStatus.QUEUED:
                    return
                shadow.active_turn_id = response["turn"]["id"]
                await workspace.set_status(db, shadow, workspace.ChatStatus.RUNNING)
                await db.commit()
            except (BouncerError, KeyError, TimeoutError) as error:
                notification = await self._fail_shadow(
                    db, shadow.id, str(error) or "Shadow startup timed out"
                )
                await db.commit()
                if notification:
                    self.hub.publish(
                        {
                            "kind": "notification_created",
                            "notification_id": str(notification.id),
                            "chat_id": str(source.id),
                        }
                    )
