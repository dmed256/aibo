from uuid import UUID

import sqlalchemy as sa
from fastapi import APIRouter, Depends, HTTPException, Request, status
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.orm import selectinload

from aibo.bouncer.client import BouncerClient, BouncerError
from aibo.common.time import now_utc
from aibo.core import workspace
from aibo.db.client import get_db
from aibo.db.models import ChatModel, NotificationModel
from aibo.server import schemas
from aibo.server.account_usage import account_usage
from aibo.server.dependencies import get_bouncer
from aibo.server.routes.common import not_found, publish

router = APIRouter()


@router.post(
    "/notifications",
    response_model=schemas.Notification,
    status_code=status.HTTP_201_CREATED,
)
async def create_notification(
    body: schemas.CreateNotification,
    request: Request,
    db: AsyncSession = Depends(get_db),
) -> schemas.Notification:
    chat = await workspace.get_chat(db, body.chat_id, include_shadow=True)
    if not chat:
        raise not_found("chat")
    try:
        notification = await workspace.create_notification(
            db, chat=chat, body=body.body
        )
    except ValueError as error:
        raise HTTPException(
            status_code=status.HTTP_422_UNPROCESSABLE_ENTITY, detail=str(error)
        ) from error
    await db.commit()
    response = schemas.Notification.from_model(notification)
    publish(
        request,
        {
            "kind": "notification_created",
            "notification": response.model_dump(mode="json"),
        },
    )
    return response


@router.get("/notifications", response_model=schemas.Sidebar)
async def get_sidebar(
    db: AsyncSession = Depends(get_db),
    bouncer: BouncerClient = Depends(get_bouncer),
) -> schemas.Sidebar:
    sidebar = await list_notifications(db)
    try:
        sidebar.account_usage = account_usage(await bouncer.rate_limits())
    except BouncerError:
        # Account telemetry must not prevent reading notifications.
        pass
    return sidebar


async def list_notifications(db: AsyncSession = Depends(get_db)) -> schemas.Sidebar:
    active = list(
        (
            await db.scalars(
                sa.select(ChatModel)
                .where(
                    ChatModel.kind != workspace.ChatKind.SHADOW,
                    ChatModel.status.in_(
                        [workspace.ChatStatus.QUEUED, workspace.ChatStatus.RUNNING]
                    ),
                )
                .order_by(*workspace.chat_order(db), ChatModel.id.desc())
            )
        ).all()
    )
    base = (
        sa.select(NotificationModel)
        .join(NotificationModel.chat)
        .options(selectinload(NotificationModel.chat))
        .where(ChatModel.kind != workspace.ChatKind.SHADOW)
        .order_by(
            *workspace.chat_order(db),
            NotificationModel.created_at.desc(),
            NotificationModel.id.desc(),
        )
    )
    unread = list(
        (await db.scalars(base.where(NotificationModel.read_at.is_(None)))).all()
    )
    read = list(
        (
            await db.scalars(
                base.where(NotificationModel.read_at.is_not(None)).limit(
                    max(0, 50 - len(active) - len(unread))
                )
            )
        ).all()
    )
    read_count = await db.scalar(
        sa.select(sa.func.count())
        .select_from(NotificationModel)
        .join(NotificationModel.chat)
        .where(
            ChatModel.kind != workspace.ChatKind.SHADOW,
            NotificationModel.read_at.is_not(None),
        )
    )
    return schemas.Sidebar(
        active=[schemas.ChatSummary.from_model(chat) for chat in active],
        unread=[schemas.Notification.from_model(item) for item in unread],
        read=[schemas.Notification.from_model(item) for item in read],
        read_count=read_count or 0,
    )


@router.post("/chats/{chat_id}/notifications/read")
async def read_chat_notifications(
    chat_id: UUID,
    request: Request,
    db: AsyncSession = Depends(get_db),
) -> dict[str, int]:
    if not await workspace.get_chat(db, chat_id):
        raise not_found("chat")
    ids = (
        await db.scalars(
            sa.update(NotificationModel)
            .where(
                NotificationModel.chat_id == chat_id,
                NotificationModel.read_at.is_(None),
            )
            .values(read_at=now_utc())
            .returning(NotificationModel.id)
        )
    ).all()
    await db.commit()
    if ids:
        publish(request, {"kind": "notifications_read", "chat_id": str(chat_id)})
    return {"read": len(ids)}


@router.post(
    "/notifications/{notification_id}/read", response_model=schemas.Notification
)
async def read_notification(
    notification_id: UUID,
    request: Request,
    db: AsyncSession = Depends(get_db),
) -> schemas.Notification:
    notification = await db.scalar(
        sa.select(NotificationModel)
        .join(NotificationModel.chat)
        .options(selectinload(NotificationModel.chat))
        .where(
            NotificationModel.id == notification_id,
            ChatModel.kind != workspace.ChatKind.SHADOW,
        )
    )
    if not notification:
        raise not_found("notification")
    notification.read_at = notification.read_at or now_utc()
    await db.commit()
    response = schemas.Notification.from_model(notification)
    publish(
        request,
        {
            "kind": "notification_read",
            "notification": response.model_dump(mode="json"),
        },
    )
    return response
