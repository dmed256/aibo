from typing import cast
from uuid import UUID

from fastapi import APIRouter, Depends, HTTPException, Query, Request, status
from sqlalchemy.ext.asyncio import AsyncSession

from aibo.bouncer.client import BouncerClient, BouncerError
from aibo.common.time import now_utc
from aibo.core import workspace
from aibo.db.client import get_db
from aibo.db.models import LocationModel, ProjectModel
from aibo.server import history, schemas
from aibo.server.delegation import source_content
from aibo.server.dependencies import get_bouncer
from aibo.server.routes.attachments import validate_paths
from aibo.server.routes.common import not_found, publish
from aibo.server.titles import notify_title

router = APIRouter()


@router.get("/chats", response_model=list[schemas.ChatSummary])
async def list_chats(
    query: str | None = None,
    project_id: UUID | None = None,
    limit: int = Query(default=20, ge=1, le=100),
    offset: int = Query(default=0, ge=0),
    unassigned: bool = False,
    db: AsyncSession = Depends(get_db),
) -> list[schemas.ChatSummary]:
    chats = await workspace.list_chats(
        db,
        query=query,
        project_id=project_id,
        limit=limit,
        offset=offset,
        unassigned=unassigned,
    )
    return [schemas.ChatSummary.from_model(chat) for chat in chats]


@router.post("/chats", response_model=schemas.Chat, status_code=status.HTTP_201_CREATED)
async def create_chat(
    request: schemas.CreateChat,
    http_request: Request,
    db: AsyncSession = Depends(get_db),
) -> schemas.Chat:
    if request.location_id and not await db.get(LocationModel, request.location_id):
        raise not_found("location")
    if request.project_id and not await db.get(ProjectModel, request.project_id):
        raise not_found("project")
    user_message, attachments = request.user_message, request.attachments
    if request.source_message_id:
        user_message, attachments = await source_content(db, request.source_message_id)
    else:
        validate_paths(attachments)
    chat = await workspace.create_chat(
        db,
        kind=request.kind,
        title=request.title,
        user_message=user_message,
        attachments=attachments,
        source_message_id=request.source_message_id,
        m_context=request.m_context,
        location_id=request.location_id,
        project_id=request.project_id,
        goal=request.goal,
    )
    await db.commit()
    response = schemas.Chat.from_model(chat)
    if chat.kind != workspace.ChatKind.SHADOW:
        publish(
            http_request,
            {"kind": "chat_created", "chat": response.model_dump(mode="json")},
        )
    return response


@router.get("/chats/by-bot/{number}", response_model=schemas.ChatSummary)
async def get_latest_bot(
    number: int, db: AsyncSession = Depends(get_db)
) -> schemas.ChatSummary:
    if not 0 <= number <= 255:
        raise HTTPException(
            status_code=status.HTTP_422_UNPROCESSABLE_ENTITY,
            detail="bot number must be 0..255",
        )
    chat = await workspace.latest_bot(db, number)
    if not chat:
        raise not_found("chat")
    return schemas.ChatSummary.from_model(chat)


@router.get("/chats/{chat_id}", response_model=schemas.Chat)
async def get_chat(chat_id: UUID, db: AsyncSession = Depends(get_db)) -> schemas.Chat:
    chat = await workspace.get_chat(db, chat_id)
    if not chat:
        raise not_found("chat")
    return schemas.Chat.from_model(chat)


@router.get("/chats/{chat_id}/history", response_model=history.History)
async def get_history(
    chat_id: UUID, before: UUID | None = None, db: AsyncSession = Depends(get_db)
) -> history.History:
    return await history.history(db, chat_id, before)


@router.get("/chats/{chat_id}/details", response_model=history.Details)
async def get_details(
    chat_id: UUID,
    first: UUID,
    last: UUID,
    after: UUID | None = None,
    db: AsyncSession = Depends(get_db),
) -> history.Details:
    return await history.details(db, chat_id, first, last, after)


@router.patch("/chats/{chat_id}", response_model=schemas.ChatSummary)
async def update_chat(
    chat_id: UUID,
    body: schemas.UpdateChat,
    request: Request,
    db: AsyncSession = Depends(get_db),
) -> schemas.ChatSummary:
    chat = await workspace.get_chat(db, chat_id)
    if not chat:
        raise not_found("chat")
    if body.title is not None:
        chat.title = body.title.strip()
    if "project_id" in body.model_fields_set:
        if body.project_id and not await db.get(ProjectModel, body.project_id):
            raise not_found("project")
        chat.project_id = body.project_id
    if body.archived is not None:
        chat.archived_at = now_utc() if body.archived else None
    title_event = {
        "kind": "chat_title_updated",
        "chat_id": str(chat.id),
        "title": chat.title,
    }
    broadcast = await notify_title(db, title_event) if body.title is not None else False
    await db.commit()
    refreshed = await workspace.get_chat(db, chat.id)
    if not refreshed:
        raise not_found("chat")
    response = schemas.ChatSummary.from_model(refreshed)
    publish(request, {"kind": "chat_updated", "chat": response.model_dump(mode="json")})
    if body.title is not None and not broadcast:
        publish(request, title_event)
    return response


@router.post("/chats/{chat_id}/messages", response_model=schemas.Message)
async def create_message(
    chat_id: UUID,
    body: schemas.CreateMessage,
    request: Request,
    db: AsyncSession = Depends(get_db),
) -> schemas.Message:
    chat = await workspace.get_chat(db, chat_id)
    if not chat:
        raise not_found("chat")
    validate_paths(body.data.get("attachments", []))
    message = await workspace.append_message(
        db, chat, kind=body.kind, content=body.content, data=body.data
    )
    await db.commit()
    response = cast(schemas.Message, schemas.Message.model_validate(message))
    publish(
        request,
        {
            "kind": "message_created",
            "chat_id": str(chat.id),
            "message": response.model_dump(mode="json"),
        },
    )
    return response


@router.patch("/chats/{chat_id}/status", response_model=schemas.ChatSummary)
async def update_chat_status(
    chat_id: UUID,
    body: schemas.UpdateStatus,
    request: Request,
    db: AsyncSession = Depends(get_db),
) -> schemas.ChatSummary:
    chat = await workspace.get_chat(db, chat_id)
    if not chat:
        raise not_found("chat")
    changed = chat.status != body.status
    await workspace.set_status(db, chat, body.status)
    if changed or (body.reason and body.reason != workspace.chat_notice(chat)[0]):
        notice = body.reason or workspace.chat_notice(chat)[0]
        if notice:
            await workspace.append_message(
                db,
                chat,
                kind=workspace.MessageKind.EVENT,
                content=notice,
                data={"event": "status_changed", "status": body.status},
            )
            await workspace.create_notification(db, chat=chat, body=notice)
    await db.commit()
    response = schemas.ChatSummary.from_model(chat)
    publish(request, {"kind": "chat_updated", "chat": response.model_dump(mode="json")})
    return response


@router.patch("/chats/{chat_id}/location", response_model=schemas.Chat)
async def update_chat_location(
    chat_id: UUID,
    body: schemas.UpdateChatLocation,
    request: Request,
    db: AsyncSession = Depends(get_db),
    bouncer: BouncerClient = Depends(get_bouncer),
) -> schemas.Chat:
    chat = await workspace.get_chat(db, chat_id)
    if not chat:
        raise not_found("chat")
    if chat.kind == workspace.ChatKind.MANAGER:
        raise HTTPException(
            status_code=422,
            detail="Managers assign locations to bots; managers have no location",
        )
    location = await db.get(LocationModel, body.location_id)
    if not location:
        raise not_found("location")
    if (
        chat.location_id != location.id
        and workspace.ChatStatus(chat.status).active
        and chat.codex_thread_id
        and chat.active_turn_id
    ):
        try:
            await bouncer.interrupt(
                thread_id=chat.codex_thread_id, turn_id=chat.active_turn_id
            )
        except BouncerError as error:
            raise HTTPException(
                status_code=status.HTTP_502_BAD_GATEWAY, detail=str(error)
            ) from error
    await workspace.move_chat(
        db,
        chat,
        location_id=location.id,
        location_name=location.name,
        location_path=location.path,
    )
    await db.commit()
    response = schemas.Chat.from_model(chat)
    publish(request, {"kind": "chat_updated", "chat": response.model_dump(mode="json")})
    return response
