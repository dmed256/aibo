from typing import cast
from uuid import UUID

import sqlalchemy as sa
from fastapi import APIRouter, Depends, HTTPException, status
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.orm import selectinload

from aibo.bouncer.client import BouncerClient, BouncerError
from aibo.common.runtime_files import ensure_project_readme
from aibo.common.time import now_utc
from aibo.core import workspace
from aibo.db.client import get_db
from aibo.db.models import ChatModel, LocationModel, ProjectModel
from aibo.server import schemas
from aibo.server.dependencies import get_bouncer
from aibo.server.routes.common import normalized_path, not_found

router = APIRouter()


@router.get("/locations", response_model=list[schemas.Location])
async def list_locations(db: AsyncSession = Depends(get_db)) -> list[LocationModel]:
    return list(
        (await db.scalars(sa.select(LocationModel).order_by(LocationModel.name))).all()
    )


@router.post(
    "/locations", response_model=schemas.Location, status_code=status.HTTP_201_CREATED
)
async def create_location(
    request: schemas.CreateLocation, db: AsyncSession = Depends(get_db)
) -> LocationModel:
    if await db.scalar(
        sa.select(LocationModel.id).where(LocationModel.name == request.name)
    ):
        raise HTTPException(
            status_code=status.HTTP_409_CONFLICT, detail="location name exists"
        )
    location = LocationModel(name=request.name, path=normalized_path(request.path))
    db.add(location)
    await db.commit()
    return location


@router.patch("/locations/{location_id}", response_model=schemas.Location)
async def update_location(
    location_id: UUID,
    request: schemas.UpdateLocation,
    db: AsyncSession = Depends(get_db),
    bouncer: BouncerClient = Depends(get_bouncer),
) -> LocationModel:
    location = cast(LocationModel | None, await db.get(LocationModel, location_id))
    if not location:
        raise not_found("location")
    path = normalized_path(request.path)
    if path == location.path:
        return location
    chats = list(
        (
            await db.scalars(
                sa.select(ChatModel)
                .options(selectinload(ChatModel.messages))
                .where(
                    ChatModel.location_id == location.id,
                    ChatModel.kind != workspace.ChatKind.MANAGER,
                )
            )
        ).all()
    )
    for chat in chats:
        if (
            workspace.ChatStatus(chat.status).active
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
        await workspace.note_location_change(
            db,
            chat,
            location_id=location.id,
            location_name=location.name,
            location_path=path,
        )
    location.path = path
    await db.commit()
    return location


@router.get("/projects", response_model=list[schemas.Project])
async def list_projects(
    archived: bool = False, db: AsyncSession = Depends(get_db)
) -> list[ProjectModel]:
    if archived:
        query = (
            sa.select(ProjectModel)
            .where(ProjectModel.archived_at.is_not(None))
            .order_by(ProjectModel.archived_at.desc())
        )
    else:
        query = (
            sa.select(ProjectModel)
            .where(ProjectModel.archived_at.is_(None))
            .order_by(ProjectModel.name.desc())
        )
    return list((await db.scalars(query)).all())


@router.post(
    "/projects", response_model=schemas.Project, status_code=status.HTTP_201_CREATED
)
async def create_project(
    request: schemas.CreateProject, db: AsyncSession = Depends(get_db)
) -> ProjectModel:
    if await db.scalar(
        sa.select(ProjectModel.id).where(ProjectModel.name == request.name)
    ):
        raise HTTPException(
            status_code=status.HTTP_409_CONFLICT, detail="project name exists"
        )
    project = ProjectModel(name=request.name, description=request.description)
    db.add(project)
    await db.commit()
    ensure_project_readme(project.name)
    return project


@router.patch("/projects/{project_id}", response_model=schemas.Project)
async def update_project(
    project_id: UUID,
    request: schemas.UpdateProject,
    db: AsyncSession = Depends(get_db),
) -> ProjectModel:
    project = cast(ProjectModel | None, await db.get(ProjectModel, project_id))
    if not project:
        raise not_found("project")
    if request.description is not None:
        project.description = request.description
    if request.archived is not None:
        project.archived_at = now_utc() if request.archived else None
    await db.commit()
    return project
