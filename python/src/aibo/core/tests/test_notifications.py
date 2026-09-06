from collections.abc import AsyncIterator

from fastapi import FastAPI
from httpx import ASGITransport, AsyncClient
from sqlalchemy.ext.asyncio import AsyncSession

from aibo.core import workspace
from aibo.db.client import get_db
from aibo.server.events import BouncerEvents, EventHub
from aibo.server.routes.workspace_routes import router


async def test_visible_api_and_notification_sections(db: AsyncSession) -> None:
    app = FastAPI()
    app.include_router(router)
    hub = EventHub()
    app.state.event_hub = hub

    async def test_db() -> AsyncIterator[AsyncSession]:
        yield db

    app.dependency_overrides[get_db] = test_db
    async with hub.subscribe() as events:
        async with AsyncClient(
            transport=ASGITransport(app=app), base_url="http://test"
        ) as client:
            manager_response = await client.post(
                "/api/chats", json={"kind": "manager", "title": "Visible"}
            )
            manager_event = events.get_nowait()
            shadow_response = await client.post(
                "/api/chats", json={"kind": "shadow", "title": "Hidden"}
            )
            assert events.empty()
            assert manager_response.status_code == 201
            assert shadow_response.status_code == 201
            assert manager_event["kind"] == "chat_created"
            manager_id = manager_response.json()["id"]
            shadow_id = shadow_response.json()["id"]

            response = await client.get("/api/chats")
            assert [chat["id"] for chat in response.json()] == [manager_id]
            assert (await client.get(f"/api/chats/{shadow_id}")).status_code == 404
            hidden_notification = await client.post(
                "/api/notifications", json={"chat_id": shadow_id, "body": "no"}
            )
            assert hidden_notification.status_code == 422

            await client.patch(
                f"/api/chats/{manager_id}/status", json={"status": "running"}
            )
            notification = await client.post(
                "/api/notifications", json={"chat_id": manager_id, "body": "Ready"}
            )
            notification_id = notification.json()["id"]
            sidebar = (await client.get("/api/notifications")).json()
            assert [chat["id"] for chat in sidebar["active"]] == [manager_id]
            assert [item["id"] for item in sidebar["unread"]] == [notification_id]

            await client.post(f"/api/notifications/{notification_id}/read")
            sidebar = (await client.get("/api/notifications")).json()
            assert sidebar["unread"] == []
            assert [item["id"] for item in sidebar["read"]] == [notification_id]


async def test_shadow_result_becomes_origin_notification(db: AsyncSession) -> None:
    origin = await workspace.create_chat(db, kind=workspace.ChatKind.BOT)
    shadow = await workspace.create_chat(db, kind=workspace.ChatKind.SHADOW)
    shadow.origin_chat_id = origin.id
    await workspace.append_message(
        db,
        shadow,
        kind=workspace.MessageKind.ASSISTANT,
        content="b0 updated the server and tests pass.",
    )
    bouncer = BouncerEvents("http://bouncer", EventHub())

    notification = await bouncer._notification_from_shadow(db, shadow)

    assert notification
    assert notification.chat_id == origin.id
    assert notification.source_chat_id == shadow.id
    assert notification.body == "b0 updated the server and tests pass."
    assert await bouncer._notification_from_shadow(db, shadow) is None
    await bouncer.client.close()


async def test_notification_order(db: AsyncSession) -> None:
    import datetime as dt

    from aibo.server.routes.notifications import list_notifications

    earlier = await workspace.create_chat(db, kind=workspace.ChatKind.MANAGER)
    first = await workspace.create_notification(
        db, chat=earlier, body="Old notification"
    )
    later = await workspace.create_chat(db, kind=workspace.ChatKind.BOT)
    second = await workspace.create_notification(
        db, chat=later, body="New notification"
    )
    for chat in (later, earlier):
        await workspace.set_status(db, chat, workspace.ChatStatus.RUNNING)
    user = await workspace.append_message(
        db, earlier, kind=workspace.MessageKind.USER, content="Request"
    )
    final = await workspace.append_message(
        db,
        later,
        kind=workspace.MessageKind.ASSISTANT,
        content="Final",
        data={
            "item": {"phase": "final_answer"},
            "completed_at": (user.created_at + dt.timedelta(seconds=10)).isoformat(),
        },
    )
    await workspace.append_message(
        db,
        earlier,
        kind=workspace.MessageKind.ASSISTANT,
        content="Still working",
        data={"streaming": True},
    )
    await workspace.append_message(
        db,
        earlier,
        kind=workspace.MessageKind.ASSISTANT,
        content="Commentary",
        data={"item": {"phase": "commentary"}},
    )
    sidebar = await list_notifications(db)
    assert [chat.id for chat in sidebar.active] == [later.id, earlier.id]
    assert [item.id for item in sidebar.unread] == [second.id, first.id]
    first.read_at = second.read_at = workspace.now_utc()
    await db.flush()
    assert [item.id for item in (await list_notifications(db)).read] == [
        second.id,
        first.id,
    ]
    # Active wins over a newer final answer, regardless of notification creation.
    await workspace.set_status(db, later, workspace.ChatStatus.COMPLETED)
    sidebar = await list_notifications(db)
    assert [chat.id for chat in sidebar.active] == [earlier.id]
    assert [item.id for item in sidebar.read] == [first.id, second.id]
    first.read_at = second.read_at = None
    await db.flush()
    assert [item.id for item in (await list_notifications(db)).unread] == [
        first.id,
        second.id,
    ]
    # Once both finish, last message time wins again; status time is irrelevant.
    await workspace.set_status(db, earlier, workspace.ChatStatus.COMPLETED)
    assert [item.id for item in (await list_notifications(db)).unread] == [
        second.id,
        first.id,
    ]
    user.created_at = dt.datetime.fromisoformat(
        str(final.data["completed_at"])
    ) + dt.timedelta(seconds=1)
    await db.flush()
    assert [item.id for item in (await list_notifications(db)).unread] == [
        first.id,
        second.id,
    ]


async def test_notification_order_before_limit(db: AsyncSession) -> None:
    from aibo.server.routes.notifications import list_notifications

    active = await workspace.create_chat(db, kind=workspace.ChatKind.BOT)
    first = await workspace.create_notification(db, chat=active, body="Old but active")
    first.read_at = workspace.now_utc()
    finished = await workspace.create_chat(db, kind=workspace.ChatKind.BOT)
    for _ in range(55):
        notification = await workspace.create_notification(
            db, chat=finished, body="Newer notification"
        )
        notification.read_at = workspace.now_utc()
    await workspace.set_status(db, active, workspace.ChatStatus.RUNNING)
    sidebar = await list_notifications(db)
    assert len(sidebar.read) == 49
    assert sidebar.read_count == 56
    assert sidebar.read[0].id == first.id


async def test_sidebar_read_budget(db: AsyncSession) -> None:
    from aibo.server.routes.notifications import list_notifications

    chat = await workspace.create_chat(db, kind=workspace.ChatKind.MANAGER)
    await workspace.set_status(db, chat, workspace.ChatStatus.RUNNING)
    for index in range(55):
        notification = await workspace.create_notification(
            db, chat=chat, body=f"Read {index}"
        )
        notification.read_at = workspace.now_utc()
    unread = await workspace.create_notification(db, chat=chat, body="Unread")
    await db.flush()
    sidebar = await list_notifications(db)
    assert sidebar.read_count == 55
    assert len(sidebar.active) == len(sidebar.unread) == 1
    assert [item.body for item in sidebar.read] == [
        f"Read {index}" for index in range(54, 6, -1)
    ]
    unread.read_at = workspace.now_utc()
    await db.flush()
    assert len((await list_notifications(db)).read) == 49
    for index in range(51):
        await workspace.create_notification(db, chat=chat, body=f"Unread {index}")
    sidebar = await list_notifications(db)
    assert len(sidebar.active) == 1
    assert len(sidebar.unread) == 51
    assert sidebar.read == []


async def test_read_chat_notifications(db: AsyncSession) -> None:
    chats = [
        await workspace.create_chat(db, kind=workspace.ChatKind.MANAGER)
        for _ in range(2)
    ]
    for chat in chats:
        for _ in range(2):
            await workspace.create_notification(db, chat=chat, body="Ready")
    await db.commit()
    app = FastAPI()
    app.include_router(router)

    async def test_db() -> AsyncIterator[AsyncSession]:
        yield db

    app.dependency_overrides[get_db] = test_db
    async with AsyncClient(
        transport=ASGITransport(app=app), base_url="http://test"
    ) as client:
        path = f"/api/chats/{chats[0].id}/notifications/read"
        assert (await client.post(path)).json() == {"read": 2}
        assert (await client.post(path)).json() == {"read": 0}
        sidebar = (await client.get("/api/notifications")).json()
        assert len(sidebar["unread"]) == 2
        assert all(item["chat"]["id"] == str(chats[1].id) for item in sidebar["unread"])
