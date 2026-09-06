from collections.abc import AsyncIterator
from pathlib import Path

from fastapi import FastAPI
from httpx import ASGITransport, AsyncClient
from sqlalchemy.ext.asyncio import AsyncSession

from aibo.common.constants import Env
from aibo.common.runtime_files import ensure_project_readme, role_instructions
from aibo.core import workspace
from aibo.core.tests.fakes import FakeBouncer
from aibo.db.client import get_db
from aibo.db.models import LocationModel, ProjectModel
from aibo.server.dependencies import get_bouncer
from aibo.server.routes.workspace_routes import router


async def test_delegation_and_location_move(db: AsyncSession) -> None:
    old_location = LocationModel(name="old", path="/tmp/old")
    new_location = LocationModel(name="aibo", path="/tmp/aibo")
    db.add_all([old_location, new_location])
    await db.flush()
    chat = await workspace.create_chat(
        db,
        kind=workspace.ChatKind.BOT,
        user_message="Fix the server",
        m_context="Use the aibo project",
        location_id=old_location.id,
    )
    await workspace.set_status(db, chat, workspace.ChatStatus.RUNNING)

    await workspace.move_chat(
        db,
        chat,
        location_id=new_location.id,
        location_name=new_location.name,
        location_path=new_location.path,
    )

    assert chat.status == workspace.ChatStatus.INTERRUPTED
    assert chat.messages[0].kind == workspace.MessageKind.SYSTEM
    assert chat.messages[1].kind == workspace.MessageKind.SYSTEM
    assert chat.messages[2].content == (
        "# user message\n\nFix the server\n\n" "# m context\n\nUse the aibo project"
    )
    assert chat.messages[-1].content == "Moved to the aibo location (/tmp/aibo)"
    assert chat.messages[-1].data["event"] == "location_changed"


async def test_delegated_turn_context(db: AsyncSession, tmp_path: Path) -> None:
    location = LocationModel(name="aibo", path=str(tmp_path))
    project = ProjectModel(name="aibo", description="Persistent workspace")
    db.add_all([location, project])
    await db.commit()
    readme = ensure_project_readme("aibo")
    readme.write_text("# Interface\nPreserve the three windows.\n")
    bouncer = FakeBouncer()
    app = FastAPI()
    app.include_router(router)

    async def test_db() -> AsyncIterator[AsyncSession]:
        yield db

    app.dependency_overrides[get_db] = test_db
    app.dependency_overrides[get_bouncer] = lambda: bouncer
    async with AsyncClient(
        transport=ASGITransport(app=app), base_url="http://test"
    ) as client:
        created = (
            await client.post(
                "/api/chats",
                json={
                    "kind": "bot",
                    "location_id": str(location.id),
                    "project_id": str(project.id),
                    "user_message": "  Fix the sidebar.\nKeep 猫.\n",
                    "m_context": "Preserve scroll",
                },
            )
        ).json()
        path = f"/api/chats/{created['id']}"
        expected = "# user message\n\n  Fix the sidebar.\nKeep 猫.\n\n\n# m context\n\nPreserve scroll"
        assert [message["kind"] for message in created["messages"]] == [
            "system",
            "system",
            "user",
        ]
        assert created["messages"][-1]["content"] == expected
        sent = await client.post(
            path + "/submit", json={"message_id": created["messages"][-1]["id"]}
        )
        assert sent.status_code == 200
        instructions = bouncer.started[0]["instructions"]
        assert instructions == role_instructions("bot")
        assert bouncer.turns[0]["text"] == expected
        context = str(bouncer.turns[0]["context"])
        for piece in [
            str(readme),
            "Persistent workspace",
            "Preserve the three windows.",
            str(tmp_path),
        ]:
            assert piece in context
        assert (
            len(
                [
                    message
                    for message in sent.json()["chat"]["messages"]
                    if message["kind"] == "user"
                ]
            )
            == 1
        )

        readme.write_text("# Interface\nKeep the sidebar reachable.\n")
        resumed = await client.post(
            path + "/submit",
            json={"text": "  Continue  ", "m_context": "Use the updated instructions"},
        )
        assert resumed.status_code == 200
        assert (
            bouncer.turns[-1]["text"]
            == "# user message\n\n  Continue  \n\n# m context\n\nUse the updated instructions"
        )
        assert "Keep the sidebar reachable." in str(bouncer.turns[-1]["context"])
        assert "Preserve the three windows." not in str(bouncer.turns[-1]["context"])
        contexts = [
            message
            for message in resumed.json()["chat"]["messages"]
            if message["data"].get("event") == "chat_context"
        ]
        assert len(contexts) == 2

        await client.patch(path, json={"project_id": None})
        assert (
            await client.post(
                path + "/submit", json={"text": "Continue without a project"}
            )
        ).status_code == 200
        assert "Project: none" in str(bouncer.turns[-1]["context"])
        assert "README:" not in str(bouncer.turns[-1]["context"])
        assert len(bouncer.started) == 1 and bouncer.resumed == ["thread-1", "thread-1"]

        # A system instruction cannot be submitted as user text.
        invalid = await client.post(
            path + "/submit", json={"message_id": created["messages"][0]["id"]}
        )
        assert invalid.status_code == 404
        mixed = await client.post(
            path + "/submit",
            json={"message_id": created["messages"][-1]["id"], "text": "extra"},
        )
        assert mixed.status_code == 422


async def test_reference_configuration(db: AsyncSession, tmp_path: Path) -> None:
    from uuid import UUID

    app = FastAPI()
    app.include_router(router)
    bouncer = FakeBouncer()

    async def test_db() -> AsyncIterator[AsyncSession]:
        yield db

    app.dependency_overrides[get_db] = test_db
    app.dependency_overrides[get_bouncer] = lambda: bouncer
    async with AsyncClient(
        transport=ASGITransport(app=app), base_url="http://test"
    ) as client:
        location = await client.post(
            "/api/locations", json={"name": "shared", "path": str(tmp_path)}
        )
        assert location.status_code == 201
        location_id = location.json()["id"]
        duplicate = await client.post(
            "/api/locations", json={"name": "shared", "path": "/elsewhere"}
        )
        assert duplicate.status_code == 409
        ids = []
        for index in range(3):
            response = await client.post(
                "/api/chats", json={"kind": "bot", "location_id": location_id}
            )
            assert response.status_code == 201
            ids.append(response.json()["id"])
            chat = await workspace.get_chat(db, UUID(ids[-1]))
            assert chat
            if index < 2:
                chat.codex_thread_id = f"thread-{index}"
                chat.active_turn_id = f"turn-{index}"
                chat.status = workspace.ChatStatus.RUNNING
        await db.commit()
        new_path = tmp_path / "moved"
        new_path.mkdir()
        changed = await client.patch(
            f"/api/locations/{location_id}", json={"path": str(new_path)}
        )
        assert changed.status_code == 200
        assert bouncer.interrupted == [("thread-0", "turn-0"), ("thread-1", "turn-1")]
        for index, chat_id in enumerate(ids):
            detail = (await client.get(f"/api/chats/{chat_id}")).json()
            assert detail["location"]["path"] == str(new_path)
            assert detail["status"] == ("interrupted" if index < 2 else "idle")
            assert (
                detail["messages"][-1]["content"]
                == f"Moved to the shared location ({new_path})"
            )
        repeated = await client.patch(
            f"/api/locations/{location_id}", json={"path": str(new_path)}
        )
        assert repeated.status_code == 200 and len(bouncer.interrupted) == 2
        detail = (await client.get(f"/api/chats/{ids[0]}")).json()
        assert sum(message["kind"] == "event" for message in detail["messages"]) == 1

        project = await client.post(
            "/api/projects", json={"name": "reference", "description": "Before"}
        )
        assert project.status_code == 201
        readme = Env.get().projects_dir / "reference/README.md"
        assert readme.exists()
        readme.write_text("User-maintained project context")
        updated = await client.patch(
            f"/api/projects/{project.json()['id']}",
            json={"description": "After", "archived": True},
        )
        assert updated.status_code == 200 and updated.json()["description"] == "After"
        assert (await client.get("/api/projects")).json() == []
        assert (await client.get("/api/projects?archived=true")).json()[0][
            "id"
        ] == project.json()["id"]
        assert readme.read_text() == "User-maintained project context"


def test_editable_role_files(tmp_path: Path) -> None:
    from importlib.resources import files

    from aibo.common.runtime_files import ensure_runtime_docs

    env = Env._get_test().model_copy(update={"CACHE_DIR": str(tmp_path)})
    bundled = files("aibo").joinpath("resources/docs")
    ensure_runtime_docs(env)
    api = env.docs_dir / "aibo/api.md"
    assert api.read_text() == bundled.joinpath("api.md").read_text()
    for kind, filename in (("manager", "m.md"), ("bot", "b.md"), ("shadow", "sb.md")):
        role = env.docs_dir / filename
        assert role.read_text() == bundled.joinpath(filename).read_text()
        # Empty files are deliberate customizations too.
        content = "" if kind == "shadow" else f"Local {kind} instructions\n"
        role.write_text(content)
        api.write_text("Outdated API reference\n")
        ensure_runtime_docs(env)
        assert role.read_text() == content
        assert api.read_text() == bundled.joinpath("api.md").read_text()
        assert role_instructions(kind, env) == content.strip()


async def test_manager_without_location(db: AsyncSession) -> None:
    app = FastAPI()
    app.include_router(router)
    bouncer = FakeBouncer()

    async def test_db() -> AsyncIterator[AsyncSession]:
        yield db

    app.dependency_overrides[get_db] = test_db
    app.dependency_overrides[get_bouncer] = lambda: bouncer
    async with AsyncClient(
        transport=ASGITransport(app=app), base_url="http://test"
    ) as client:
        created = await client.post("/api/chats", json={"kind": "manager"})
        assert created.status_code == 201 and created.json()["location"] is None
        path = f"/api/chats/{created.json()['id']}"
        response = await client.post(path + "/submit", json={"text": "Help me plan"})
        assert response.status_code == 200
        assert response.json()["chat"]["location"] is None
        assert bouncer.started[0]["cwd"] == str(Path.home())
        assert bouncer.turns[0]["cwd"] == str(Path.home())
        location = await client.post(
            "/api/locations", json={"name": "work", "path": str(Path.home())}
        )
        rejected = await client.patch(
            path + "/location", json={"location_id": location.json()["id"]}
        )
        assert rejected.status_code == 422
