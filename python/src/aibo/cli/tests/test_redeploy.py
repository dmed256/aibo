"""Deployment failure contracts, using only owned temporary services."""

import asyncio
import json
import signal
import socket
import subprocess
import sys
from pathlib import Path
from typing import Any
from unittest.mock import Mock

import httpx
import pytest
import websockets

from aibo.cli.redeploy import Deployment
from aibo.common.constants import Env


def saved(deploy: Deployment, name: str) -> dict[str, Any]:
    record = deploy._read_json(deploy.runtime / f"{name}.json")
    assert record is not None
    return record


@pytest.fixture
def deploy(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> Deployment:
    env = Env._get_test().model_copy(update={"CACHE_DIR": str(tmp_path / "cache")})
    monkeypatch.setattr(Env, "get", lambda: env)
    deployment = Deployment()
    deployment.root = tmp_path / "source"
    (deployment.root / "python").mkdir(parents=True)
    return deployment


def test_lock(deploy: Deployment) -> None:
    with deploy._locked():
        with pytest.raises(RuntimeError, match="in progress"):
            with Deployment()._locked():
                pytest.fail("Concurrent deployment acquired the same lock")
    with deploy._locked():
        pass


@pytest.mark.parametrize(
    "arguments,bouncer",
    [
        (["redeploy"], False),
        (["redeploy", "--redeploy-bouncer"], True),
        (["redeploy", "--redeploy-bouncer=true"], True),
        (["redeploy", "--redeploy-bouncer=false"], False),
        (["bouncer-redeploy"], None),
    ],
)
def test_commands(
    monkeypatch: pytest.MonkeyPatch, arguments: list[str], bouncer: bool | None
) -> None:
    from aibo.cli import deploy as cli

    deployment = Mock(spec=Deployment)
    monkeypatch.setattr(cli, "Deployment", lambda: deployment)
    monkeypatch.setattr(sys, "argv", ["aibo.cli.deploy", *arguments])
    cli.main()
    if bouncer is None:
        deployment.redeploy_bouncer.assert_called_once_with()
        deployment.run.assert_not_called()
    else:
        deployment.run.assert_called_once_with(bouncer=bouncer)
        deployment.redeploy_bouncer.assert_not_called()


def test_invalid_flag(monkeypatch: pytest.MonkeyPatch) -> None:
    from aibo.cli import deploy as cli

    constructor = Mock()
    monkeypatch.setattr(cli, "Deployment", constructor)
    monkeypatch.setattr(
        sys, "argv", ["aibo.cli.deploy", "redeploy", "--redeploy-bouncer=maybe"]
    )
    with pytest.raises(ValueError, match="true or false"):
        cli.main()
    constructor.assert_not_called()


def test_bouncer_only(deploy: Deployment, monkeypatch: pytest.MonkeyPatch) -> None:
    package = deploy.root / "python/src/aibo"
    package.mkdir(parents=True)
    (package / "bouncer.py").write_text("original bouncer")
    started = Mock(return_value="bouncer-instance")
    stopped = Mock()
    healthy = Mock()
    monkeypatch.setattr(deploy, "_start_process", started)
    monkeypatch.setattr(deploy, "_stop_process", stopped)
    monkeypatch.setattr(deploy, "_wait_healthy", healthy)
    for method in ("build_assets", "_migrate", "_reload_emacs", "_reload_nginx"):
        monkeypatch.setattr(deploy, method, Mock(side_effect=AssertionError(method)))

    with deploy._locked(), pytest.raises(RuntimeError, match="in progress"):
        deploy.redeploy_bouncer()
    started.assert_not_called()
    deploy.redeploy_bouncer()
    stopped.assert_called_once_with("bouncer")
    started.assert_called_once()
    name, command = started.call_args.args
    assert name == "bouncer"
    assert "aibo.bouncer.main:create_app" in command
    release = Path(command[command.index("--app-dir") + 1])
    (package / "bouncer.py").write_text("later source edit")
    assert (release / "aibo/bouncer.py").read_text() == "original bouncer"
    assert not (deploy.runtime / "state.json").exists()
    healthy.assert_called_once_with("http://127.0.0.1:5010/health", "bouncer-instance")


@pytest.mark.parametrize("mismatch", ["command", "birth", "legacy"])
def test_ownership(
    deploy: Deployment, monkeypatch: pytest.MonkeyPatch, mismatch: str
) -> None:
    identity = "Sat Sep 5 04:00:00 2026 python -m uvicorn app:app --header x-aibo-instance:ours"
    state = {"pid": 90001, "instance": "ours", "identity": identity}
    deploy._write_json(deploy.runtime / "blue.json", state)
    monkeypatch.setattr(deploy, "_running", lambda _: True)
    observed = identity
    if mismatch == "command":
        observed = identity.replace("app:app", "unrelated:app")
    elif mismatch == "birth":
        observed = identity.replace("04:00:00", "04:01:00")
    else:
        state.pop("identity")
        deploy._write_json(deploy.runtime / "blue.json", state)
    monkeypatch.setattr(deploy, "_process_identity", lambda _: observed)
    kill = Mock()
    monkeypatch.setattr("aibo.cli.redeploy.os.kill", kill)
    with pytest.raises(RuntimeError, match="unowned"):
        deploy._stop_process("blue")
    kill.assert_not_called()
    assert saved(deploy, "blue") == state


def test_drain_timeout(deploy: Deployment, monkeypatch: pytest.MonkeyPatch) -> None:
    state = {"pid": 90001, "instance": "ours"}
    deploy._write_json(deploy.runtime / "blue.json", state)
    monkeypatch.setattr(deploy, "_running", lambda _: True)
    monkeypatch.setattr(deploy, "_owns_process", lambda _: True)
    kill = Mock()
    monkeypatch.setattr("aibo.cli.redeploy.os.kill", kill)
    with pytest.raises(RuntimeError, match="did not drain"):
        deploy._stop_process("blue", timeout=0)
    kill.assert_called_once_with(90001, signal.SIGTERM)
    assert saved(deploy, "blue") == state


def test_health_identity(deploy: Deployment, monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setattr(deploy, "_response", lambda _: (200, "old-listener"))
    with pytest.raises(RuntimeError, match="Timed out"):
        deploy._wait_healthy("http://unused/health", "candidate", timeout=0.01)
    monkeypatch.setattr(deploy, "_response", lambda _: (200, "candidate"))
    deploy._wait_healthy("http://unused/health", "candidate", timeout=0.01)


def test_active_route(deploy: Deployment, monkeypatch: pytest.MonkeyPatch) -> None:
    # A crash after nginx reload but before state.json must not stop the active slot.
    deploy._write_json(deploy.runtime / "state.json", {"active_slot": "blue"})
    for slot in ("blue", "green"):
        deploy._write_json(deploy.runtime / f"{slot}.json", {"instance": slot})
    monkeypatch.setattr(deploy, "_running", lambda _: True)
    monkeypatch.setattr(deploy, "_owns_process", lambda _: True)
    monkeypatch.setattr(deploy, "_response", lambda _: (503, "green"))
    assert deploy._active_slot() == "green"
    monkeypatch.setattr(deploy, "_response", lambda _: (200, "unknown"))
    with pytest.raises(RuntimeError, match="unowned"):
        deploy._active_slot()
    monkeypatch.setattr(deploy, "_response", lambda _: None)
    with pytest.raises(RuntimeError, match="Cannot identify"):
        deploy._active_slot()


@pytest.fixture
def cutover(deploy: Deployment, monkeypatch: pytest.MonkeyPatch) -> list[str]:
    events: list[str] = []
    release = deploy.runtime / "candidate"
    deploy._write_json(
        deploy.runtime / "state.json", {"active_slot": "blue", "release": "old"}
    )
    deploy._write_json(deploy.runtime / "blue.json", {"instance": "old"})
    (deploy.runtime / "active-upstream.conf").write_text("server 127.0.0.1:5001;\n")
    monkeypatch.setattr(deploy, "_active_slot", lambda: "blue")
    monkeypatch.setattr(deploy, "build_assets", lambda: release)
    monkeypatch.setattr(deploy, "_migrate", lambda _: events.append("migrate"))
    monkeypatch.setattr(deploy, "_restart_bouncer", lambda _: events.append("bouncer"))
    monkeypatch.setattr(deploy, "_start_process", lambda *_: "new")
    monkeypatch.setattr(
        deploy, "_stop_process", lambda name: events.append("stop " + name)
    )
    monkeypatch.setattr(
        deploy, "_wait_healthy", lambda url, _: events.append("health " + url)
    )
    monkeypatch.setattr(deploy, "_reload_emacs", lambda _: events.append("emacs"))
    monkeypatch.setattr(deploy, "_reload_nginx", lambda: events.append("nginx"))
    monkeypatch.setattr(deploy, "_response", lambda _: (200, "old"))
    return events


@pytest.mark.parametrize("failure", ["health", "reload", "route", "state", "rollback"])
def test_rollback(
    deploy: Deployment,
    cutover: list[str],
    monkeypatch: pytest.MonkeyPatch,
    failure: str,
) -> None:
    if failure == "route":
        # An interrupted earlier reload can leave the file ahead of live routing.
        (deploy.runtime / "active-upstream.conf").write_text("server 127.0.0.1:5002;\n")

    def health(url: str, _: str) -> None:
        cutover.append("health " + url)
        if failure == "health" or (
            failure in ("route", "rollback") and ":5000/" in url
        ):
            raise RuntimeError("health failure")

    def reload() -> None:
        cutover.append("nginx")
        if failure == "reload" and cutover.count("nginx") == 1:
            raise RuntimeError("reload failure")
        if failure == "rollback" and cutover.count("nginx") == 2:
            raise RuntimeError("rollback failure")

    write = deploy._write_json

    def state(path: Path, data: dict[str, Any]) -> None:
        if (
            failure == "state"
            and path.name == "state.json"
            and data.get("active_slot") == "green"
        ):
            raise OSError("disk full")
        write(path, data)

    monkeypatch.setattr(deploy, "_wait_healthy", health)
    monkeypatch.setattr(deploy, "_reload_nginx", reload)
    monkeypatch.setattr(deploy, "_write_json", state)
    with pytest.raises((RuntimeError, OSError)):
        deploy.run()
    assert (
        "stop blue" not in cutover
        and "bouncer" not in cutover
        and "emacs" not in cutover
    )
    assert cutover.count("stop green") == (1 if failure == "rollback" else 2)
    assert (
        deploy.runtime / "active-upstream.conf"
    ).read_text() == "server 127.0.0.1:5001;\n"
    assert saved(deploy, "state")["release"] == "old"
    if failure not in ("health", "rollback"):
        assert cutover.index("nginx") < len(cutover) - 1
        assert cutover[-1] == "stop green"


@pytest.mark.parametrize("bouncer", [False, True])
def test_cutover_order(deploy: Deployment, cutover: list[str], bouncer: bool) -> None:
    deploy.run(bouncer=bouncer)
    assert ("bouncer" in cutover) == bouncer
    assert cutover[-2:] == ["stop blue", "emacs"]
    assert "health http://127.0.0.1:5000/api/health" in cutover
    assert saved(deploy, "state")["active_slot"] == "green"


def test_route_changes_during_build(
    deploy: Deployment, cutover: list[str], monkeypatch: pytest.MonkeyPatch
) -> None:
    monkeypatch.setattr(deploy, "_active_slot", Mock(side_effect=["blue", "green"]))
    with pytest.raises(RuntimeError, match="routing changed"):
        deploy.run()
    assert cutover == ["migrate"]
    assert saved(deploy, "state")["release"] == "old"


def test_old_slot_will_not_drain(
    deploy: Deployment, cutover: list[str], monkeypatch: pytest.MonkeyPatch
) -> None:
    def stop(name: str) -> None:
        cutover.append("stop " + name)
        if name == "blue":
            raise RuntimeError("did not drain")

    monkeypatch.setattr(deploy, "_stop_process", stop)
    with pytest.raises(RuntimeError, match="did not drain"):
        deploy.run()
    assert cutover[-1] == "emacs"
    assert cutover.count("stop green") == 1
    assert saved(deploy, "state")["active_slot"] == "green"
    assert (
        deploy.runtime / "active-upstream.conf"
    ).read_text() == "server 127.0.0.1:5002;\n"


def test_release_isolation(deploy: Deployment, monkeypatch: pytest.MonkeyPatch) -> None:
    package = deploy.root / "python/src/aibo"
    (package / "resources/web").mkdir(parents=True)
    (package / "resources/web/index.html").write_text("active web")
    (package / "main.py").write_text("original source")
    elisp = deploy.root / "elisp"
    elisp.mkdir()
    for name in (
        "aibo-custom",
        "aibo-api",
        "aibo-search",
        "aibo-ui",
        "aibo-ui-input",
        "aibo-pkg",
        "aibo",
    ):
        (elisp / f"{name}.el").write_text("original elisp")
    commands: list[list[str]] = []
    fail_compile = False

    def build(command: list[str], **_: Any) -> None:
        commands.append(command)
        if "--outDir" in command:
            output = Path(command[command.index("--outDir") + 1])
            output.mkdir(parents=True)
            (output / "index.html").write_text("candidate web")
        if command[0] == "emacs" and fail_compile:
            raise subprocess.CalledProcessError(1, command)

    monkeypatch.setattr("aibo.cli.redeploy.subprocess.run", build)
    first = deploy.build_assets()
    old_assets = first / "python/aibo/resources/web/assets"
    old_assets.mkdir()
    (old_assets / "index-previous.js").write_text("previous browser code")
    (package / "main.py").write_text("updated source")
    fail_compile = True
    with pytest.raises(subprocess.CalledProcessError):
        deploy.build_assets()
    assert (first / "python/aibo/main.py").read_text() == "original source"
    assert (
        first / "python/aibo/resources/web/index.html"
    ).read_text() == "candidate web"
    assert (package / "resources/web/index.html").read_text() == "active web"
    assert any(arg.endswith("/aibo-search.el") for arg in commands[2])
    assert any(arg.endswith("/aibo-ui-input.el") for arg in commands[2])
    assert not any(arg.endswith("/aibo-pkg.el") for arg in commands[2])
    assert not (deploy.runtime / "state.json").exists()
    fail_compile = False
    next_release = deploy.build_assets()
    assert (next_release / "python/aibo/main.py").read_text() == "updated source"
    assert (
        next_release / "python/aibo/resources/web/assets/index-previous.js"
    ).read_text() == "previous browser code"


@pytest.mark.parametrize("active", [False, True])
def test_emacs_probe(
    deploy: Deployment, monkeypatch: pytest.MonkeyPatch, tmp_path: Path, active: bool
) -> None:
    # Run the actual reload script with a fake rc file, never the user's daemon.
    rc = tmp_path / "test.bashrc"
    calls = tmp_path / "calls"
    rc.write_text(
        f"""
EMACS_SOCKET_PATH=/tmp/aibo-test-emacs.sock
_EMACS_FLAGS=-nw
emacsclient() {{ printf '%s\\n' "$*" >> '{calls}'; return {0 if active else 1}; }}
e-daemon() {{ printf 'UNEXPECTED START\\n' >> '{calls}'; }}
e() {{ e-daemon; emacsclient ${{_EMACS_FLAGS}} "$@"; }}
"""
    )
    run = subprocess.run

    def isolated(command: list[str], **kwargs: Any) -> subprocess.CompletedProcess[str]:
        return run(["bash", "--rcfile", str(rc), *command[1:]], **kwargs)

    monkeypatch.setattr("aibo.cli.redeploy.subprocess.run", isolated)
    release = tmp_path / 'release with "quotes" and $(touch unexpected)'
    deploy._reload_emacs(release)
    lines = calls.read_text().splitlines()
    assert len(lines) == (2 if active else 1)
    assert "UNEXPECTED START" not in calls.read_text()
    assert all("--alternate-editor=false" in line for line in lines)
    assert all("-s /tmp/aibo-test-emacs.sock" in line for line in lines)
    if active:
        assert "aibo:reload" in lines[1] and "$(touch unexpected)" in lines[1]
        assert json.dumps(str(release / "elisp")) in lines[1]


async def test_http_and_websocket_drain(deploy: Deployment, tmp_path: Path) -> None:
    # No Aibo app, database or Codex process: just an owned uvicorn fixture.
    with socket.socket() as reserved:
        reserved.bind(("127.0.0.1", 0))
        port = reserved.getsockname()[1]
    app_dir = tmp_path / "app"
    app_dir.mkdir()
    started, release = app_dir / "started", app_dir / "release"
    (app_dir / "fixture.py").write_text(
        """
import asyncio
from pathlib import Path
from typing import Any
from fastapi import FastAPI, WebSocket, WebSocketDisconnect
app = FastAPI()
root = Path(__file__).parent
@app.get('/health')
async def health():
    return {'status': 'ok'}
@app.get('/hold')
async def hold():
    (root / 'started').touch()
    while not (root / 'release').exists():
        await asyncio.sleep(.01)
    return {'finished': True}
@app.websocket('/events')
async def events(ws: WebSocket):
    await ws.accept()
    await ws.send_text('connected')
    try:
        await ws.receive_text()
    except WebSocketDisconnect:
        pass
"""
    )
    command = [
        sys.executable,
        "-m",
        "uvicorn",
        "fixture:app",
        "--app-dir",
        str(app_dir),
        "--host",
        "127.0.0.1",
        "--port",
        str(port),
        "--timeout-graceful-shutdown",
        "5",
    ]
    instance = deploy._start_process("fixture", command)
    stopping = None
    try:
        await asyncio.to_thread(
            deploy._wait_healthy, f"http://127.0.0.1:{port}/health", instance, 5
        )
        with pytest.raises(RuntimeError, match="occupied"):
            deploy._start_process("duplicate", command)
        assert not (deploy.runtime / "duplicate.json").exists()
        async with httpx.AsyncClient(timeout=10, trust_env=False) as client:
            async with websockets.connect(f"ws://127.0.0.1:{port}/events") as ws:
                assert await ws.recv() == "connected"
                request = asyncio.create_task(
                    client.get(f"http://127.0.0.1:{port}/hold")
                )
                for _ in range(200):
                    if started.exists():
                        break
                    await asyncio.sleep(0.01)
                assert started.exists()
                stopping = asyncio.create_task(
                    asyncio.to_thread(deploy._stop_process, "fixture", 8)
                )
                await asyncio.wait_for(ws.wait_closed(), 3)
                assert ws.close_code == 1012
                assert not request.done() and not stopping.done()
                release.touch()
                response = await request
                assert response.status_code == 200 and response.json() == {
                    "finished": True
                }
                await stopping
                assert saved(deploy, "fixture")["stopped"] is True
    finally:
        release.touch()
        if stopping:
            await stopping
        else:
            await asyncio.to_thread(deploy._stop_process, "fixture", 8)


def test_local_startup(deploy: Deployment, monkeypatch: pytest.MonkeyPatch) -> None:
    from aibo.cli.local import LocalServices

    local = LocalServices(deploy, "http://127.0.0.1:5003")
    release = deploy.runtime / "release"
    ready = False
    monkeypatch.setattr(local, "healthy", lambda: ready)
    monkeypatch.setattr(local, "_database", lambda: "postgresql+asyncpg://local/aibo")
    build = Mock(return_value=release)
    monkeypatch.setattr(deploy, "build_assets", build)
    monkeypatch.setattr(deploy, "_response", lambda _: (200, "bouncer"))
    restart_bouncer = Mock()
    monkeypatch.setattr(deploy, "_restart_bouncer", restart_bouncer)
    for name in ("_migrate", "_stop_process", "_reload_emacs"):
        monkeypatch.setattr(deploy, name, Mock())
    started = Mock(return_value="local-instance")
    monkeypatch.setattr(deploy, "_start_process", started)
    monkeypatch.setattr(deploy, "_wait_healthy", Mock())
    local.run()
    assert started.call_args.args[0] == "local"
    assert "5003" in started.call_args.args[1]
    restart_bouncer.assert_not_called()
    assert saved(deploy, "local-settings")["server_url"] == local.url
    ready = True
    with deploy._locked():
        local.run()
    build.assert_called_once()
    monkeypatch.setattr(deploy, "_response", lambda _: (503, "bouncer"))
    local.run(force=True)
    restart_bouncer.assert_not_called()
    with pytest.raises(ValueError, match="local"):
        LocalServices(deploy, "https://example.com")


def test_local_database(deploy: Deployment, monkeypatch: pytest.MonkeyPatch) -> None:
    from aibo.cli.local import LocalServices, postgres_bin

    try:
        postgres_bin("initdb")
    except RuntimeError:
        pytest.skip("Local PostgreSQL tools are not installed")
    monkeypatch.delenv("AIBO_DATABASE_URL", raising=False)
    local = LocalServices(deploy, "http://127.0.0.1:5003")
    root = deploy.runtime.parent / "postgres"
    try:
        first = local._database()
        subprocess.run(
            [postgres_bin("pg_ctl"), "-D", str(root), "-m", "fast", "-w", "stop"],
            check=True,
            capture_output=True,
        )
        assert local._database() == first
        assert saved(deploy, "local-settings")["managed_postgres"]
    finally:
        if (root / "postmaster.pid").exists():
            subprocess.run(
                [postgres_bin("pg_ctl"), "-D", str(root), "-m", "fast", "-w", "stop"],
                check=True,
                capture_output=True,
            )
