"""Deploy immutable server releases; retain the old slot until cutover is verified."""

from __future__ import annotations

import fcntl
import json
import logging
import os
import shutil
import signal
import socket
import subprocess
import sys
import tempfile
import time
import urllib.error
import urllib.request
import uuid
from contextlib import contextmanager
from pathlib import Path
from typing import Any, Iterator

import fire

from aibo.common.constants import PACKAGE_DIR, Env
from aibo.common.log import configure_logging

SLOT_PORTS = {"blue": 5001, "green": 5002}
logger = logging.getLogger(__name__)


def parse_bool(value: str) -> bool:
    if value.lower() not in ("true", "false"):
        raise ValueError("Expected true or false")
    return value.lower() == "true"


class Deployment:
    def __init__(self) -> None:
        self.env: Env = Env.get()
        configure_logging("deploy", self.env)
        self.root = Path(PACKAGE_DIR).parents[2]
        self.runtime = Path(self.env.CACHE_DIR) / "deploy"
        self.runtime.mkdir(parents=True, exist_ok=True)
        self.public_url = os.environ.get("AIBO_PUBLIC_URL", "http://127.0.0.1:5000")
        self.children: dict[int, subprocess.Popen[bytes]] = {}

    @contextmanager
    def _locked(self, *, wait: bool = False) -> Iterator[None]:
        with (self.runtime / "deploy.lock").open("a") as lock:
            try:
                fcntl.flock(lock, fcntl.LOCK_EX | (0 if wait else fcntl.LOCK_NB))
            except BlockingIOError as error:
                raise RuntimeError("Another deployment is in progress") from error
            try:
                yield
            finally:
                fcntl.flock(lock, fcntl.LOCK_UN)

    def run(self, *, bouncer: bool = False) -> None:
        if settings := self._read_json(self.runtime / "local-settings.json"):
            from aibo.cli.local import LocalServices

            LocalServices(self, settings["server_url"]).run(force=True, bouncer=bouncer)
            return
        with self._locked():
            self._run(bouncer=bouncer)

    def redeploy_bouncer(self) -> None:
        """Replace only the Codex bouncer, from an immutable Python snapshot."""
        with self._locked():
            release = self._snapshot_python()
            self._restart_bouncer(release)
            logger.info("Redeployed bouncer from %s", release)
            print("Redeployed bouncer on port 5010.")

    def _run(self, *, bouncer: bool) -> None:
        old_slot = self._active_slot()
        old_state = self._read_json(self.runtime / "state.json") or {}
        old_process = self._read_json(self.runtime / f"{old_slot}.json") or {}
        new_slot = "green" if old_slot == "blue" else "blue"
        upstream = self.runtime / "active-upstream.conf"
        previous = upstream.read_text() if upstream.exists() else None
        if old_slot:
            # The file can be ahead of nginx after an interrupted reload.
            previous = f"server 127.0.0.1:{SLOT_PORTS[old_slot]};\n"
        release = self.build_assets()
        if bouncer:
            self._restart_bouncer(release)
        self._migrate(release)
        if self._active_slot() != old_slot:
            raise RuntimeError(
                "Nginx routing changed during the build; deployment stopped"
            )
        self._stop_process(new_slot)
        instance = self._start_process(
            new_slot, self._command(release, "server", SLOT_PORTS[new_slot])
        )
        switching = False
        try:
            self._wait_healthy(
                f"http://127.0.0.1:{SLOT_PORTS[new_slot]}/api/health", instance
            )
            switching = True
            self._switch_nginx(SLOT_PORTS[new_slot], instance)
            self._write_json(
                self.runtime / "state.json",
                {"active_slot": new_slot, "release": str(release)},
            )
        except Exception:
            if switching:
                try:
                    self._restore_nginx(previous, old_process.get("instance"))
                    self._write_json(self.runtime / "state.json", old_state)
                except Exception as error:
                    raise RuntimeError(
                        "Cutover and rollback failed. Both slots were retained; "
                        f"inspect {self._log_path()} and nginx before retrying."
                    ) from error
            self._stop_process(new_slot)
            logger.exception("Deployment failed; previous routing retained")
            raise

        # Uvicorn closes WebSockets and finishes in-flight HTTP requests on TERM.
        # Keep the new release active even if an old process refuses to drain.
        try:
            if old_slot:
                self._stop_process(old_slot)
        finally:
            self._reload_emacs(release)
        logger.info("Deployed %s from %s", new_slot, release)
        print(f"Deployed {new_slot} on port {SLOT_PORTS[new_slot]}.")

    def _snapshot_python(self) -> Path:
        releases = self.runtime / "releases"
        releases.mkdir(exist_ok=True)
        release = Path(tempfile.mkdtemp(prefix="release-", dir=releases))
        source = self.root / "python" / "src" / "aibo"

        def ignore(directory: str, names: list[str]) -> set[str]:
            excluded = set(
                shutil.ignore_patterns("__pycache__", "*.pyc", "tests")(
                    directory, names
                )
            )
            if Path(directory) == source / "resources":
                excluded.add("web")
            return excluded

        shutil.copytree(source, release / "python" / "aibo", ignore=ignore)
        return release

    def build_assets(self) -> Path:
        release = self._snapshot_python()
        web = self.root / "web"
        subprocess.run(["bun", "install", "--frozen-lockfile"], cwd=web, check=True)
        subprocess.run(
            [
                "bun",
                "run",
                "build",
                "--outDir",
                str(release / "python/aibo/resources/web"),
            ],
            cwd=web,
            check=True,
        )
        # An index response from the old slot may request its hashed assets after
        # cutover. Carry those files forward without changing either old release.
        assets = release / "python/aibo/resources/web/assets"
        for previous in release.parent.glob("*/python/aibo/resources/web/assets"):
            if previous == assets:
                continue
            for source_file in previous.rglob("*"):
                destination = assets / source_file.relative_to(previous)
                if source_file.is_file() and not destination.exists():
                    destination.parent.mkdir(parents=True, exist_ok=True)
                    shutil.copy2(source_file, destination)
        elisp = release / "elisp"
        elisp.mkdir()
        for source_file in (self.root / "elisp").glob("*.el"):
            shutil.copy2(source_file, elisp / source_file.name)
        files = [
            str(source)
            for source in sorted(elisp.glob("*.el"))
            if source.name != "aibo-pkg.el"
        ]
        subprocess.run(
            [
                "emacs",
                "--batch",
                "-Q",
                "--eval",
                "(setq load-prefer-newer t byte-compile-error-on-warn t)",
                "-L",
                str(elisp),
                "-f",
                "batch-byte-compile",
                *files,
            ],
            check=True,
        )
        return release

    @staticmethod
    def _migrate(release: Path) -> None:
        subprocess.run(
            [sys.executable, "-c", "from aibo.db import migrate_db; migrate_db()"],
            cwd=release / "python",
            check=True,
        )

    @staticmethod
    def _command(release: Path, service: str, port: int) -> list[str]:
        return [
            sys.executable,
            "-m",
            "uvicorn",
            f"aibo.{service}.main:create_app",
            "--factory",
            "--app-dir",
            str(release / "python"),
            "--host",
            "127.0.0.1",
            "--port",
            str(port),
            "--timeout-graceful-shutdown",
            "30",
        ]

    def _restart_bouncer(self, release: Path) -> None:
        self._stop_process("bouncer")
        instance = self._start_process(
            "bouncer", self._command(release, "bouncer", 5010)
        )
        try:
            self._wait_healthy("http://127.0.0.1:5010/health", instance)
        except Exception:
            self._stop_process("bouncer")
            raise

    def _start_process(self, name: str, command: list[str]) -> str:
        state_path = self.runtime / f"{name}.json"
        existing = self._read_json(state_path)
        if existing and self._running(existing):
            raise RuntimeError(
                f"{name} still has a live process; refusing to replace its record"
            )
        host = command[command.index("--host") + 1]
        port = int(command[command.index("--port") + 1])
        with socket.socket() as probe:
            probe.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
            try:
                probe.bind((host, port))
            except OSError as error:
                raise RuntimeError(
                    f"{host}:{port} is occupied; refusing to start {name}"
                ) from error
        instance = uuid.uuid4().hex
        command = [*command, "--header", f"x-aibo-instance:{instance}"]
        log_path = self._log_path()
        log_path.parent.mkdir(parents=True, exist_ok=True)
        with log_path.open("ab") as log:
            process = subprocess.Popen(
                command,
                cwd=self.root / "python",
                stdin=subprocess.DEVNULL,
                env={**os.environ, "AIBO_LOG_CONSOLE": "0"},
                stdout=log,
                stderr=subprocess.STDOUT,
                start_new_session=True,
            )
        self.children[process.pid] = process
        try:
            identity = None
            for _ in range(20):
                if process.poll() is not None:
                    raise RuntimeError(
                        f"{name} exited during startup; inspect {log_path}"
                    )
                identity = self._process_identity(process.pid)
                if identity and f"x-aibo-instance:{instance}" in identity:
                    break
                time.sleep(0.05)
            else:
                raise RuntimeError(f"Could not identify the new {name} process")
            self._write_json(
                state_path,
                {
                    "pid": process.pid,
                    "command": command,
                    "identity": identity,
                    "instance": instance,
                },
            )
        except Exception:
            # This Popen handle belongs to this invocation, even if saving failed.
            if process.poll() is None:
                process.terminate()
            process.wait(timeout=35)
            raise
        logger.info("Started %s as pid %d (%s)", name, process.pid, instance)
        return instance

    def _stop_process(self, name: str, timeout: float = 35.0) -> None:
        state_path = self.runtime / f"{name}.json"
        state = self._read_json(state_path)
        if not state:
            return
        pid = int(state["pid"])
        if self._running(state):
            if not self._owns_process(state):
                raise RuntimeError(
                    f"Refusing to signal unowned {name} pid {pid}; record retained"
                )
            try:
                os.kill(pid, signal.SIGTERM)
            except ProcessLookupError:
                pass
            deadline = time.monotonic() + timeout
            while self._running(state):
                if not self._owns_process(state):
                    if not self._running(state):
                        break
                    raise RuntimeError(
                        f"Identity changed while draining {name} pid {pid}"
                    )
                if time.monotonic() >= deadline:
                    raise RuntimeError(
                        f"{name} pid {pid} did not drain; process and record retained"
                    )
                time.sleep(0.1)
        self._write_json(state_path, {**state, "stopped": True})
        logger.info("Stopped %s pid %d", name, pid)

    def _running(self, state: dict[str, Any]) -> bool:
        if state.get("stopped"):
            return False
        pid = int(state["pid"])
        if pid <= 1:
            raise RuntimeError(f"Invalid deployment pid: {pid}")
        child = self.children.get(pid)
        if child is not None and child.poll() is not None:
            return False
        try:
            os.kill(pid, 0)
            return True
        except ProcessLookupError:
            return False

    def _owns_process(self, state: dict[str, Any]) -> bool:
        identity = state.get("identity")
        instance = state.get("instance")
        return bool(
            identity
            and instance
            and f"x-aibo-instance:{instance}" in identity
            and self._process_identity(int(state["pid"])) == identity
        )

    @staticmethod
    def _process_identity(pid: int) -> str | None:
        result = subprocess.run(
            ["ps", "-ww", "-p", str(pid), "-o", "lstart=", "-o", "command="],
            env={**os.environ, "LC_ALL": "C"},
            capture_output=True,
            text=True,
            check=False,
        )
        return result.stdout.strip() if result.returncode == 0 else None

    def _log_path(self) -> Path:
        return Path(self.env.logs_dir) / f"{time.strftime('%Y-%m-%d')}.log"

    @staticmethod
    def _response(url: str) -> tuple[int, str | None] | None:
        try:
            request = urllib.request.Request(url, headers={"Connection": "close"})
            with urllib.request.urlopen(request, timeout=1) as response:
                return response.status, response.headers.get("x-aibo-instance")
        except urllib.error.HTTPError as error:
            try:
                return error.code, error.headers.get("x-aibo-instance")
            finally:
                error.close()
        except (urllib.error.URLError, TimeoutError):
            return None

    def _wait_healthy(self, url: str, instance: str, timeout: float = 30.0) -> None:
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            if self._response(url) == (200, instance):
                return
            time.sleep(0.25)
        raise RuntimeError(f"Timed out waiting for instance {instance} at {url}")

    def _reload_nginx(self) -> None:
        subprocess.run([self.env.NGINX_COMMAND, "-t"], check=True)
        subprocess.run([self.env.NGINX_COMMAND, "-s", "reload"], check=True)

    def _switch_nginx(self, port: int, instance: str) -> None:
        self._write_text(
            self.runtime / "active-upstream.conf", f"server 127.0.0.1:{port};\n"
        )
        self._reload_nginx()
        self._wait_healthy(f"{self.public_url}/api/health", instance)

    def _restore_nginx(self, previous: str | None, instance: str | None) -> None:
        # A first deployment has no previous worker. Retain a valid unavailable
        # upstream instead of leaving nginx pointing at a failed candidate.
        self._write_text(
            self.runtime / "active-upstream.conf",
            previous or "server 127.0.0.1:9 down;\n",
        )
        self._reload_nginx()
        deadline = time.monotonic() + 30
        while time.monotonic() < deadline:
            response = self._response(f"{self.public_url}/api/health")
            if (
                response
                and response[1] == instance
                and (instance or response[0] >= 400)
            ):
                return
            time.sleep(0.25)
        raise RuntimeError("Could not verify restored nginx routing")

    def _active_slot(self) -> str | None:
        response = self._response(f"{self.public_url}/api/health")
        records = {
            slot: self._read_json(self.runtime / f"{slot}.json") for slot in SLOT_PORTS
        }
        if response and response[1]:
            for slot, state in records.items():
                if (
                    state
                    and state.get("instance") == response[1]
                    and self._running(state)
                    and self._owns_process(state)
                ):
                    return slot
            raise RuntimeError(
                "Nginx routes to an unowned process; refusing deployment"
            )
        if (response and response[0] == 200) or any(
            state and self._running(state) for state in records.values()
        ):
            raise RuntimeError(
                "Cannot identify nginx's active slot; refusing deployment"
            )
        return None

    def _reload_emacs(self, release: Path) -> None:
        elisp = json.dumps(str(release / "elisp"), ensure_ascii=False)
        expression = f'(progn (add-to-list \'load-path {elisp}) (load "aibo-ui" nil t) (aibo:reload))'
        # e normally starts a daemon. Probe the same configured client first,
        # then disable that startup helper in this shell to close the exit race.
        script = """
if [ -n "${EMACS_SOCKET_PATH}" ]; then
    _EMACS_FLAGS="-s ${EMACS_SOCKET_PATH}"
fi
if ! emacsclient ${_EMACS_FLAGS} --alternate-editor=false --eval t >/dev/null 2>&1; then
    exit 3
fi
e-daemon() { :; }
e --alternate-editor=false --eval "$1"
"""
        result = subprocess.run(
            ["bash", "-ic", script, "aibo-reload", expression],
            capture_output=True,
            text=True,
            check=False,
        )
        if result.returncode == 3:
            logger.info("No active Emacs daemon; reload skipped")
        elif result.returncode:
            logger.warning(
                "Server deployed, but Emacs reload failed: %s", result.stderr.strip()
            )

    @staticmethod
    def _read_json(path: Path) -> dict[str, Any] | None:
        return dict(json.loads(path.read_text())) if path.exists() else None

    @staticmethod
    def _write_text(path: Path, text: str) -> None:
        temporary = path.with_suffix(path.suffix + ".tmp")
        temporary.write_text(text)
        temporary.replace(path)

    @classmethod
    def _write_json(cls, path: Path, data: dict[str, Any]) -> None:
        cls._write_text(path, json.dumps(data, indent=2) + "\n")


@fire.decorators.SetParseFn(parse_bool, "bouncer", "redeploy_bouncer")
def main(bouncer: bool = False, redeploy_bouncer: bool = False) -> None:
    Deployment().run(bouncer=bouncer or redeploy_bouncer)


if __name__ == "__main__":
    fire.Fire(main)
