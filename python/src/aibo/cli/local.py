"""Start a local workspace on demand, retaining its database and process records."""

import getpass
import os
import shutil
import socket
import subprocess
from contextlib import contextmanager
from pathlib import Path
from typing import Iterator
from urllib.parse import urlparse

import psycopg

from aibo.cli.redeploy import Deployment


def postgres_bin(name: str) -> str:
    executable = shutil.which(name)
    if executable:
        return executable
    for root in (
        "/opt/homebrew/opt/postgresql@18/bin",
        "/usr/local/opt/postgresql@18/bin",
    ):
        path = Path(root) / name
        if path.is_file():
            return str(path)
    raise RuntimeError(f"Install PostgreSQL and put {name} on PATH")


class LocalServices:
    def __init__(self, deployment: Deployment, url: str) -> None:
        self.deployment = deployment
        parsed = urlparse(url)
        if (
            parsed.scheme != "http"
            or parsed.hostname not in ("localhost", "127.0.0.1")
            or parsed.path not in ("", "/")
        ):
            raise ValueError("Automatic startup requires a local http://localhost URL")
        self.url = url.rstrip("/")
        self.port = parsed.port or 80

    def healthy(self) -> bool:
        response = self.deployment._response(self.url + "/api/health")
        return bool(response and response[0] == 200)

    def _database(self) -> str:
        deployment = self.deployment
        settings = (
            deployment._read_json(deployment.runtime / "local-settings.json") or {}
        )
        explicit = os.environ.get("AIBO_DATABASE_URL")
        supplied = explicit or settings.get("database_url")
        if supplied and (explicit or not settings.get("managed_postgres")):
            return str(supplied)
        root = deployment.runtime.parent / "postgres"
        root.mkdir(exist_ok=True)
        if not (root / "PG_VERSION").exists():
            subprocess.run(
                [
                    postgres_bin("initdb"),
                    "-D",
                    str(root),
                    "--auth-local=trust",
                    "--auth-host=trust",
                    "--encoding=UTF8",
                    "--locale=C",
                ],
                check=True,
            )
        port = settings.get("postgres_port")
        if not port:
            with socket.socket() as probe:
                probe.bind(("127.0.0.1", 0))
                port = probe.getsockname()[1]
        deployment._write_json(
            deployment.runtime / "local-settings.json",
            {
                **settings,
                "postgres_port": port,
                "managed_postgres": True,
                "server_url": self.url,
            },
        )
        pg_ctl = postgres_bin("pg_ctl")
        running = subprocess.run(
            [pg_ctl, "-D", str(root), "status"], capture_output=True, check=False
        )
        if running.returncode:
            subprocess.run(
                [
                    pg_ctl,
                    "-D",
                    str(root),
                    "-l",
                    str(root / "server.log"),
                    "-o",
                    f"-h 127.0.0.1 -p {port}",
                    "-w",
                    "start",
                ],
                check=True,
            )
        user = getpass.getuser()
        with psycopg.connect(
            host="127.0.0.1", port=port, user=user, dbname="postgres", autocommit=True
        ) as connection:
            if not connection.execute(
                "SELECT 1 FROM pg_database WHERE datname = 'aibo'"
            ).fetchone():
                connection.execute("CREATE DATABASE aibo")
        database_url = f"postgresql+asyncpg://{user}@127.0.0.1:{port}/aibo"
        deployment._write_json(
            deployment.runtime / "local-settings.json",
            {
                "database_url": database_url,
                "postgres_port": port,
                "managed_postgres": True,
                "server_url": self.url,
            },
        )
        return database_url

    @contextmanager
    def environment(self) -> Iterator[None]:
        database_url = self._database()
        path = self.deployment.runtime / "local-settings.json"
        settings = self.deployment._read_json(path) or {}
        if os.environ.get("AIBO_DATABASE_URL"):
            settings["managed_postgres"] = False
        self.deployment._write_json(
            path, {**settings, "database_url": database_url, "server_url": self.url}
        )
        path.chmod(0o600)
        values = {
            "AIBO_DATABASE_URL": database_url,
            "AIBO_CACHE_DIR": self.deployment.env.CACHE_DIR,
            "AIBO_PUBLIC_URL": self.url,
        }
        previous = {key: os.environ.get(key) for key in values}
        os.environ.update(values)
        try:
            yield
        finally:
            for key, value in previous.items():
                if value is None:
                    os.environ.pop(key, None)
                else:
                    os.environ[key] = value

    def run(self, *, force: bool = False, bouncer: bool = False) -> None:
        deployment = self.deployment
        if not force and self.healthy():
            print(f"Aibo is ready at {self.url}")
            return
        with deployment._locked(wait=not force):
            if not force and self.healthy():
                print(f"Aibo is ready at {self.url}")
                return
            with self.environment():
                release = deployment.build_assets()
                response = deployment._response("http://127.0.0.1:5010/health")
                if bouncer or (not force and (not response or response[0] != 200)):
                    deployment._restart_bouncer(release)
                if force or not self.healthy():
                    deployment._migrate(release)
                    deployment._stop_process("local")
                    instance = deployment._start_process(
                        "local", deployment._command(release, "server", self.port)
                    )
                    deployment._wait_healthy(self.url + "/api/health", instance)
                if force:
                    deployment._reload_emacs(release)
                print(f"Aibo is ready at {self.url}")
