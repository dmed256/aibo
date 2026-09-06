# aibo-server

Python services for Aibo. See the repository [README](../README.md) for installation, configuration, testing, and deployment.

- `core/`: workspace rules, project context, model settings, and title generation.
- `server/`: FastAPI setup, live-event persistence/recovery, and resource-specific routes.
- `bouncer/`: Codex app-server transport and session management.
- `db/` and `__db_migrations/`: PostgreSQL models and migrations.
- `cli/`: local startup, isolated release builds, and deployment commands.
- `resources/docs/`: bundled editable role and API instructions.
- `resources/web/`: generated frontend assets; build these from `web/` before packaging or running HTTP asset tests.

Tests live beside the subsystems they cover. Core tests are grouped by behavior and share an in-memory SQLite fixture; `core/tests/test_postgres.py` separately verifies cross-worker behavior in disposable PostgreSQL schemas when `AIBO_POSTGRES_TEST_URL` is set.

Completed messages are persisted and forwarded to clients. Token deltas and incomplete snapshots remain hidden; reconnect reconciles finished turns. Test doubles exercise Codex interactions without submitting real model turns.
