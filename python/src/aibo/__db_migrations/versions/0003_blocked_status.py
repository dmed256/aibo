"""Persist blocked chats separately from failed turns."""

import sqlalchemy as sa
from alembic import op

revision = "0003_blocked_status"
down_revision = "0002_manager_numbers"
branch_labels = None
depends_on = None


def replace_status_constraint(statuses: str) -> None:
    # The original migration left this constraint unnamed; PostgreSQL names it.
    for constraint in sa.inspect(op.get_bind()).get_check_constraints("chats"):
        if "status" in constraint["sqltext"]:
            op.drop_constraint(constraint["name"], "chats", type_="check")
    op.create_check_constraint("chats_status_check", "chats", f"status IN ({statuses})")


def upgrade() -> None:
    replace_status_constraint(
        "'idle', 'queued', 'running', 'completed', 'interrupted', 'cancelled', 'error', 'blocked'"
    )


def downgrade() -> None:
    op.execute("UPDATE chats SET status = 'error' WHERE status = 'blocked'")
    replace_status_constraint(
        "'idle', 'queued', 'running', 'completed', 'interrupted', 'cancelled', 'error'"
    )
