"""Persist goal mode and conversation usage."""

import sqlalchemy as sa
from alembic import op

revision = "0005_chat_goals"
down_revision = "0004_model_settings"
branch_labels = None
depends_on = None


def upgrade() -> None:
    op.add_column(
        "chats",
        sa.Column("goal_enabled", sa.Boolean, nullable=False, server_default=sa.true()),
    )
    op.add_column("chats", sa.Column("goal", sa.JSON, nullable=True))
    op.add_column(
        "chats",
        sa.Column("tokens_used", sa.BigInteger, nullable=False, server_default="0"),
    )
    op.add_column(
        "chats",
        sa.Column("elapsed_seconds", sa.Float, nullable=False, server_default="0"),
    )
    op.add_column(
        "chats", sa.Column("running_since", sa.DateTime(timezone=True), nullable=True)
    )
    op.execute("UPDATE chats SET goal_enabled = false WHERE kind = 'shadow'")
    op.execute(
        "UPDATE chats SET running_since = last_status_change_at WHERE status = 'running'"
    )


def downgrade() -> None:
    for name in (
        "running_since",
        "elapsed_seconds",
        "tokens_used",
        "goal",
        "goal_enabled",
    ):
        op.drop_column("chats", name)
