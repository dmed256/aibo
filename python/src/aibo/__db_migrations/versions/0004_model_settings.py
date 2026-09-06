"""Shared model choices for conversations and title generation."""

import sqlalchemy as sa
from alembic import op

revision = "0004_model_settings"
down_revision = "0003_blocked_status"
branch_labels = None
depends_on = None


def upgrade() -> None:
    op.create_table(
        "model_settings",
        sa.Column("id", sa.Integer, primary_key=True),
        sa.Column("models", sa.JSON, nullable=False),
    )


def downgrade() -> None:
    op.drop_table("model_settings")
