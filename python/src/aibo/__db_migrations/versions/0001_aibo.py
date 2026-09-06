"""Create the aibo schema."""

from typing import Any

import sqlalchemy as sa
from alembic import op

revision: str = "0001_aibo"
down_revision: str | None = None
branch_labels: Any = None
depends_on: Any = None


def upgrade() -> None:
    op.create_table(
        "locations",
        sa.Column("id", sa.Uuid(), nullable=False),
        sa.Column("name", sa.String(length=80), nullable=False),
        sa.Column("path", sa.Text(), nullable=False),
        sa.Column("created_at", sa.DateTime(timezone=True), nullable=False),
        sa.Column("updated_at", sa.DateTime(timezone=True), nullable=False),
        sa.PrimaryKeyConstraint("id"),
        sa.UniqueConstraint("name"),
    )
    op.create_table(
        "projects",
        sa.Column("id", sa.Uuid(), nullable=False),
        sa.Column("name", sa.String(length=80), nullable=False),
        sa.Column("description", sa.String(length=500), nullable=False),
        sa.Column("archived_at", sa.DateTime(timezone=True), nullable=True),
        sa.Column("created_at", sa.DateTime(timezone=True), nullable=False),
        sa.Column("updated_at", sa.DateTime(timezone=True), nullable=False),
        sa.PrimaryKeyConstraint("id"),
        sa.UniqueConstraint("name"),
    )
    op.create_table(
        "counters",
        sa.Column("name", sa.String(length=80), nullable=False),
        sa.Column("value", sa.Integer(), nullable=False),
        sa.PrimaryKeyConstraint("name"),
    )
    op.create_table(
        "chats",
        sa.Column("id", sa.Uuid(), nullable=False),
        sa.Column("kind", sa.String(length=16), nullable=False),
        sa.Column("bot_number", sa.Integer(), nullable=True),
        sa.Column("title", sa.String(length=300), nullable=False),
        sa.Column("status", sa.String(length=16), nullable=False),
        sa.Column("codex_thread_id", sa.String(length=100), nullable=True),
        sa.Column("active_turn_id", sa.String(length=100), nullable=True),
        sa.Column("location_id", sa.Uuid(), nullable=True),
        sa.Column("project_id", sa.Uuid(), nullable=True),
        sa.Column("origin_chat_id", sa.Uuid(), nullable=True),
        sa.Column("shadow_for_turn_id", sa.String(length=100), nullable=True),
        sa.Column("activity_at", sa.DateTime(timezone=True), nullable=False),
        sa.Column("last_user_message_at", sa.DateTime(timezone=True), nullable=True),
        sa.Column("last_status_change_at", sa.DateTime(timezone=True), nullable=False),
        sa.Column("archived_at", sa.DateTime(timezone=True), nullable=True),
        sa.Column("created_at", sa.DateTime(timezone=True), nullable=False),
        sa.Column("updated_at", sa.DateTime(timezone=True), nullable=False),
        sa.CheckConstraint("kind IN ('manager', 'bot', 'shadow')"),
        sa.CheckConstraint(
            "status IN ('idle', 'queued', 'running', 'completed', "
            "'interrupted', 'cancelled', 'error')"
        ),
        sa.CheckConstraint(
            "(kind = 'bot' AND bot_number BETWEEN 0 AND 255) "
            "OR (kind != 'bot' AND bot_number IS NULL)"
        ),
        sa.ForeignKeyConstraint(["location_id"], ["locations.id"], ondelete="SET NULL"),
        sa.ForeignKeyConstraint(["project_id"], ["projects.id"], ondelete="SET NULL"),
        sa.ForeignKeyConstraint(["origin_chat_id"], ["chats.id"], ondelete="CASCADE"),
        sa.PrimaryKeyConstraint("id"),
        sa.UniqueConstraint("codex_thread_id"),
        sa.UniqueConstraint("shadow_for_turn_id"),
    )
    op.create_index("chats_activity_idx", "chats", ["activity_at"])
    op.create_index("chats_bot_number_idx", "chats", ["bot_number", "created_at"])
    op.create_table(
        "chat_messages",
        sa.Column("id", sa.Uuid(), nullable=False),
        sa.Column("chat_id", sa.Uuid(), nullable=False),
        sa.Column("kind", sa.String(length=16), nullable=False),
        sa.Column("content", sa.Text(), nullable=False),
        sa.Column("data", sa.JSON(), nullable=False),
        sa.Column("external_id", sa.String(length=200), nullable=True),
        sa.Column("created_at", sa.DateTime(timezone=True), nullable=False),
        sa.CheckConstraint("kind IN ('system', 'user', 'assistant', 'tool', 'event')"),
        sa.ForeignKeyConstraint(["chat_id"], ["chats.id"], ondelete="CASCADE"),
        sa.PrimaryKeyConstraint("id"),
        sa.UniqueConstraint("chat_id", "external_id"),
    )
    op.create_index(
        "chat_messages_chat_idx", "chat_messages", ["chat_id", "created_at"]
    )
    op.create_table(
        "notifications",
        sa.Column("id", sa.Uuid(), nullable=False),
        sa.Column("chat_id", sa.Uuid(), nullable=False),
        sa.Column("source_chat_id", sa.Uuid(), nullable=True),
        sa.Column("body", sa.Text(), nullable=False),
        sa.Column("created_at", sa.DateTime(timezone=True), nullable=False),
        sa.Column("read_at", sa.DateTime(timezone=True), nullable=True),
        sa.ForeignKeyConstraint(["chat_id"], ["chats.id"], ondelete="CASCADE"),
        sa.ForeignKeyConstraint(["source_chat_id"], ["chats.id"], ondelete="SET NULL"),
        sa.PrimaryKeyConstraint("id"),
        sa.UniqueConstraint("source_chat_id"),
    )
    op.create_index(
        "notifications_read_idx", "notifications", ["read_at", "created_at"]
    )
    op.bulk_insert(
        sa.table(
            "counters",
            sa.column("name", sa.String()),
            sa.column("value", sa.Integer()),
        ),
        [{"name": "next_bot_number", "value": 0}],
    )


def downgrade() -> None:
    op.drop_index("notifications_read_idx", table_name="notifications")
    op.drop_table("notifications")
    op.drop_index("chat_messages_chat_idx", table_name="chat_messages")
    op.drop_table("chat_messages")
    op.drop_index("chats_bot_number_idx", table_name="chats")
    op.drop_index("chats_activity_idx", table_name="chats")
    op.drop_table("chats")
    op.drop_table("counters")
    op.drop_table("projects")
    op.drop_table("locations")
