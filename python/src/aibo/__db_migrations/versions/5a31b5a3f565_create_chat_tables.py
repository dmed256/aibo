"""
Summary: Create chat tables

Migration information:
- Version: 5a31b5a3f565
- Previous version:
- Created at: 2023-10-08 20:10:15.035337

"""
from typing import Any, Optional

import sqlalchemy as sa
from alembic import op

# ---[ Alembic ]-----------------------------------
revision: str = "5a31b5a3f565"
down_revision: Optional[str] = None
branch_labels: Optional[Any] = None
depends_on: Optional[Any] = None


# ---[ Migration ]---------------------------------
def upgrade() -> None:
    op.create_table(
        "conversations",
        sa.Column("id", sa.String(), nullable=False),
        sa.Column("trace_id", sa.String(), nullable=False),
        sa.Column("title", sa.String(), nullable=False),
        sa.Column("openai_model_source", sa.String(), nullable=False),
        sa.Column("enabled_package_names", sa.String(), nullable=False),
        sa.Column("conversation_depth", sa.Integer(), nullable=False),
        sa.Column("created_at", sa.DateTime(), nullable=False),
        sa.Column("deleted_at", sa.DateTime(), nullable=True),
        sa.PrimaryKeyConstraint("id"),
    )
    with op.batch_alter_table("conversations", schema=None) as batch_op:
        batch_op.create_index(
            "conversations_idx_created_at", ["created_at"], unique=False
        )
        batch_op.create_index("conversations_idx_trace_id", ["trace_id"], unique=False)

    op.create_table(
        "messages",
        sa.Column("id", sa.String(), nullable=False),
        sa.Column("trace_id", sa.String(), nullable=False),
        sa.Column("conversation_id", sa.String(), nullable=True),
        sa.Column("parent_id", sa.String(), nullable=True),
        sa.Column("original_parent_id", sa.String(), nullable=True),
        sa.Column("source_text", sa.String(), nullable=False),
        sa.Column("role", sa.String(), nullable=False),
        sa.Column("content_text", sa.String(), nullable=False),
        sa.Column("source", sa.String(), nullable=True),
        sa.Column("contents", sa.String(), nullable=True),
        sa.Column("created_at", sa.DateTime(), nullable=False),
        sa.Column("deleted_at", sa.DateTime(), nullable=True),
        sa.ForeignKeyConstraint(
            ["conversation_id"],
            ["conversations.id"],
        ),
        sa.ForeignKeyConstraint(
            ["original_parent_id"],
            ["messages.id"],
        ),
        sa.ForeignKeyConstraint(
            ["parent_id"],
            ["messages.id"],
        ),
        sa.PrimaryKeyConstraint("id"),
    )
    with op.batch_alter_table("messages", schema=None) as batch_op:
        batch_op.create_index(
            "messages_idx_conversation_id", ["conversation_id"], unique=False
        )
        batch_op.create_index("messages_idx_created_at", ["created_at"], unique=False)
        batch_op.create_index("messages_idx_trace_id", ["trace_id"], unique=False)


def downgrade() -> None:
    with op.batch_alter_table("messages", schema=None) as batch_op:
        batch_op.drop_index("messages_idx_trace_id")
        batch_op.drop_index("messages_idx_created_at")
        batch_op.drop_index("messages_idx_conversation_id")

    op.drop_table("messages")
    with op.batch_alter_table("conversations", schema=None) as batch_op:
        batch_op.drop_index("conversations_idx_trace_id")
        batch_op.drop_index("conversations_idx_created_at")

    op.drop_table("conversations")
