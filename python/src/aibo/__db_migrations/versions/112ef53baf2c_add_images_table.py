"""
Summary: Add images table

Migration information:
- Version: 112ef53baf2c
- Previous version: 8c2aa3d4c59a
- Created at: 2023-11-10 20:21:16.425003

"""

from typing import Any, Optional

import sqlalchemy as sa
from alembic import op

# ---[ Alembic ]-----------------------------------
revision: str = "112ef53baf2c"
down_revision: Optional[str] = "8c2aa3d4c59a"
branch_labels: Optional[Any] = None
depends_on: Optional[Any] = None


# ---[ Migration ]---------------------------------
def upgrade() -> None:
    # ### commands auto generated by Alembic - please adjust! ###
    op.create_table(
        "images",
        sa.Column("id", sa.String(), nullable=False),
        sa.Column("trace_id", sa.String(), nullable=False),
        sa.Column("conversation_id", sa.String(), nullable=True),
        sa.Column("format", sa.String(), nullable=False),
        sa.Column("contents_b64", sa.String(), nullable=False),
        sa.Column("created_at", sa.DateTime(), nullable=False),
        sa.ForeignKeyConstraint(
            ["conversation_id"],
            ["conversations.id"],
        ),
        sa.PrimaryKeyConstraint("id"),
    )
    with op.batch_alter_table("images", schema=None) as batch_op:
        batch_op.create_index(
            "images_idx_conversation_id", ["conversation_id"], unique=False
        )
        batch_op.create_index("images_idx_created_at", ["created_at"], unique=False)
        batch_op.create_index("images_idx_trace_id", ["trace_id"], unique=False)

    # ### end Alembic commands ###


def downgrade() -> None:
    # ### commands auto generated by Alembic - please adjust! ###
    with op.batch_alter_table("images", schema=None) as batch_op:
        batch_op.drop_index("images_idx_trace_id")
        batch_op.drop_index("images_idx_created_at")
        batch_op.drop_index("images_idx_conversation_id")

    op.drop_table("images")
    # ### end Alembic commands ###
