"""
Summary: Add conversation foreign keys

Migration information:
- Version: 8c2aa3d4c59a
- Previous version: 5a31b5a3f565
- Created at: 2023-10-08 20:10:54.323046

"""
from typing import Any, Optional

import sqlalchemy as sa
from alembic import op

# ---[ Alembic ]-----------------------------------
revision: str = "8c2aa3d4c59a"
down_revision: Optional[str] = "5a31b5a3f565"
branch_labels: Optional[Any] = None
depends_on: Optional[Any] = None


# ---[ Migration ]---------------------------------
def upgrade() -> None:
    with op.batch_alter_table("conversations", schema=None) as batch_op:
        batch_op.add_column(sa.Column("root_message_id", sa.String(), nullable=True))
        batch_op.add_column(sa.Column("current_message_id", sa.String(), nullable=True))
        batch_op.add_column(sa.Column("origin_message_id", sa.String(), nullable=True))
        batch_op.create_foreign_key(
            "conversations_fk_origin_message_id",
            "messages",
            ["origin_message_id"],
            ["id"],
        )
        batch_op.create_foreign_key(
            "conversations_fk_current_message_id",
            "messages",
            ["current_message_id"],
            ["id"],
        )
        batch_op.create_foreign_key(
            "conversations_fk_root_message_id", "messages", ["root_message_id"], ["id"]
        )


def downgrade() -> None:
    with op.batch_alter_table("conversations", schema=None) as batch_op:
        batch_op.drop_constraint(
            "conversations_fk_origin_message_id", type_="foreignkey"
        )
        batch_op.drop_constraint(
            "conversations_fk_current_message_id", type_="foreignkey"
        )
        batch_op.drop_constraint("conversations_fk_root_message_id", type_="foreignkey")
        batch_op.drop_column("origin_message_id")
        batch_op.drop_column("current_message_id")
        batch_op.drop_column("root_message_id")
