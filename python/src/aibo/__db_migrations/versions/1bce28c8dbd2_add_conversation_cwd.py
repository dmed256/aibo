"""
Summary: add conversation.cwd

Migration information:
- Version: 1bce28c8dbd2
- Previous version: f43a2b565df6
- Created at: 2025-07-15 07:32:57.855779

"""

from typing import Any, Optional

import sqlalchemy as sa
from alembic import op

import aibo

# ---[ Alembic ]-----------------------------------
revision: str = "1bce28c8dbd2"
down_revision: Optional[str] = "f43a2b565df6"
branch_labels: Optional[Any] = None
depends_on: Optional[Any] = None


# ---[ Migration ]---------------------------------
def upgrade() -> None:
    with op.batch_alter_table("conversations", schema=None) as batch_op:
        batch_op.add_column(sa.Column("cwd", sa.String(), nullable=True))


def downgrade() -> None:
    # Probably not going to add the triggers back, just rerun migrations if needed :(
    with op.batch_alter_table("conversations", schema=None) as batch_op:
        batch_op.drop_column("cwd")
