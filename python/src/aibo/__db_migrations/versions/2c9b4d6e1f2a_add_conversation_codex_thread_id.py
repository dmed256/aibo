"""
Summary: add conversation.codex_thread_id

Migration information:
- Version: 2c9b4d6e1f2a
- Previous version: 1bce28c8dbd2
- Created at: 2026-01-29 12:45:00.000000

"""

from typing import Any, Optional

import sqlalchemy as sa
from alembic import op

import aibo

# ---[ Alembic ]-----------------------------------
revision: str = "2c9b4d6e1f2a"
down_revision: Optional[str] = "1bce28c8dbd2"
branch_labels: Optional[Any] = None
depends_on: Optional[Any] = None


# ---[ Migration ]---------------------------------
def upgrade() -> None:
    with op.batch_alter_table("conversations", schema=None) as batch_op:
        batch_op.add_column(sa.Column("codex_thread_id", sa.String(), nullable=True))


def downgrade() -> None:
    with op.batch_alter_table("conversations", schema=None) as batch_op:
        batch_op.drop_column("codex_thread_id")
