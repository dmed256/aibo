"""Number managers without changing existing chat identities."""

import sqlalchemy as sa
from alembic import op

revision = "0002_manager_numbers"
down_revision = "0001_aibo"
branch_labels = None
depends_on = None


def upgrade() -> None:
    op.add_column("chats", sa.Column("manager_number", sa.Integer(), nullable=True))
    op.execute(
        """
        WITH numbered AS (
            SELECT id, (row_number() OVER (ORDER BY created_at, id) - 1) % 256 AS n
            FROM chats WHERE kind = 'manager'
        )
        UPDATE chats SET manager_number = numbered.n
        FROM numbered WHERE chats.id = numbered.id
    """
    )
    op.execute(
        """
        INSERT INTO counters (name, value)
        SELECT 'next_manager_number', count(*) % 256 FROM chats WHERE kind = 'manager'
    """
    )
    op.create_check_constraint(
        "chats_manager_number_check",
        "chats",
        "(kind = 'manager' AND manager_number IS NOT NULL "
        "AND manager_number BETWEEN 0 AND 255) "
        "OR (kind != 'manager' AND manager_number IS NULL)",
    )
    op.create_index(
        "chats_manager_number_idx", "chats", ["manager_number", "created_at"]
    )


def downgrade() -> None:
    op.drop_index("chats_manager_number_idx", table_name="chats")
    op.drop_constraint("chats_manager_number_check", "chats", type_="check")
    op.drop_column("chats", "manager_number")
    op.execute("DELETE FROM counters WHERE name = 'next_manager_number'")
