"""Keep failures visible in conversation history."""

import sqlalchemy as sa
from alembic import op

revision = "0006_error_messages"
down_revision = "0005_chat_goals"
branch_labels = None
depends_on = None


def _constraint(kinds: str) -> None:
    checks = sa.inspect(op.get_bind()).get_check_constraints("chat_messages")
    for check in checks:
        if "kind" in check["sqltext"]:
            op.drop_constraint(check["name"], "chat_messages", type_="check")
    op.create_check_constraint(
        "chat_messages_kind_check", "chat_messages", f"kind IN ({kinds})"
    )


def upgrade() -> None:
    _constraint("'system', 'user', 'assistant', 'tool', 'event', 'error'")
    op.execute(
        """
        UPDATE chat_messages SET kind = 'error', content = CASE
            WHEN data->>'event' = 'turn/completed'
            THEN COALESCE(data->'turn'->'error'->>'message', 'The turn could not continue.')
            ELSE content END
        WHERE kind = 'event' AND (
            data->>'event' IN ('turn_start_failed', 'notification_failed')
            OR (data->>'event' = 'turn/completed' AND data->'turn'->>'status' = 'failed'))
    """
    )


def downgrade() -> None:
    op.execute("UPDATE chat_messages SET kind = 'event' WHERE kind = 'error'")
    _constraint("'system', 'user', 'assistant', 'tool', 'event'")
