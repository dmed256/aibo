"""
Summary: ${message}

Migration information:
- Version: ${up_revision}
- Previous version: ${down_revision | comma,n}
- Created at: ${create_date}

"""
from alembic import op
from typing import Any, Optional
import sqlalchemy as sa


#---[ Alembic ]-----------------------------------
revision: str = ${repr(up_revision)}
down_revision: Optional[str] = ${repr(down_revision)}
branch_labels: Optional[Any] = ${repr(branch_labels)}
depends_on: Optional[Any] = ${repr(depends_on)}


#---[ Migration ]---------------------------------
def upgrade() -> None:
    ${upgrades if upgrades else "pass"}


def downgrade() -> None:
    ${downgrades if downgrades else "pass"}
