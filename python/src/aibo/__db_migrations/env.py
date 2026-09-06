from alembic import context

from aibo.db.client import get_sync_engine
from aibo.db.models import BaseDBModel

config = context.config
target_metadata = BaseDBModel.metadata


def run_migrations_offline() -> None:
    url = config.get_main_option("sqlalchemy.url")
    context.configure(
        url=url,
        target_metadata=target_metadata,
        literal_binds=True,
        dialect_opts={"paramstyle": "named"},
    )

    with context.begin_transaction():
        context.run_migrations()


def run_migrations_online() -> None:
    with get_sync_engine().begin() as conn:
        context.configure(
            connection=conn,
            target_metadata=target_metadata,
            render_as_batch=conn.dialect.name == "sqlite",
        )

        with context.begin_transaction():
            context.run_migrations()


if context.is_offline_mode():
    run_migrations_offline()
else:
    run_migrations_online()
