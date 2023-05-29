"""
Run DB migrations

    python -m aibo.cli.migrate

"""

import fire

from aibo.db.client import migrate_db


def main() -> None:
    migrate_db()


if __name__ == "__main__":
    fire.Fire(main)
