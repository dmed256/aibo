"""
Helper command to format the python service

    python -m aibo.cli.format

Options:

    # Fail if formatting was applied
    python -m aibo.cli.format --check

"""

import fire

from aibo.cli.common import call_command
from aibo.common.constants import PACKAGE_DIR


def main(check: bool = False) -> None:
    if check:
        call_command(f"black --check .", cwd=PACKAGE_DIR)
        call_command(f"isort --check --profile black .", cwd=PACKAGE_DIR)
    else:
        call_command(f"black .", cwd=PACKAGE_DIR)
        call_command(f"isort --profile black .", cwd=PACKAGE_DIR)


if __name__ == "__main__":
    fire.Fire(main)
