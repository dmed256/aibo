"""
Helper command to run tests

    python -m aibo.cli.test

"""

import sys

import fire
import pytest

from aibo.cli.common import call_command
from aibo.common.constants import PACKAGE_DIR


def main() -> None:
    call_command(f"pytest -n auto", cwd=PACKAGE_DIR)


if __name__ == "__main__":
    fire.Fire(main)
