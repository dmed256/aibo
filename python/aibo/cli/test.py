"""
Helper command to run tests

    python -m aibo.cli.test

Options:

    python -m aibo.cli.test

"""

import sys

import fire
import pytest

from aibo.cli.common import PKG_DIR, call_command


def main() -> None:
    call_command(f"pytest -n auto", cwd=PKG_DIR)


if __name__ == "__main__":
    fire.Fire(main)
