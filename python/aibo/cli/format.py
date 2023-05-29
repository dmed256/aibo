"""
Helper command to format the python service

    python -m aibo.cli.format

Options:

    python -m aibo.cli.format

"""
import fire

from aibo.cli.common import PKG_DIR, call_command


def main(port: int = 5000):
    call_command(f"black .", cwd=PKG_DIR)
    call_command(f"isort --profile black .", cwd=PKG_DIR)


if __name__ == "__main__":
    fire.Fire(main)
