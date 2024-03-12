"""
Helper command to start the python service

    python -m aibo.cli.start

Options:

    # Set the port
    python -m aibo.cli.start --port [PORT]

"""
import os

import fire
import uvicorn

from aibo.common.constants import PACKAGE_DIR


def main(port: int = 5000) -> None:
    os.chdir(PACKAGE_DIR)
    uvicorn.run(
        "aibo.server.main:create_app",
        port=port,
        factory=True,
        reload=True,
        reload_dirs=[PACKAGE_DIR],
    )


if __name__ == "__main__":
    fire.Fire(main)
