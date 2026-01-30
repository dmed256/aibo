"""
Helper command to start the python service

    python -m aibo.cli.start

Options:

    # Set the port
    python -m aibo.cli.start --port [PORT]

"""

import os
from typing import Any

import fire
import uvicorn

from aibo.common.constants import PACKAGE_DIR


def main(
    port: int = 5000,
    mode: str = "prod",
) -> None:
    os.chdir(PACKAGE_DIR)

    mode_kwargs: dict[str, Any] = {}
    if mode == "prod":
        mode_kwargs.update(workers=8)
    else:
        mode_kwargs.update(
            reload=True,
            reload_dirs=[PACKAGE_DIR],
        )

    uvicorn.run(
        "aibo.server.main:create_app",
        port=port,
        factory=True,
        **mode_kwargs,
    )


if __name__ == "__main__":
    fire.Fire(main)
