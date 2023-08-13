"""
Helper command to format the python service

    python -m aibo.cli.format

Options:

    python -m aibo.cli.format

"""
import fire
import uvicorn

from aibo.cli.common import PKG_DIR


def main(port: int = 5000):
    uvicorn.run(
        "aibo.server.main:app",
        port=port,
        reload=True,
        reload_dirs=[PKG_DIR],
    )


if __name__ == "__main__":
    fire.Fire(main)
