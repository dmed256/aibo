"""Start the long-lived Codex app-server bouncer."""

import fire
import uvicorn


def main(port: int = 5010) -> None:
    uvicorn.run(
        "aibo.bouncer.main:create_app",
        host="127.0.0.1",
        port=port,
        factory=True,
    )


if __name__ == "__main__":
    fire.Fire(main)
