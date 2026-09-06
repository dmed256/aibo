from __future__ import annotations

import datetime as dt
import logging
import os
from collections.abc import Callable
from pathlib import Path
from typing import TextIO

from aibo.common.constants import Env


class DailyFileHandler(logging.Handler):
    def __init__(
        self,
        directory: Path,
        clock: Callable[[], dt.date] = dt.date.today,
    ) -> None:
        super().__init__()
        self.directory = directory
        self.clock = clock
        self.day: dt.date | None = None
        self.stream: TextIO | None = None

    def emit(self, record: logging.LogRecord) -> None:
        try:
            day = self.clock()
            if day != self.day:
                if self.stream:
                    self.stream.close()
                self.directory.mkdir(parents=True, exist_ok=True)
                self.stream = (self.directory / f"{day.isoformat()}.log").open(
                    "a", encoding="utf-8"
                )
                self.day = day
            if self.stream:
                self.stream.write(self.format(record) + "\n")
                self.stream.flush()
        except Exception:
            self.handleError(record)

    def close(self) -> None:
        if self.stream:
            self.stream.close()
            self.stream = None
        super().close()


def configure_logging(component: str, env: Env | None = None) -> None:
    env = env or Env.get()
    console = os.environ.get("AIBO_LOG_CONSOLE", "1") != "0"
    formatter = logging.Formatter(
        f"%(asctime)s {component} %(levelname)s %(name)s: %(message)s"
    )

    targets = [
        logging.getLogger(),
        logging.getLogger("uvicorn"),
        logging.getLogger("uvicorn.access"),
    ]
    for logger in targets:
        for handler in list(logger.handlers):
            if isinstance(handler, DailyFileHandler):
                logger.removeHandler(handler)
                handler.close()
            elif not console and isinstance(handler, logging.StreamHandler):
                logger.removeHandler(handler)
        handler = DailyFileHandler(env.logs_dir)
        handler.setFormatter(formatter)
        logger.addHandler(handler)
        logger.setLevel(logging.INFO)

    if console and not any(
        isinstance(handler, logging.StreamHandler)
        and not isinstance(handler, DailyFileHandler)
        for handler in logging.getLogger().handlers
    ):
        console_handler = logging.StreamHandler()
        console_handler.setFormatter(formatter)
        logging.getLogger().addHandler(console_handler)
