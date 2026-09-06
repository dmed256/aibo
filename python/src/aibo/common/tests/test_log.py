import datetime as dt
import logging
from pathlib import Path

from aibo.common.log import DailyFileHandler


def test_daily_log_rotation(tmp_path: Path) -> None:
    day = [dt.date(2026, 9, 4)]
    handler = DailyFileHandler(tmp_path, clock=lambda: day[0])
    handler.setFormatter(logging.Formatter("%(message)s"))

    handler.handle(logging.makeLogRecord({"msg": "first"}))
    day[0] = dt.date(2026, 9, 5)
    handler.handle(logging.makeLogRecord({"msg": "second"}))
    handler.close()

    assert (tmp_path / "2026-09-04.log").read_text() == "first\n"
    assert (tmp_path / "2026-09-05.log").read_text() == "second\n"
