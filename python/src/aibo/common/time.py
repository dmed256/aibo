import datetime as dt

__all__ = ["now_utc"]


def now_utc() -> dt.datetime:
    return dt.datetime.now(dt.timezone.utc)
