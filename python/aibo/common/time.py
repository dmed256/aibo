import datetime as dt

__all__ = ["now_utc"]


def now_utc():
    return dt.datetime.now(dt.timezone.utc)
