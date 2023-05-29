import typing
from datetime import datetime, timedelta
from functools import lru_cache, wraps
from typing import Any, Callable, Dict, TypeVar, Union

T = TypeVar("T")
TCallable = TypeVar("TCallable", bound=Callable[..., Any])


def chunk_sequence(items: T, chunk_size: int) -> list[T]:
    return [
        items[start_index : start_index + chunk_size]  # type: ignore[index]
        for start_index in range(0, len(items), chunk_size)  # type: ignore[arg-type]
    ]


def read_file(filepath: str) -> str:
    with open(filepath, "r") as fd:
        return fd.read()
