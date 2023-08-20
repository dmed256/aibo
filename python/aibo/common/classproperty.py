from typing import Any, Callable, Generic, TypeVar, cast

T = TypeVar("T")


class classproperty(Generic[T]):
    def __init__(self, fn: Callable[..., T]):
        self.fn = fn

    def __get__(self, instance: Any, owner: Any) -> T:
        return cast(T, cast(Any, self.fn)(owner))
