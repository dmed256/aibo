from enum import StrEnum
from typing import Any, Generic, Literal, Optional, Self, TypeVar

from pydantic import BaseModel, Field

__all__ = [
    "Result",
    "Ok",
    "Error",
    "Undefined",
    "UNDEFINED",
]

TOk = TypeVar("TOk")
TError = TypeVar("TError")


class Undefined(BaseModel):
    pass


UNDEFINED = Undefined()


class Ok(BaseModel, Generic[TOk]):
    kind: Literal["ok"] = "ok"
    value: TOk


class Error(BaseModel, Generic[TError]):
    class Code(StrEnum):
        # gRPC error codes
        CANCELLED = "cancelled"
        UNKNOWN = "unknown"
        INVALID_ARGUMENT = "invalid_argument"
        DEADLINE_EXCEEDED = "deadline_exceeded"
        NOT_FOUND = "not_found"
        ALREADY_EXISTS = "already_exists"
        PERMISSION_DENIED = "permission_denied"
        RESOURCE_EXHAUSTED = "resource_exhausted"
        FAILED_PRECONDITION = "failed_precondition"
        ABORTED = "aborted"
        OUT_OF_RANGE = "out_of_range"
        UNIMPLEMENTED = "unimplemented"
        INTERNAL = "internal"
        UNAVAILABLE = "unavailable"
        DATA_LOSS = "data_loss"
        UNAUTHENTICATED = "unauthenticated"
        # Custom error codes
        INVALID_COMPLETION = "invalid_completion"

    kind: Literal["error"] = "error"
    value: Optional[TError] = None
    error_message: str
    error_code: Code


class Result(BaseModel, Generic[TOk, TError]):
    value: Ok[TOk] | Error[TError] = Field(..., discriminator="kind")

    @classmethod
    def ok(cls, value: TOk) -> Self:
        return cls(value=Ok(value=value))

    @classmethod
    def error(
        cls,
        error_message: str,
        *,
        error_code: Error.Code,
        value: TError | None = None,
    ) -> Self:
        return cls(
            value=Error(error_message=error_message, error_code=error_code, value=value)
        )

    @staticmethod
    def is_undefined(value: Any) -> bool:
        return isinstance(value, Undefined)

    @property
    def ok_value(self) -> TOk:
        if isinstance(self.value, Ok):
            return self.value.value

        raise Error(  # type: ignore[misc]
            error_message="Fetching ok_value from Error",
            error_code=Error.Code.FAILED_PRECONDITION,
        )

    @property
    def error_value(self) -> TError | None:
        if isinstance(self.value, Error):
            return self.value.value

        raise Error(  # type: ignore[misc]
            error_message="Fetching error_value from Ok",
            error_code=Error.Code.FAILED_PRECONDITION,
        )
