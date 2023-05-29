from typing import Any, Generic, Literal, Optional, Self, TypeVar

from pydantic import BaseModel, Field
from pydantic.generics import GenericModel

from aibo.common.types import StrEnum

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


class Ok(GenericModel, Generic[TOk]):
    kind: Literal["ok"] = "ok"
    value: TOk


class Error(GenericModel, Generic[TError]):
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
    value: TOk
    error_message: str
    error_code: Code


class Result(GenericModel, Generic[TOk, TError]):
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

    def update_error(
        self,
        *,
        error_message: Optional[str] = None,
        error_code: Optional[Error.Code] = None,
        value: TError | None = None,
    ) -> Self:
        error_value = self.error_value

        return self.error(
            error_message=error_value.error_message
            if error_message is None
            else error_message,
            error_code=error_value.error_code if error_code is None else error_code,
            value=error_value.value if value is None else value,
        )

    @property
    def is_ok(self):
        return isinstance(self.value, Ok)

    @property
    def is_error(self):
        return isinstance(self.value, Error)

    @staticmethod
    def is_undefined(value: Any):
        return isinstance(value, Undefined)

    @property
    def ok_value(self) -> TOk:
        if self.is_ok:
            return self.value.value

        raise Error(
            "Fetching ok_value from Error",
            error_code=Error.Code.FAILED_PRECONDITION,
        )

    @property
    def error_value(self) -> TError | None:
        if self.is_error:
            return self.value.value

        raise Error(
            "Fetching error_value from Ok",
            error_code=Error.Code.FAILED_PRECONDITION,
        )

    def validate(self):
        if self.is_error:
            raise self.value
