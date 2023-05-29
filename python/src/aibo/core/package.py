from __future__ import annotations

import inspect
import typing
from typing import (
    TYPE_CHECKING,
    Any,
    Awaitable,
    Callable,
    Coroutine,
    Literal,
    Optional,
    Self,
    TypeVar,
    Union,
)

from pydantic import BaseModel, ConfigDict, Field
from pydantic.fields import FieldInfo

from aibo.common.types import JsonValue

if TYPE_CHECKING:
    from aibo.core.chat import (
        Conversation,
        FunctionResponseErrorType,
        Message,
        MessageContent,
    )

__all__ = ["FunctionContext", "Function", "Package"]

TFunctionCallable = Callable[..., Coroutine]

_PACKAGE_REGISTRY: dict[str, "Package"] = {}


class FunctionContext(BaseModel):
    conversation: "Conversation"
    function: "Function"
    arguments: JsonValue

    async def insert_message(
        self,
        *,
        error_type: Optional["FunctionResponseErrorType"] = None,
        error_message: Optional[str] = None,
        response: JsonValue,
    ) -> "Message":
        from aibo.core import chat

        content = chat.FunctionResponseContent(
            package=self.function.package.name,
            function=self.function.name,
            status=chat.FunctionResponseStatus.SUCCESS,
            arguments=self.arguments,
            error_type=error_type,
            error_message=error_message,
            response=response,
        )
        return await self.conversation.insert_message(
            source=chat.ProgrammaticSource(source=self.function.qualified_name),
            role=chat.MessageRole.FUNCTION,
            contents=[content],
        )


class Function(BaseModel):
    """
    Function the assistant is allowed to invoke
    """

    package: "Package"
    fn: Callable[..., Awaitable[JsonValue]] = Field(..., exclude=True)
    name: str
    description: str
    arguments_json_schema: dict[str, Any]

    async def __call__(
        self, *, conversation: "Conversation", arguments: dict[str, Any]
    ) -> "Message":
        ctx = FunctionContext(
            conversation=conversation,
            function=self,
            arguments=arguments,
        )
        response = await self.fn(ctx, **arguments)
        return await ctx.insert_message(response=response)

    @property
    def qualified_name(self) -> str:
        return Package.get_openai_function_name(
            package=self.package.name,
            function=self.name,
        )

    def to_openai(self) -> dict[str, Any]:
        return {
            "name": self.qualified_name,
            "description": self.description,
            "parameters": self.arguments_json_schema,
        }


class Package(BaseModel):
    """
    Grouping of functions
    """

    name: str
    description: str
    functions: dict[str, Function] = {}

    @staticmethod
    def get(package_name: str) -> Optional["Package"]:
        return _PACKAGE_REGISTRY.get(package_name)

    @staticmethod
    def registered_packages() -> list["Package"]:
        global _PACKAGE_REGISTRY
        return list(_PACKAGE_REGISTRY.values())

    def register(self) -> Self:
        global _PACKAGE_REGISTRY

        if self.name in _PACKAGE_REGISTRY:
            raise ValueError(f"Duplicate package names registered: {self.name}")

        _PACKAGE_REGISTRY[self.name] = self
        return self

    def function(self, fn: TFunctionCallable) -> TFunctionCallable:
        name = fn.__name__
        qualified_name = f"{self.name}.{name}"

        if name in self.functions:
            raise ValueError(
                f"Duplicate function names registered in package: {qualified_name}"
            )

        description = (fn.__doc__ or "").strip()
        arguments_json_schema = _params_to_json_schema_type(
            fn=fn, qualified_name=qualified_name
        )

        self.functions[name] = Function(
            package=self,
            fn=fn,
            name=name,
            description=description,
            arguments_json_schema=arguments_json_schema,
        )
        return fn

    @staticmethod
    def get_openai_function_name(*, package: str, function: str) -> str:
        return f"{package}__{function}"

    @staticmethod
    def split_openai_function_name(function_name: str) -> Optional[tuple[str, str]]:
        parts = function_name.split("__")
        if len(parts) != 2:
            return None

        package, function = parts
        return package, function

    def to_openai(self) -> list[dict[str, Any]]:
        return [
            self.functions[function_name].to_openai()
            for function_name in sorted(self.functions.keys())
        ]


def _params_to_json_schema_type(
    *, fn: TFunctionCallable, qualified_name: str
) -> dict[str, Any]:
    fn_signature = inspect.signature(fn)
    arguments_json_schema: dict[str, Any] = {
        "type": "object",
        "properties": {},
        "required": [],
    }
    props = arguments_json_schema["properties"]
    required = arguments_json_schema["required"]

    ctx_arg, *args = fn_signature.parameters.values()
    if ctx_arg.annotation is not FunctionContext:
        raise ValueError(
            "All package function args should start with a FunctionContext positional argument"
        )

    for arg in args:
        field_info = arg.default
        if not isinstance(field_info, FieldInfo):
            raise ValueError(
                "All package function args should have a pydantic.Field(...)"
                + f" default value: {qualified_name}"
            )

        if not field_info.description:
            raise ValueError(
                "All package function args have a description:"
                + f" {qualified_name}(..., {arg.name})"
            )

        props[arg.name] = _typing_to_json_schema_type(
            arg_name=arg.name,
            qualified_name=qualified_name,
            typing_type=arg.annotation,
            description=field_info.description,
        )
        required.append(arg.name)

    return arguments_json_schema


def _typing_to_json_schema_type(
    *,
    arg_name: str,
    typing_type: Any,
    qualified_name: str,
    description: Optional[str] = None,
) -> dict[str, Any]:
    json_schema: dict[str, Any] = {}
    if description is not None:
        json_schema["description"] = description

    if typing_type is type(None):
        return {**json_schema, "type": "null"}

    if typing_type is int:
        return {**json_schema, "type": "integer"}

    if typing_type is float:
        return {**json_schema, "type": "number"}

    if typing_type is str:
        return {**json_schema, "type": "string"}

    if typing_type is bool:
        return {**json_schema, "type": "boolean"}

    type_origin = typing.get_origin(typing_type)
    if type_origin is list:
        list_typing_type = typing.get_args(typing_type)[0]
        return {
            **json_schema,
            "type": "array",
            "items": _typing_to_json_schema_type(
                arg_name=arg_name,
                qualified_name=qualified_name,
                typing_type=list_typing_type,
            ),
        }

    if type_origin is tuple:
        return {
            **json_schema,
            "type": "array",
            "prefixItems": [
                _typing_to_json_schema_type(
                    arg_name=typing_type_arg,
                    qualified_name=qualified_name,
                    typing_type=typing_type_arg,
                )
                for typing_type_arg in typing.get_args(typing_type)
            ],
        }

    if type_origin is Literal:
        return {
            **json_schema,
            "enum": list(typing.get_args(typing_type)),
        }

    if type_origin is Union:
        return {
            **json_schema,
            "anyOf": [
                _typing_to_json_schema_type(
                    arg_name=arg_name,
                    qualified_name=qualified_name,
                    typing_type=typing_type_arg,
                )
                for typing_type_arg in typing.get_args(typing_type)
            ],
        }

    raise ValueError(
        f"Unimplemented json_schema type: {qualified_name}(..., {arg_name}: {typing_type})"
    )
