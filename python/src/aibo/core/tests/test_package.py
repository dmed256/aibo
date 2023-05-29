from typing import Literal, Optional, Union

import pytest
from pydantic import Field

from aibo.core.package import FunctionContext, _params_to_json_schema_type


class TestJsonSchema:
    def test_params_to_json_schema_type_no_arguments(self) -> None:
        async def fn(ctx: FunctionContext) -> None:
            pass

        result = _params_to_json_schema_type(fn=fn, qualified_name="test_fn")

        assert result["properties"] == {}
        assert result["required"] == []

    def test_params_to_json_schema_type_missing_field(self) -> None:
        async def fn(ctx: FunctionContext, arg1: int, arg2: str) -> None:
            pass

        with pytest.raises(ValueError):
            _params_to_json_schema_type(fn=fn, qualified_name="test_fn")

    def test_params_to_json_schema_type_different_types(self) -> None:
        async def fn(
            ctx: FunctionContext,
            arg1: int = Field(..., description="Argument 1"),
            arg2: str = Field(..., description="Argument 2"),
            arg3: list[int] = Field(..., description="Argument 3"),
        ) -> None:
            pass

        result = _params_to_json_schema_type(fn=fn, qualified_name="test_fn")

        assert result["properties"] == {
            "arg1": {"type": "integer", "description": "Argument 1"},
            "arg2": {"type": "string", "description": "Argument 2"},
            "arg3": {
                "type": "array",
                "items": {"type": "integer"},
                "description": "Argument 3",
            },
        }
        assert result["required"] == ["arg1", "arg2", "arg3"]

    def test_params_to_json_schema_type_literal_types(self) -> None:
        async def fn(
            ctx: FunctionContext,
            arg1: Literal["foo", "bar"] = Field(..., description="Argument 1"),
        ) -> None:
            pass

        result = _params_to_json_schema_type(fn=fn, qualified_name="test_fn")

        assert result["properties"] == {
            "arg1": {"enum": ["foo", "bar"], "description": "Argument 1"}
        }
        assert result["required"] == ["arg1"]

    def test_params_to_json_schema_type_optional_types(self) -> None:
        async def fn(
            ctx: FunctionContext,
            arg1: Optional[int] = Field(None, description="Argument 1"),
        ) -> None:
            pass

        result = _params_to_json_schema_type(fn=fn, qualified_name="test_fn")

        assert result["properties"] == {
            "arg1": {
                "anyOf": [{"type": "integer"}, {"type": "null"}],
                "description": "Argument 1",
            }
        }
        result["required"] == []

    def test_params_to_json_schema_type_union_types(self) -> None:
        async def fn(
            ctx: FunctionContext,
            arg1: Union[int, str] = Field(..., description="Argument 1"),
        ) -> None:
            pass

        result = _params_to_json_schema_type(fn=fn, qualified_name="test_fn")

        assert result["properties"] == {
            "arg1": {
                "anyOf": [{"type": "integer"}, {"type": "string"}],
                "description": "Argument 1",
            }
        }
        result["required"] == ["arg1"]
