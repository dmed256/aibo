import json
import os
from typing import Any, Literal, Optional
from uuid import uuid4

from pydantic import BaseModel, ConfigDict, Field

from aibo.tools.lsp.types.constants import LSP_HEADER_DELIMITER

__all__ = ["BaseLspModel", "JsonRpcMessage"]


def to_lower_camel(name: str) -> str:
    upper = "".join(word.capitalize() for word in name.split("_"))
    return upper[:1].lower() + upper[1:]


def gen_message_id() -> str:
    return str(uuid4())


class BaseLspModel(BaseModel):
    model_config = ConfigDict(populate_by_name=True, alias_generator=to_lower_camel)


class JsonRpcMessage(BaseLspModel):
    jsonrpc: Literal["2.0"] = "2.0"
    id: str = Field(default_factory=gen_message_id)

    def to_jsonrpc_message(self) -> str:
        request = self.model_dump_json(by_alias=True, exclude_none=True)
        return (
            f"Content-Length: {len(request)}{LSP_HEADER_DELIMITER}"
            + LSP_HEADER_DELIMITER
            + request
        )

    def write(self, write_pipe: int) -> None:
        os.write(write_pipe, self.to_jsonrpc_message().encode())
