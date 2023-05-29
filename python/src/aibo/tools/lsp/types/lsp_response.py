import asyncio
import json
import os
import typing
from typing import Any, Optional

from pydantic import BaseModel, TypeAdapter

from aibo.tools.lsp.types.base_lsp_types import BaseLspModel, JsonRpcMessage
from aibo.tools.lsp.types.common_types import (
    DocumentHighlightKind,
    InsertTextFormat,
    InsertTextMode,
)
from aibo.tools.lsp.types.constants import LSP_HEADER_DELIMITER
from aibo.tools.lsp.types.location_types import Location, Range
from aibo.tools.lsp.types.lsp_request import (
    CompletionItem,
    LspRequest,
    LspRequestAdapter,
    MarkupContent,
    SignatureHelp,
    WorkspaceFolder,
)
from aibo.tools.lsp.types.server_capabilities import ServerCapabilities
from aibo.tools.lsp.types.text_document_types import (
    DocumentSymbol,
    SymbolInformation,
    WorkspaceSymbol,
)

__all__ = [
    "CompletionItemDefaults",
    "CompletionItemEditRange",
    "CompletionItemResolveResult",
    "CompletionItemResolveResultAdapter",
    "CompletionList",
    "CompletionList",
    "CompletionResult",
    "CompletionResultAdapter",
    "DeclarationResult",
    "DeclarationResultAdapter",
    "DefinitionResult",
    "DefinitionResultAdapter",
    "DocumentHighlightResult",
    "DocumentHighlightResultAdapter",
    "DocumentSymbolResult",
    "DocumentSymbolResultAdapter",
    "HoverResult",
    "HoverResultAdapter",
    "ImplementationResult",
    "ImplementationResultAdapter",
    "InitializationResult",
    "LspError",
    "LspResponse",
    "MarkedString",
    "MarkedStringWithLanguage",
    "ReferencesResult",
    "ReferencesResultAdapter",
    "ServerInfo",
    "SignatureHelpResult",
    "SignatureHelpResultAdapter",
    "TypeDefinitionResult",
    "TypeDefinitionResultAdapter",
    "WorkspaceFoldersResult",
    "WorkspaceFoldersResultAdapter",
    "WorkspaceSymbolResult",
    "WorkspaceSymbolResultAdapter",
]


class LspError(BaseModel):
    code: int
    message: str
    data: Optional[Any] = None


class LspResponse(JsonRpcMessage):
    result: Optional[Any] = None
    error: Optional[LspError] = None

    @classmethod
    async def read(cls, read_pipe: int) -> Optional["LspResponse"]:
        loop = asyncio.get_running_loop()
        return await loop.run_in_executor(None, cls._sync_read, read_pipe)

    @classmethod
    def _sync_read(cls, read_pipe: int) -> Optional["LspResponse"]:
        header = ""
        header_end = LSP_HEADER_DELIMITER + LSP_HEADER_DELIMITER
        while True:
            header += os.read(read_pipe, 1).decode("utf-8")
            if header.endswith(header_end):
                break

        headers = {
            kv[0].strip(): kv[1].strip()
            for header_content in header.split(LSP_HEADER_DELIMITER)
            if header_content.strip() and (kv := header_content.split(":"))
        }
        content_length = int(headers["Content-Length"])

        response_json = os.read(read_pipe, content_length).decode("utf-8").strip()
        response_dict = json.loads(response_json)

        # Sometimes we are passed a request ... but let's ignore them for now
        if "method" in response_dict:
            request = typing.cast(
                LspRequest, LspRequestAdapter.validate_json(response_json)
            )
            print(f"Ignoring request: {request}")
            return None

        return LspResponse.model_validate_json(response_json)


LspResponse.model_json_schema()


class ServerInfo(BaseLspModel):
    name: str
    version: Optional[str] = None


class InitializationResult(BaseLspModel):
    capabilities: ServerCapabilities
    server_info: Optional[ServerInfo] = None


class CompletionItemEditRange(BaseLspModel):
    insert: Range
    replace: Range


class CompletionItemDefaults(BaseLspModel):
    commitCharacters: Optional[str] = None
    editRange: Optional[Range | CompletionItemEditRange] = None
    insertTextFormat: Optional[InsertTextFormat] = None
    insertTextMode: Optional[InsertTextMode] = None
    data: Optional[Any] = None


class CompletionList(BaseLspModel):
    is_incomplete: bool
    item_defaults: Optional[CompletionItemDefaults] = None
    items: list[CompletionItem]


class DocumentHighlight(BaseLspModel):
    range: Range
    kind: Optional[DocumentHighlightKind] = None


class MarkedStringWithLanguage(BaseLspModel):
    language: str
    value: str


MarkedString = str | MarkedStringWithLanguage


class Hover(BaseLspModel):
    contents: MarkedString | list[MarkedString] | MarkupContent
    range: Optional[Range] = None


# TODO(dmed): Figure out why TypeAdapter is returning just UnionType
CompletionResult = Optional[list[CompletionItem] | CompletionList]
CompletionResultAdapter = TypeAdapter(CompletionResult)

CompletionItemResolveResult = CompletionItem
CompletionItemResolveResultAdapter = TypeAdapter(CompletionItemResolveResult)

DeclarationResult = Optional[Location | list[Location]]
DeclarationResultAdapter = TypeAdapter(DeclarationResult)

DefinitionResult = Optional[Location | list[Location]]
DefinitionResultAdapter = TypeAdapter(DefinitionResult)

DocumentHighlightResult = Optional[list[DocumentHighlight]]
DocumentHighlightResultAdapter = TypeAdapter(DocumentHighlightResult)

DocumentSymbolResult = Optional[list[DocumentSymbol] | list[SymbolInformation]]
DocumentSymbolResultAdapter = TypeAdapter(DocumentSymbolResult)

HoverResult = Optional[Hover]
HoverResultAdapter = TypeAdapter(HoverResult)

ImplementationResult = Optional[Location | list[Location]]
ImplementationResultAdapter = TypeAdapter(ImplementationResult)

ReferencesResult = Optional[list[Location]]
ReferencesResultAdapter = TypeAdapter(ReferencesResult)

SignatureHelpResult = Optional[SignatureHelp]
SignatureHelpResultAdapter = TypeAdapter(SignatureHelpResult)

TypeDefinitionResult = Optional[Location | list[Location]]
TypeDefinitionResultAdapter = TypeAdapter(TypeDefinitionResult)

WorkspaceFoldersResult = Optional[list[WorkspaceFolder]]
WorkspaceFoldersResultAdapter = TypeAdapter(WorkspaceFoldersResult)

WorkspaceSymbolResult = Optional[list[WorkspaceSymbol] | list[SymbolInformation]]
WorkspaceSymbolResultAdapter = TypeAdapter(WorkspaceSymbolResult)
