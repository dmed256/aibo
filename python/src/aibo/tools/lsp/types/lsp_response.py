import asyncio
import json
import os
import typing
from typing import Any, Union

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
    data: Any | None = None


class LspResponse(JsonRpcMessage):
    result: Any | None = None
    error: LspError | None = None

    @classmethod
    async def read(cls, read_pipe: int) -> Union["LspResponse", None]:
        loop = asyncio.get_running_loop()
        return await loop.run_in_executor(None, cls._sync_read, read_pipe)

    @classmethod
    def _sync_read(cls, read_pipe: int) -> Union["LspResponse", None]:
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
        if (content_length_str := headers.get("Content-Length")) is None:
            return None

        content_length = int(content_length_str)

        response_json = ""
        while content_length:
            content_to_read = min(content_length, 65536)
            content_bytes = os.read(read_pipe, content_to_read)
            response_json += content_bytes.decode("utf-8").strip()
            content_length -= len(content_bytes)

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
    version: str | None = None


class InitializationResult(BaseLspModel):
    capabilities: ServerCapabilities
    server_info: ServerInfo | None = None


class CompletionItemEditRange(BaseLspModel):
    insert: Range
    replace: Range


class CompletionItemDefaults(BaseLspModel):
    commitCharacters: str | None = None
    editRange: Range | CompletionItemEditRange | None = None
    insertTextFormat: InsertTextFormat | None = None
    insertTextMode: InsertTextMode | None = None
    data: Any | None = None


class CompletionList(BaseLspModel):
    is_incomplete: bool
    item_defaults: CompletionItemDefaults | None = None
    items: list[CompletionItem]


class DocumentHighlight(BaseLspModel):
    range: Range
    kind: DocumentHighlightKind | None = None


class MarkedStringWithLanguage(BaseLspModel):
    language: str
    value: str


MarkedString = str | MarkedStringWithLanguage


class Hover(BaseLspModel):
    contents: MarkedString | list[MarkedString] | MarkupContent
    range: Range | None = None


# TODO(dmed): Figure out why TypeAdapter is returning just UnionType
CompletionResult = list[CompletionItem | None | CompletionList]
CompletionResultAdapter = TypeAdapter(CompletionResult)

CompletionItemResolveResult = CompletionItem
CompletionItemResolveResultAdapter = TypeAdapter(CompletionItemResolveResult)

DeclarationResult = Location | list[Location]
DeclarationResultAdapter = TypeAdapter(DeclarationResult)

DefinitionResult = Location | list[Location]
DefinitionResultAdapter = TypeAdapter(DefinitionResult)

DocumentHighlightResult = list[DocumentHighlight]
DocumentHighlightResultAdapter = TypeAdapter(DocumentHighlightResult)

DocumentSymbolResult = list[DocumentSymbol | list[SymbolInformation]]
DocumentSymbolResultAdapter = TypeAdapter(DocumentSymbolResult)

HoverResult = Hover | None
HoverResultAdapter = TypeAdapter(HoverResult)

ImplementationResult = Location | list[Location]
ImplementationResultAdapter = TypeAdapter(ImplementationResult)

ReferencesResult = list[Location]
ReferencesResultAdapter = TypeAdapter(ReferencesResult)

SignatureHelpResult = SignatureHelp | None
SignatureHelpResultAdapter = TypeAdapter(SignatureHelpResult)

TypeDefinitionResult = Location | list[Location]
TypeDefinitionResultAdapter = TypeAdapter(TypeDefinitionResult)

WorkspaceFoldersResult = list[WorkspaceFolder]
WorkspaceFoldersResultAdapter = TypeAdapter(WorkspaceFoldersResult)

WorkspaceSymbolResult = list[WorkspaceSymbol | list[SymbolInformation]]
WorkspaceSymbolResultAdapter = TypeAdapter(WorkspaceSymbolResult)
