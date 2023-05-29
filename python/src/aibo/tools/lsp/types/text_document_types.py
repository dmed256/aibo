from typing import Any, Optional, Self

from pydantic import Field

from aibo.tools.lsp.types.base_lsp_types import BaseLspModel
from aibo.tools.lsp.types.common_types import SymbolKind, SymbolTag
from aibo.tools.lsp.types.location_types import Location, Position

__all__ = [
    "DocumentSymbol",
    "SymbolInformation",
    "TextDocumentIdentifier",
    "TextDocumentPositionParams",
    "WorkspaceSymbol",
]


class DocumentSymbol(BaseLspModel):
    name: str
    location: Location
    kind: SymbolKind

    def content(self) -> str:
        return self.location.content()

    def __contains__(self, other: Self) -> bool:
        return self.location in other.location


class TextDocumentIdentifier(BaseLspModel):
    uri: str


class TextDocumentPositionParams(BaseLspModel):
    text_document: TextDocumentIdentifier
    position: Position


# Supposed to be deprecated
class SymbolInformation(BaseLspModel):
    name: str
    kind: SymbolKind
    tags: Optional[list[SymbolTag]] = None
    deprecated: Optional[bool] = None
    location: Location
    container_name: Optional[str] = None


# Supposed to be deprecated
class WorkspaceSymbol(BaseLspModel):
    name: str
    kind: SymbolKind
    tags: Optional[list[SymbolTag]] = None
    location: Location
    container_name: Optional[str] = None
    data: Optional[Any] = None
