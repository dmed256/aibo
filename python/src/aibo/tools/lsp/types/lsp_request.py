import json
from typing import Annotated, Any, Literal, Optional, Union

from pydantic import Field, TypeAdapter

from aibo.tools.lsp.types.base_lsp_types import BaseLspModel, JsonRpcMessage
from aibo.tools.lsp.types.client_capabilities import ClientCapabilities
from aibo.tools.lsp.types.common_types import (
    CompletionItemKind,
    CompletionItemTag,
    CompletionTriggerKind,
    InsertTextFormat,
    InsertTextMode,
    MarkupKind,
    ProgressToken,
    SignatureHelpTriggerKind,
    TraceValue,
)
from aibo.tools.lsp.types.location_types import Position, Range
from aibo.tools.lsp.types.text_document_types import (
    TextDocumentIdentifier,
    TextDocumentPositionParams,
)

__all__ = [
    "ClientInfo",
    "Command",
    "CompletionContext",
    "CompletionItem",
    "CompletionItem",
    "CompletionItemLabelDetails",
    "CompletionItemResolveParams",
    "CompletionItemResolveRequest",
    "CompletionParams",
    "CompletionRequest",
    "DeclarationParams",
    "DeclarationRequest",
    "DefinitionParams",
    "DefinitionRequest",
    "DocumentHighlightParams",
    "DocumentHighlightRequest",
    "DocumentSymbolParams",
    "DocumentSymbolRequest",
    "HoverParams",
    "HoverRequest",
    "ImplementationParams",
    "ImplementationRequest",
    "InitializationRequest",
    "InitializeParams",
    "InsertReplaceEdit",
    "JsonRpcRequestMessage",
    "LspRequest",
    "LspRequestAdapter",
    "MarkupContent",
    "ParameterInformation",
    "PartialResultParams",
    "ReferenceContext",
    "ReferencesParams",
    "ReferencesRequest",
    "SignatureHelp",
    "SignatureHelpContext",
    "SignatureHelpParams",
    "SignatureHelpRequest",
    "SignatureInformation",
    "TextEdit",
    "TypeDefinitionParams",
    "TypeDefinitionRequest",
    "WindowLogMessageRequest",
    "WorkDoneProgressParams",
    "WorkspaceDidChangeConfigurationRequest",
    "WorkspaceFolder",
    "WorkspaceFoldersRequest",
    "WorkspaceSymbolParams",
    "WorkspaceSymbolRequest",
]


class WorkspaceFolder(BaseLspModel):
    uri: str
    name: str


class WorkDoneProgressParams(BaseLspModel):
    work_done_token: Optional[ProgressToken] = None


class PartialResultParams(BaseLspModel):
    partial_result_token: Optional[ProgressToken] = None


class ReferenceContext(BaseLspModel):
    include_declaration: bool


class ClientInfo(BaseLspModel):
    name: str
    version: Optional[str] = None


class InitializeParams(WorkDoneProgressParams):
    capabilities: ClientCapabilities
    client_info: Optional[ClientInfo] = None
    initialization_options: Optional[Any] = None
    locale: Optional[str] = None
    process_id: Optional[int] = None
    root_path: Optional[str] = None
    root_uri: Optional[str] = None
    trace: Optional[TraceValue] = None
    workspace_folders: Optional[list[WorkspaceFolder]] = None


class DeclarationParams(
    TextDocumentPositionParams, WorkDoneProgressParams, PartialResultParams
):
    pass


class DefinitionParams(
    TextDocumentPositionParams, WorkDoneProgressParams, PartialResultParams
):
    pass


class HoverParams(TextDocumentPositionParams, WorkDoneProgressParams):
    pass


class TypeDefinitionParams(
    TextDocumentPositionParams, WorkDoneProgressParams, PartialResultParams
):
    pass


class ImplementationParams(
    TextDocumentPositionParams, WorkDoneProgressParams, PartialResultParams
):
    pass


class ReferencesParams(
    TextDocumentPositionParams, WorkDoneProgressParams, PartialResultParams
):
    context: ReferenceContext


class MarkupContent(BaseLspModel):
    kind: MarkupKind
    value: str


class ParameterInformation(BaseLspModel):
    label: str | tuple[int, int]
    documentation: Optional[str | MarkupContent]


class SignatureInformation(BaseLspModel):
    label: str
    documentation: Optional[str | MarkupContent] = None
    parameters: Optional[list[ParameterInformation]] = None
    active_parameter: Optional[int] = None


class SignatureHelp(BaseLspModel):
    signatures: list[SignatureInformation]
    active_signature: Optional[int] = None
    active_parameter: Optional[int] = None


class SignatureHelpContext(BaseLspModel):
    trigger_kind: SignatureHelpTriggerKind
    trigger_character: Optional[str] = None
    is_retrigger: bool = False
    active_signature_help: Optional[SignatureHelp] = None


class SignatureHelpParams(TextDocumentPositionParams, WorkDoneProgressParams):
    context: SignatureHelpContext


class CompletionContext(BaseLspModel):
    trigger_kind: CompletionTriggerKind
    trigger_character: Optional[str] = None


class CompletionParams(
    TextDocumentPositionParams, WorkDoneProgressParams, PartialResultParams
):
    context: CompletionContext


class TextEdit(BaseLspModel):
    range: Range
    new_text: str


class InsertReplaceEdit(BaseLspModel):
    new_text: str
    insert: Range
    replace: Range


class Command(BaseLspModel):
    title: str
    command: str
    arguments: Optional[list[Any]] = None


class CompletionItemLabelDetails(BaseLspModel):
    detail: Optional[str] = None
    description: Optional[str] = None


class CompletionItem(BaseLspModel):
    label: str
    label_details: Optional[CompletionItemLabelDetails] = None
    kind: Optional[CompletionItemKind] = None
    tags: Optional[list[CompletionItemTag]] = None
    detail: Optional[str] = None
    documentation: Optional[str | MarkupContent] = None
    deprecated: Optional[bool] = None
    preselect: Optional[bool] = None
    sort_text: Optional[str] = None
    filter_text: Optional[str] = None
    insert_text: Optional[str] = None
    insert_text_format: Optional[InsertTextFormat] = None
    insert_text_mode: Optional[InsertTextMode] = None
    text_edit: Optional[TextEdit | InsertReplaceEdit] = None
    text_edit_text: Optional[str] = None
    additional_text_edits: Optional[list[TextEdit]] = None
    commit_characters: Optional[list[str]] = None
    command: Optional[Command] = None
    data: Optional[Any] = None


CompletionItemResolveParams = CompletionItem


class DocumentHighlightParams(
    TextDocumentPositionParams, WorkDoneProgressParams, PartialResultParams
):
    pass


class DocumentSymbolParams(WorkDoneProgressParams, PartialResultParams):
    text_document: TextDocumentIdentifier


class WorkspaceSymbolParams(WorkDoneProgressParams, PartialResultParams):
    query: str


class JsonRpcRequestMessage(JsonRpcMessage):
    method: str
    params: Optional[Any] = None


class InitializationRequest(JsonRpcRequestMessage):
    method: Literal["initialize"] = "initialize"
    params: InitializeParams


class DeclarationRequest(JsonRpcRequestMessage):
    method: Literal["textDocument/declaration"] = "textDocument/declaration"
    params: DeclarationParams


class DefinitionRequest(JsonRpcRequestMessage):
    method: Literal["textDocument/definition"] = "textDocument/definition"
    params: DefinitionParams


class HoverRequest(JsonRpcRequestMessage):
    method: Literal["textDocument/hover"] = "textDocument/hover"
    params: HoverParams


class TypeDefinitionRequest(JsonRpcRequestMessage):
    method: Literal["textDocument/typeDefinition"] = "textDocument/typeDefinition"
    params: TypeDefinitionParams


class ImplementationRequest(JsonRpcRequestMessage):
    method: Literal["textDocument/implementation"] = "textDocument/implementation"
    params: ImplementationParams


class ReferencesRequest(JsonRpcRequestMessage):
    method: Literal["textDocument/references"] = "textDocument/references"
    params: ReferencesParams


class SignatureHelpRequest(JsonRpcRequestMessage):
    method: Literal["textDocument/signatureHelp"] = "textDocument/signatureHelp"
    params: SignatureHelpParams


class CompletionRequest(JsonRpcRequestMessage):
    method: Literal["textDocument/completion"] = "textDocument/completion"
    params: CompletionParams


class CompletionItemResolveRequest(JsonRpcRequestMessage):
    method: Literal["completionItem/resolve"] = "completionItem/resolve"
    params: CompletionItemResolveParams


class DocumentHighlightRequest(JsonRpcRequestMessage):
    method: Literal["textDocument/documentHighlight"] = "textDocument/documentHighlight"
    params: DocumentHighlightParams


class DocumentSymbolRequest(JsonRpcRequestMessage):
    method: Literal["textDocument/documentSymbol"] = "textDocument/documentSymbol"
    params: DocumentSymbolParams


class WorkspaceSymbolRequest(JsonRpcRequestMessage):
    method: Literal["workspace/symbol"] = "workspace/symbol"
    params: WorkspaceSymbolParams


class WorkspaceFoldersRequest(JsonRpcRequestMessage):
    method: Literal["workspace/workspaceFolders"] = "workspace/workspaceFolders"


class WorkspaceDidChangeConfigurationRequest(JsonRpcRequestMessage):
    method: Literal[
        "workspace/didChangeConfiguration"
    ] = "workspace/didChangeConfiguration"
    settings: Any = {}


class WindowLogMessageRequest(JsonRpcRequestMessage):
    method: Literal["window/logMessage"] = "window/logMessage"


LspRequest = Annotated[
    Union[
        CompletionRequest,
        CompletionItemResolveRequest,
        DeclarationRequest,
        DefinitionRequest,
        DocumentHighlightRequest,
        DocumentSymbolRequest,
        HoverRequest,
        ImplementationRequest,
        InitializationRequest,
        ReferencesRequest,
        SignatureHelpRequest,
        TypeDefinitionRequest,
        WindowLogMessageRequest,
        WorkspaceDidChangeConfigurationRequest,
        WorkspaceFoldersRequest,
        WorkspaceSymbolRequest,
    ],
    Field(discriminator="method"),
]
LspRequestAdapter = TypeAdapter(LspRequest)
