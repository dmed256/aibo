from typing import Any, Optional

from pydantic import BaseModel

from aibo.tools.lsp.types.base_lsp_types import BaseLspModel
from aibo.tools.lsp.types.common_types import (
    CodeActionKind,
    EmptyDict,
    FileOperationPatternKind,
    PositionEncodingKind,
    TextDocumentSyncKind,
)

__all__ = [
    "CallHierarchyOptions",
    "CallHierarchyRegistrationOptions",
    "CodeActionOptions",
    "CodeLensOptions",
    "CompletionOptions",
    "DeclarationOptions",
    "DeclarationRegistrationOptions",
    "DefinitionOptions",
    "DiagnosticOptions",
    "DiagnosticRegistrationOptions",
    "DocumentColorOptions",
    "DocumentColorRegistrationOptions",
    "DocumentFormattingOptions",
    "DocumentHighlightOptions",
    "DocumentLinkOptions",
    "DocumentOnTypeFormattingOptions",
    "DocumentRangeFormattingOptions",
    "DocumentSelector",
    "DocumentSymbolOptions",
    "ExecuteCommandOptions",
    "FileOperationFilter",
    "FileOperationPattern",
    "FileOperationPatternOptions",
    "FileOperationRegistrationOptions",
    "FoldingRangeOptions",
    "FoldingRangeRegistrationOptions",
    "HoverOptions",
    "ImplementationOptions",
    "ImplementationRegistrationOptions",
    "InlayHintOptions",
    "InlayHintRegistrationOptions",
    "InlineValueOptions",
    "InlineValueRegistrationOptions",
    "LinkedEditingRangeOptions",
    "LinkedEditingRangeRegistrationOptions",
    "MonikerOptions",
    "MonikerRegistrationOptions",
    "NotebookDocumentFilter",
    "NotebookDocumentSyncOptions",
    "NotebookDocumentSyncRegistrationOptions",
    "NotebookSelector",
    "NotebookSelectorCell",
    "ReferenceOptions",
    "RenameOptions",
    "SelectionRangeOptions",
    "SelectionRangeRegistrationOptions",
    "SemanticTokenFull",
    "SemanticTokensLegend",
    "SemanticTokensOptions",
    "SemanticTokensRegistrationOptions",
    "ServerCapabilities",
    "ServerCompletionItem",
    "ServerWorkspaceCapabilities",
    "ServerWorkspaceFileOperationsCapabilities",
    "SignatureHelpOptions",
    "StaticRegistrationOptions",
    "TextDocumentRegistrationOptions",
    "TextDocumentSyncOptions",
    "TypeDefinitionOptions",
    "TypeDefinitionRegistrationOptions",
    "TypeHierarchyOptions",
    "TypeHierarchyRegistrationOptions",
    "WorkDoneProgressOptions",
    "WorkspaceFoldersServerCapabilities",
    "WorkspaceSymbolOptions",
]


class DocumentFilter(BaseLspModel):
    language: Optional[str] = None
    scheme: Optional[str] = None
    pattern: Optional[str] = None


DocumentSelector = list[DocumentFilter]


class TextDocumentRegistrationOptions(BaseLspModel):
    document_selector: DocumentSelector | None


class WorkDoneProgressOptions(BaseLspModel):
    work_done_progress: Optional[bool] = None


class StaticRegistrationOptions(BaseLspModel):
    id: Optional[str] = None


class CallHierarchyOptions(WorkDoneProgressOptions):
    pass


class CallHierarchyRegistrationOptions(
    TextDocumentRegistrationOptions,
    CallHierarchyOptions,
    StaticRegistrationOptions,
):
    pass


class CodeActionOptions(WorkDoneProgressOptions):
    code_action_kinds: Optional[list[CodeActionKind]] = None
    resolve_provider: Optional[bool] = None


class CodeLensOptions(WorkDoneProgressOptions):
    resolve_provider: Optional[bool] = None


class ServerCompletionItem(BaseLspModel):
    label_details_support: Optional[bool] = None


class CompletionOptions(WorkDoneProgressOptions):
    trigger_characters: Optional[list[str]] = None
    all_commit_characters: Optional[list[str]] = None
    resolve_provider: Optional[bool] = None


class DeclarationOptions(WorkDoneProgressOptions):
    pass


class DeclarationRegistrationOptions(
    DeclarationOptions,
    TextDocumentRegistrationOptions,
    StaticRegistrationOptions,
):
    pass


class DefinitionOptions(WorkDoneProgressOptions):
    pass


class DiagnosticOptions(WorkDoneProgressOptions):
    identifier: Optional[str] = None
    inter_file_dependencies: bool
    workspace_diagnostics: bool


class DiagnosticRegistrationOptions(
    TextDocumentRegistrationOptions, DiagnosticOptions, StaticRegistrationOptions
):
    pass


class DocumentColorOptions(WorkDoneProgressOptions):
    pass


class DocumentColorRegistrationOptions(
    TextDocumentRegistrationOptions,
    StaticRegistrationOptions,
    DocumentColorOptions,
):
    pass


class DocumentFormattingOptions(WorkDoneProgressOptions):
    pass


class DocumentHighlightOptions(WorkDoneProgressOptions):
    pass


class DocumentLinkOptions(WorkDoneProgressOptions):
    resolve_provider: Optional[bool] = None


class DocumentOnTypeFormattingOptions(BaseLspModel):
    first_trigger_character: str
    more_trigger_character: Optional[list[str]] = None


class DocumentRangeFormattingOptions(WorkDoneProgressOptions):
    pass


class DocumentSymbolOptions(WorkDoneProgressOptions):
    label: Optional[str] = None


class ExecuteCommandOptions(WorkDoneProgressOptions):
    commands: list[str]


class FileOperationPatternOptions(BaseLspModel):
    ignore_case: Optional[bool] = None


class FileOperationPattern(BaseLspModel):
    glob: str
    matches: Optional[FileOperationPatternKind] = None
    options: Optional[FileOperationPatternOptions] = None


class FileOperationFilter(BaseLspModel):
    scheme: Optional[str] = None
    pattern: FileOperationPattern


class FileOperationRegistrationOptions(BaseLspModel):
    filters: list[FileOperationFilter]


class FoldingRangeOptions(WorkDoneProgressOptions):
    pass


class FoldingRangeRegistrationOptions(
    TextDocumentRegistrationOptions,
    FoldingRangeOptions,
    StaticRegistrationOptions,
):
    pass


class HoverOptions(WorkDoneProgressOptions):
    pass


class ImplementationOptions(WorkDoneProgressOptions):
    pass


class ImplementationRegistrationOptions(
    TextDocumentRegistrationOptions,
    ImplementationOptions,
    StaticRegistrationOptions,
):
    pass


class InlayHintOptions(WorkDoneProgressOptions):
    resolve_provider: Optional[bool] = None


class InlayHintRegistrationOptions(
    InlayHintOptions,
    TextDocumentRegistrationOptions,
    StaticRegistrationOptions,
):
    pass


class InlineValueOptions(WorkDoneProgressOptions):
    pass


class InlineValueRegistrationOptions(
    InlineValueOptions,
    TextDocumentRegistrationOptions,
    StaticRegistrationOptions,
):
    pass


class LinkedEditingRangeOptions(WorkDoneProgressOptions):
    pass


class LinkedEditingRangeRegistrationOptions(
    TextDocumentRegistrationOptions,
    LinkedEditingRangeOptions,
    StaticRegistrationOptions,
):
    pass


class MonikerOptions(WorkDoneProgressOptions):
    pass


class MonikerRegistrationOptions(
    TextDocumentRegistrationOptions,
    MonikerOptions,
):
    pass


class NotebookSelectorCell(BaseLspModel):
    language: str


class NotebookDocumentFilter(BaseLspModel):
    notebook_type: Optional[str] = None
    scheme: Optional[str] = None
    pattern: Optional[str] = None


class NotebookSelector(BaseLspModel):
    notebook: Optional[str | NotebookDocumentFilter] = None
    cells: Optional[list[NotebookSelectorCell]] = None


class NotebookDocumentSyncOptions(BaseLspModel):
    notebook_selector: NotebookSelector
    save: Optional[bool] = None


class NotebookDocumentSyncRegistrationOptions(
    NotebookDocumentSyncOptions, StaticRegistrationOptions
):
    pass


class ReferenceOptions(WorkDoneProgressOptions):
    pass


class RenameOptions(WorkDoneProgressOptions):
    prepare_provider: Optional[bool] = None


class SelectionRangeOptions(WorkDoneProgressOptions):
    pass


class SelectionRangeRegistrationOptions(
    SelectionRangeOptions,
    TextDocumentRegistrationOptions,
    StaticRegistrationOptions,
):
    pass


class SemanticTokenFull(BaseLspModel):
    delta: Optional[bool] = None


class SemanticTokensLegend(BaseLspModel):
    token_types: list[str]
    token_modifiers: list[str]


class SemanticTokensOptions(WorkDoneProgressOptions):
    legend: SemanticTokensLegend
    range: Optional[bool | EmptyDict] = None
    full: Optional[bool | SemanticTokenFull] = None


class SemanticTokensRegistrationOptions(
    TextDocumentRegistrationOptions,
    SemanticTokensOptions,
    StaticRegistrationOptions,
):
    pass


class SignatureHelpOptions(WorkDoneProgressOptions):
    trigger_characters: Optional[list[str]] = None
    retrigger_characters: Optional[list[str]] = None


class TextDocumentSyncOptions(BaseLspModel):
    open_close: Optional[bool] = None
    change: Optional[TextDocumentSyncKind] = None


class TypeDefinitionOptions(WorkDoneProgressOptions):
    pass


class TypeDefinitionRegistrationOptions(
    TextDocumentRegistrationOptions,
    TypeDefinitionOptions,
    StaticRegistrationOptions,
):
    pass


class TypeHierarchyOptions(WorkDoneProgressOptions):
    pass


class TypeHierarchyRegistrationOptions(
    TextDocumentRegistrationOptions,
    TypeHierarchyOptions,
    StaticRegistrationOptions,
):
    pass


class WorkspaceFoldersServerCapabilities(BaseLspModel):
    supported: Optional[bool] = None
    change_notifications: Optional[str | bool] = None


class WorkspaceSymbolOptions(WorkDoneProgressOptions):
    resolve_provider: Optional[bool] = None


class ServerWorkspaceFileOperationsCapabilities(BaseLspModel):
    did_create: Optional[FileOperationRegistrationOptions] = None
    will_create: Optional[FileOperationRegistrationOptions] = None
    did_rename: Optional[FileOperationRegistrationOptions] = None
    will_rename: Optional[FileOperationRegistrationOptions] = None
    did_delete: Optional[FileOperationRegistrationOptions] = None
    will_delete: Optional[FileOperationRegistrationOptions] = None


class ServerWorkspaceCapabilities(BaseLspModel):
    workspace_folders: Optional[WorkspaceFoldersServerCapabilities] = None
    file_operations: Optional[ServerWorkspaceFileOperationsCapabilities] = None


class ServerCapabilities(BaseLspModel):
    position_encoding: Optional[PositionEncodingKind] = None
    text_document_sync: Optional[TextDocumentSyncOptions | TextDocumentSyncKind] = None
    notebook_document_sync: Optional[
        NotebookDocumentSyncOptions | NotebookDocumentSyncRegistrationOptions
    ] = None
    completion_provider: Optional[CompletionOptions] = None
    hover_provider: Optional[bool | HoverOptions] = None
    signature_help_provider: Optional[SignatureHelpOptions] = None
    declaration_provider: Optional[
        bool | DeclarationOptions | DeclarationRegistrationOptions
    ] = None
    definition_provider: Optional[bool | DefinitionOptions] = None
    type_definition_provider: Optional[
        bool | TypeDefinitionOptions | TypeDefinitionRegistrationOptions
    ] = None
    implementation_provider: Optional[
        bool | ImplementationOptions | ImplementationRegistrationOptions
    ] = None
    references_provider: Optional[bool | ReferenceOptions] = None
    document_highlight_provider: Optional[bool | DocumentHighlightOptions] = None
    document_symbol_provider: Optional[bool | DocumentSymbolOptions] = None
    code_action_provider: Optional[bool | CodeActionOptions] = None
    code_lens_provider: Optional[CodeLensOptions] = None
    document_link_provider: Optional[DocumentLinkOptions] = None
    color_provider: Optional[
        bool | DocumentColorOptions | DocumentColorRegistrationOptions
    ] = None
    document_formatting_provider: Optional[bool | DocumentFormattingOptions] = None
    document_range_formatting_provider: Optional[
        bool | DocumentRangeFormattingOptions
    ] = None
    document_on_type_formatting_provider: Optional[
        DocumentOnTypeFormattingOptions
    ] = None
    rename_provider: Optional[bool | RenameOptions] = None
    folding_range_provider: Optional[
        bool | FoldingRangeOptions | FoldingRangeRegistrationOptions
    ] = None
    execute_command_provider: Optional[ExecuteCommandOptions] = None
    selection_range_provider: Optional[
        bool | SelectionRangeOptions | SelectionRangeRegistrationOptions
    ] = None
    linked_editing_range_provider: Optional[
        bool | LinkedEditingRangeOptions | LinkedEditingRangeRegistrationOptions
    ] = None
    call_hierarchy_provider: Optional[
        bool | CallHierarchyOptions | CallHierarchyRegistrationOptions
    ] = None
    semantic_tokens_provider: Optional[
        SemanticTokensOptions | SemanticTokensRegistrationOptions
    ] = None
    moniker_provider: Optional[
        bool | MonikerOptions | MonikerRegistrationOptions
    ] = None
    type_hierarchy_provider: Optional[
        bool | TypeHierarchyOptions | TypeHierarchyRegistrationOptions
    ] = None
    inline_value_provider: Optional[
        bool | InlineValueOptions | InlineValueRegistrationOptions
    ] = None
    inlay_hint_provider: Optional[
        bool | InlayHintOptions | InlayHintRegistrationOptions
    ] = None
    diagnostic_provider: Optional[
        DiagnosticOptions | DiagnosticRegistrationOptions
    ] = None
    workspace_symbol_provider: Optional[bool | WorkspaceSymbolOptions] = None
    workspace: Optional[ServerWorkspaceCapabilities] = None
    experimental: Optional[Any] = None
