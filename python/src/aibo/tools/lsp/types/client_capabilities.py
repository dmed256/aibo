from typing import Any, Optional, TypeVar

from aibo.tools.lsp.types.base_lsp_types import BaseLspModel
from aibo.tools.lsp.types.common_types import (
    CodeActionKind,
    CompletionItemKind,
    CompletionItemTag,
    DiagnosticTag,
    EmptyDict,
    FailureHandlingKind,
    FoldingRangeKind,
    InsertTextMode,
    MarkupKind,
    PositionEncodingKind,
    PrepareSupportDefaultBehavior,
    ResourceOperationKind,
    SymbolKind,
    SymbolTag,
    TokenFormat,
)

__all__ = [
    "CallHierarchyClientCapabilities",
    "ChangeAnnotationSupport",
    "ClientCapabilities",
    "CodeActionClientCapabilities",
    "CodeActionLiteralSupport",
    "CodeLensClientCapabilities",
    "CodeLensWorkspaceClientCapabilities",
    "CompletionClientCapabilities",
    "CompletionClientCompletionItem",
    "CompletionClientCompletionList",
    "DeclarationClientCapabilities",
    "DefinitionClientCapabilities",
    "DiagnosticClientCapabilities",
    "DiagnosticWorkspaceClientCapabilities",
    "DidChangeConfigurationClientCapabilities",
    "DidChangeWatchedFilesClientCapabilities",
    "DocumentColorClientCapabilities",
    "DocumentFormattingClientCapabilities",
    "DocumentHighlightClientCapabilities",
    "DocumentLinkClientCapabilities",
    "DocumentOnTypeFormattingClientCapabilities",
    "DocumentRangeFormattingClientCapabilities",
    "DocumentSymbolClientCapabilities",
    "ExecuteCommandClientCapabilities",
    "FoldingRange",
    "FoldingRangeClientCapabilities",
    "GeneralCapabilities",
    "GeneralStaleRequestSupport",
    "HoverClientCapabilities",
    "ImplementationClientCapabilities",
    "InlayHintClientCapabilities",
    "InlayHintClientResolveSupport",
    "InlayHintWorkspaceClientCapabilities",
    "InlineValueClientCapabilities",
    "InlineValueWorkspaceClientCapabilities",
    "IntValueSet",
    "LinkedEditingRangeClientCapabilities",
    "MarkdownClientCapabilities",
    "MonikerClientCapabilities",
    "NotebookDocumentClientCapabilities",
    "NotebookDocumentSyncClientCapabilities",
    "Properties",
    "PublishDiagnosticsClientCapabilities",
    "ReferenceClientCapabilities",
    "RegularExpressionsClientCapabilities",
    "RenameClientCapabilities",
    "SelectionRangeClientCapabilities",
    "SemanticTokenRequest",
    "SemanticTokenRequestFull",
    "SemanticTokensClientCapabilities",
    "SemanticTokensWorkspaceClientCapabilities",
    "ShowDocumentClientCapabilities",
    "ShowMessageRequestClientCapabilities",
    "ShowMessageRequestMessageActionItem",
    "SignatureHelpClientCapabilities",
    "SignatureHelpParameterInformation",
    "SignatureHelpClientSignatureInformation",
    "StrValueSet",
    "TextDocumentClientCapabilities",
    "TextDocumentSyncClientCapabilities",
    "TypeDefinitionClientCapabilities",
    "TypeHierarchyClientCapabilities",
    "WindowCapabilities",
    "WorkspaceCapabilities",
    "WorkspaceEditClientCapabilities",
    "WorkspaceFileOperations",
    "WorkspaceSymbolClientCapabilities",
]

T = TypeVar("T")


class IntValueSet(BaseLspModel):
    value_set: list[int]


class StrValueSet(BaseLspModel):
    value_set: list[str]


class Properties(BaseLspModel):
    properties: list[str]


class CallHierarchyClientCapabilities(BaseLspModel):
    dynamic_registration: Optional[bool] = None


class CodeActionLiteralSupport(BaseLspModel):
    code_action_kind: StrValueSet


class CodeActionClientCapabilities(BaseLspModel):
    dynamic_registration: Optional[bool] = None
    code_action_literal_support: Optional[CodeActionLiteralSupport] = None
    is_preferred_support: Optional[bool] = None
    disabled_support: Optional[bool] = None
    data_support: Optional[bool] = None
    resolve_support: Optional[Properties] = None
    honors_change_annotations: Optional[bool] = None


class CodeLensClientCapabilities(BaseLspModel):
    dynamic_registration: Optional[bool] = None


class CodeLensWorkspaceClientCapabilities(BaseLspModel):
    refresh_support: Optional[bool] = None


class CompletionClientCompletionItem(BaseLspModel):
    snippet_support: Optional[bool] = None
    commit_characters_support: Optional[bool] = None
    documentation_format: Optional[list[MarkupKind]] = None
    deprecated_support: Optional[bool] = None
    preselect_support: Optional[bool] = None
    tag_support: Optional[IntValueSet] = None
    insert_replace_support: Optional[bool] = None
    resolve_support: Optional[Properties] = None
    insert_text_mode_support: Optional[IntValueSet] = None
    label_details_support: Optional[bool] = None


class CompletionClientCompletionList(BaseLspModel):
    item_defaults: Optional[list[str]] = None


class CompletionClientCapabilities(BaseLspModel):
    dynamic_registration: Optional[bool] = None
    completion_item: Optional[CompletionClientCompletionItem] = None
    completion_item_kind: Optional[IntValueSet] = None
    context_support: Optional[bool] = None
    insert_text_mode: Optional[InsertTextMode] = None
    completion_list: Optional[CompletionClientCompletionList] = None


class DeclarationClientCapabilities(BaseLspModel):
    dynamic_registration: Optional[bool] = None
    link_support: Optional[bool] = None


class DefinitionClientCapabilities(BaseLspModel):
    dynamic_registration: Optional[bool] = None
    link_support: Optional[bool] = None


class DiagnosticClientCapabilities(BaseLspModel):
    dynamic_registration: Optional[bool] = None
    related_document_support: Optional[bool] = None


class DiagnosticWorkspaceClientCapabilities(BaseLspModel):
    refresh_support: Optional[bool] = None


class DidChangeConfigurationClientCapabilities(BaseLspModel):
    dynamic_registration: Optional[bool] = None


class DidChangeWatchedFilesClientCapabilities(BaseLspModel):
    dynamic_registration: Optional[bool] = None
    relative_pattern_support: Optional[bool] = None


class DocumentColorClientCapabilities(BaseLspModel):
    dynamic_registration: Optional[bool] = None


class DocumentFormattingClientCapabilities(BaseLspModel):
    dynamic_registration: Optional[bool] = None


class DocumentHighlightClientCapabilities(BaseLspModel):
    dynamic_registration: Optional[bool] = None


class DocumentLinkClientCapabilities(BaseLspModel):
    dynamic_registration: Optional[bool] = None
    tooltip_support: Optional[bool] = None


class DocumentOnTypeFormattingClientCapabilities(BaseLspModel):
    dynamic_registration: Optional[bool] = None


class DocumentRangeFormattingClientCapabilities(BaseLspModel):
    dynamic_registration: Optional[bool] = None


class DocumentSymbolClientCapabilities(BaseLspModel):
    dynamic_registration: Optional[bool] = None
    symbol_kind: Optional[IntValueSet] = None
    hierarchical_document_symbol_support: Optional[bool] = None
    tag_support: Optional[IntValueSet] = None
    label_support: Optional[bool] = None


class ExecuteCommandClientCapabilities(BaseLspModel):
    dynamic_registration: Optional[bool] = None


class FoldingRange(BaseLspModel):
    collapsed_text: Optional[bool] = None


class FoldingRangeClientCapabilities(BaseLspModel):
    dynamic_registration: Optional[bool] = None
    range_limit: Optional[int] = None
    line_folding_only: Optional[bool] = None
    folding_range_kind: Optional[IntValueSet] = None
    folding_range: Optional[FoldingRange] = None


class HoverClientCapabilities(BaseLspModel):
    dynamic_registration: Optional[bool] = None
    content_format: Optional[list[MarkupKind]] = None


class ImplementationClientCapabilities(BaseLspModel):
    dynamic_registration: Optional[bool] = None
    link_support: Optional[bool] = None


class InlayHintClientResolveSupport(BaseLspModel):
    properties: Properties


class InlayHintClientCapabilities(BaseLspModel):
    dynamic_registration: Optional[bool] = None
    resolve_support: InlayHintClientResolveSupport


class InlayHintWorkspaceClientCapabilities(BaseLspModel):
    refresh_support: Optional[bool] = None


class InlineValueClientCapabilities(BaseLspModel):
    dynamic_registration: Optional[bool] = None


class InlineValueWorkspaceClientCapabilities(BaseLspModel):
    refresh_support: Optional[bool] = None


class LinkedEditingRangeClientCapabilities(BaseLspModel):
    dynamic_registration: Optional[bool] = None


class MarkdownClientCapabilities(BaseLspModel):
    parser: str
    version: Optional[str] = None
    allowed_tags: Optional[list[str]] = None


class MonikerClientCapabilities(BaseLspModel):
    dynamic_registration: Optional[bool] = None


class PublishDiagnosticsClientCapabilities(BaseLspModel):
    related_information: Optional[bool] = None
    tag_support: Optional[IntValueSet] = None
    versionSupport: Optional[bool] = None
    code_description_support: Optional[bool] = None
    dataSupport: Optional[bool] = None


class ReferenceClientCapabilities(BaseLspModel):
    dynamic_registration: Optional[bool] = None


class RegularExpressionsClientCapabilities(BaseLspModel):
    engine: str
    version: Optional[str] = None


class RenameClientCapabilities(BaseLspModel):
    dynamic_registration: Optional[bool] = None
    prepare_support: Optional[bool] = None
    prepare_support_default_behavior: Optional[PrepareSupportDefaultBehavior] = None
    honors_change_annotations: Optional[bool] = None


class SelectionRangeClientCapabilities(BaseLspModel):
    dynamic_registration: Optional[bool] = None


class SemanticTokenRequestFull(BaseLspModel):
    delta: Optional[bool] = None


class SemanticTokenRequest(BaseLspModel):
    range: Optional[bool | EmptyDict] = None
    full: Optional[bool | SemanticTokenRequestFull] = None


class SemanticTokensClientCapabilities(BaseLspModel):
    dynamic_registration: Optional[bool] = None
    requests: SemanticTokenRequest
    token_types: list[str]
    token_modifiers: list[str]
    formats: list[TokenFormat]
    overlapping_token_support: Optional[bool] = None
    multiline_token_support: Optional[bool] = None
    server_cancel_support: Optional[bool] = None
    augments_syntax_tokens: Optional[bool] = None


class SemanticTokensWorkspaceClientCapabilities(BaseLspModel):
    refresh_support: Optional[bool] = None


class ShowDocumentClientCapabilities(BaseLspModel):
    support: bool


class ShowMessageRequestMessageActionItem(BaseLspModel):
    additional_properties_support: Optional[bool] = None


class ShowMessageRequestClientCapabilities(BaseLspModel):
    message_action_item: Optional[ShowMessageRequestMessageActionItem] = None


class SignatureHelpParameterInformation(BaseLspModel):
    label_offset_support: Optional[bool] = None


class SignatureHelpClientSignatureInformation(BaseLspModel):
    documentation_format: Optional[list[MarkupKind]] = None
    parameter_information: Optional[SignatureHelpParameterInformation] = None
    active_parameter_support: Optional[bool] = None


class SignatureHelpClientCapabilities(BaseLspModel):
    dynamic_registration: Optional[bool] = None
    signature_information: Optional[SignatureHelpClientSignatureInformation] = None
    context_support: Optional[bool] = None


class TextDocumentSyncClientCapabilities(BaseLspModel):
    dynamic_registration: Optional[bool] = None
    will_save: Optional[bool] = None
    will_save_wait_until: Optional[bool] = None
    did_save: Optional[bool] = None


class TypeDefinitionClientCapabilities(BaseLspModel):
    dynamic_registration: Optional[bool] = None
    link_support: Optional[bool] = None


class TypeHierarchyClientCapabilities(BaseLspModel):
    dynamic_registration: Optional[bool] = None


class ChangeAnnotationSupport(BaseLspModel):
    groups_on_label: Optional[bool] = None


class WorkspaceEditClientCapabilities(BaseLspModel):
    document_changes: Optional[bool] = None
    resource_operations: Optional[list[ResourceOperationKind]] = None
    failure_handling: Optional[FailureHandlingKind] = None
    normalizes_line_endings: Optional[bool] = None
    change_annotation_support: Optional[ChangeAnnotationSupport] = None


class WorkspaceSymbolClientCapabilities(BaseLspModel):
    dynamic_registration: Optional[bool] = None
    symbol_kind: Optional[IntValueSet] = None
    tag_support: Optional[IntValueSet] = None
    resolve_support: Optional[Properties] = None


class WorkspaceFileOperations(BaseLspModel):
    dynamic_registration: Optional[bool] = None
    did_create: Optional[bool] = None
    will_create: Optional[bool] = None
    did_rename: Optional[bool] = None
    will_rename: Optional[bool] = None
    did_delete: Optional[bool] = None
    will_delete: Optional[bool] = None


class TextDocumentClientCapabilities(BaseLspModel):
    synchronization: Optional[TextDocumentSyncClientCapabilities] = None
    completion: Optional[CompletionClientCapabilities] = None
    hover: Optional[HoverClientCapabilities] = None
    signature_help: Optional[SignatureHelpClientCapabilities] = None
    declaration: Optional[DeclarationClientCapabilities] = None
    definition: Optional[DefinitionClientCapabilities] = None
    type_definition: Optional[TypeDefinitionClientCapabilities] = None
    implementation: Optional[ImplementationClientCapabilities] = None
    references: Optional[ReferenceClientCapabilities] = None
    document_highlight: Optional[DocumentHighlightClientCapabilities] = None
    document_symbol: Optional[DocumentSymbolClientCapabilities] = None
    code_action: Optional[CodeActionClientCapabilities] = None
    code_lens: Optional[CodeLensClientCapabilities] = None
    document_link: Optional[DocumentLinkClientCapabilities] = None
    color_provider: Optional[DocumentColorClientCapabilities] = None
    formatting: Optional[DocumentFormattingClientCapabilities] = None
    range_formatting: Optional[DocumentRangeFormattingClientCapabilities] = None
    on_type_formatting: Optional[DocumentOnTypeFormattingClientCapabilities] = None
    rename: Optional[RenameClientCapabilities] = None
    publish_diagnostics: Optional[PublishDiagnosticsClientCapabilities] = None
    folding_range: Optional[FoldingRangeClientCapabilities] = None
    selection_range: Optional[SelectionRangeClientCapabilities] = None
    linked_editing_range: Optional[LinkedEditingRangeClientCapabilities] = None
    call_hierarchy: Optional[CallHierarchyClientCapabilities] = None
    semantic_tokens: Optional[SemanticTokensClientCapabilities] = None
    moniker: Optional[MonikerClientCapabilities] = None
    type_hierarchy: Optional[TypeHierarchyClientCapabilities] = None
    inline_value: Optional[InlineValueClientCapabilities] = None
    inlay_hint: Optional[InlayHintClientCapabilities] = None
    diagnostic: Optional[DiagnosticClientCapabilities] = None


class WorkspaceCapabilities(BaseLspModel):
    apply_edit: Optional[bool] = None
    workspace_edit: Optional[WorkspaceEditClientCapabilities] = None
    did_change_configuration: Optional[DidChangeConfigurationClientCapabilities] = None
    did_change_watched_files: Optional[DidChangeWatchedFilesClientCapabilities] = None
    symbol: Optional[WorkspaceSymbolClientCapabilities] = None
    execute_command: Optional[ExecuteCommandClientCapabilities] = None
    workspace_folders: Optional[bool] = None
    configuration: Optional[bool] = None
    semantic_tokens: Optional[SemanticTokensWorkspaceClientCapabilities] = None
    code_lens: Optional[CodeLensWorkspaceClientCapabilities] = None
    file_operations: Optional[WorkspaceFileOperations] = None
    inline_value: Optional[InlineValueWorkspaceClientCapabilities] = None
    inlay_hint: Optional[InlayHintWorkspaceClientCapabilities] = None
    diagnostics: Optional[DiagnosticWorkspaceClientCapabilities] = None


class NotebookDocumentSyncClientCapabilities(BaseLspModel):
    dynamic_registration: Optional[bool] = None
    execution_summary_support: Optional[bool] = None


class NotebookDocumentClientCapabilities(BaseLspModel):
    synchronization: NotebookDocumentSyncClientCapabilities


class WindowCapabilities(BaseLspModel):
    work_done_progress: Optional[bool] = None
    show_message: Optional[ShowMessageRequestClientCapabilities] = None
    show_document: Optional[ShowDocumentClientCapabilities] = None


class GeneralStaleRequestSupport(BaseLspModel):
    cancel: bool
    retry_on_content_modified: list[str]


class GeneralCapabilities(BaseLspModel):
    stale_request_support: Optional[GeneralStaleRequestSupport] = None
    regular_expressions: Optional[RegularExpressionsClientCapabilities] = None
    markdown: Optional[MarkdownClientCapabilities] = None
    position_encodings: Optional[list[PositionEncodingKind]] = None


class ClientCapabilities(BaseLspModel):
    workspace: Optional[WorkspaceCapabilities] = None
    text_document: Optional[TextDocumentClientCapabilities] = None
    notebook_document: Optional[NotebookDocumentClientCapabilities] = None
    window: Optional[WindowCapabilities] = None
    general: Optional[GeneralCapabilities] = None
    experimental: Optional[Any] = None
