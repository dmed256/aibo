from enum import IntEnum, StrEnum
from typing import Any, Literal

from pydantic import BaseModel

__all__ = [
    "CodeActionKind",
    "CompletionItemKind",
    "CompletionItemTag",
    "CompletionTriggerKind",
    "DiagnosticTag",
    "DocumentHighlightKind",
    "EmptyDict",
    "FailureHandlingKind",
    "FileOperationPatternKind",
    "FoldingRangeKind",
    "InsertTextFormat",
    "InsertTextMode",
    "MarkupKind",
    "PositionEncodingKind",
    "PrepareSupportDefaultBehavior",
    "ProgressToken",
    "ResourceOperationKind",
    "SignatureHelpTriggerKind",
    "SymbolKind",
    "SymbolTag",
    "TextDocumentSyncKind",
    "TokenFormat",
    "TraceValue",
]


class EmptyDict(BaseModel):
    pass


class SymbolKind(IntEnum):
    FILE = 1
    MODULE = 2
    NAMESPACE = 3
    PACKAGE = 4
    CLASS = 5
    METHOD = 6
    PROPERTY = 7
    FIELD = 8
    CONSTRUCTOR = 9
    ENUM = 10
    INTERFACE = 11
    FUNCTION = 12
    VARIABLE = 13
    CONSTANT = 14
    STRING = 15
    NUMBER = 16
    BOOLEAN = 17
    ARRAY = 18
    OBJECT = 19
    KEY = 20
    NULL = 21
    ENUM_MEMBER = 22
    STRUCT = 23
    EVENT = 24
    OPERATOR = 25
    TYPE_PARAMETER = 26


class CompletionItemKind(IntEnum):
    TEXT = 1
    METHOD = 2
    FUNCTION = 3
    CONSTRUCTOR = 4
    FIELD = 5
    VARIABLE = 6
    CLASS = 7
    INTERFACE = 8
    MODULE = 9
    PROPERTY = 10
    UNIT = 11
    VALUE = 12
    ENUM = 13
    KEYWORD = 14
    SNIPPET = 15
    COLOR = 16
    FILE = 17
    REFERENCE = 18
    FOLDER = 19
    ENUM_MEMBER = 20
    CONSTANT = 21
    STRUCT = 22
    EVENT = 23
    OPERATOR = 24
    TYPE_PARAMETER = 25


CodeActionKind = str
FoldingRangeKind = str
ProgressToken = int | str


class CompletionItemTag(IntEnum):
    DEPRECATED = 1


class DiagnosticTag(IntEnum):
    UNNECESSARY = 1
    DEPRECATED = 2


class DocumentHighlightKind(IntEnum):
    TEXT = 1
    READ = 2
    WRITE = 3


class InsertTextMode(IntEnum):
    AS_IS = 1
    ADJUST_INDENTATION = 2


class PrepareSupportDefaultBehavior(IntEnum):
    IDENTIFIER = 1


class SymbolTag(IntEnum):
    DEPRECATED = 1


class TextDocumentSyncKind(IntEnum):
    NONE = 0
    FULL = 1
    INCREMENTAL = 2


class SignatureHelpTriggerKind(IntEnum):
    INVOKED = 1
    TRIGGER_CHARACTER = 2
    CONTENT_CHANGE = 3


class CompletionTriggerKind(IntEnum):
    INVOKED = 1
    TRIGGER_CHARACTER = 2
    TRIGGER_FOR_INCOMPLETE_COMPLETIONS = 3


class InsertTextFormat(IntEnum):
    PLAIN_TEXT = 1
    SNIPPET = 2


class FailureHandlingKind(StrEnum):
    ABORT = "abort"
    TRANSACTIONAL = "transactional"
    UNDO = "undo"
    TEXT_ONLY_TRANSACTIONAL = "textOnlyTransactional"


class MarkupKind(StrEnum):
    PLAINTEXT = "plaintext"
    MARKDOWN = "markdown"


class PositionEncodingKind(StrEnum):
    UTF_8 = "utf-8"
    UTF_16 = "utf-16"
    UTF_32 = "utf-32"


class ResourceOperationKind(StrEnum):
    CREATE = "create"
    RENAME = "rename"
    DELETE = "delete"


class TokenFormat(StrEnum):
    RELATIVE = "relative"


class TraceValue(StrEnum):
    OFF = "off"
    MESSAGES = "messages"
    VERBOSE = "verbose"


class FileOperationPatternKind(StrEnum):
    FILE = "file"
    FOLDER = "folder"
