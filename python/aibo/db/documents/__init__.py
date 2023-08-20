from typing import Type

from .base_document import BaseDocument
from .conversation_document import *
from .message_content import *
from .message_document import *
from .message_edge_document import *
from .message_source import *

DOCUMENT_CLASSES: list[Type[BaseDocument]] = [
    MessageDocument,
    MessageEdgeDocument,
    ConversationDocument,
]


def migrate_documents() -> None:
    for document_class in DOCUMENT_CLASSES:
        document_class.migrate()
