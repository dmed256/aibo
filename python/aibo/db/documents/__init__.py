from typing import Type

from .conversation_document import *
from .message_content import *
from .message_document import *
from .message_edge_document import *
from .message_source import *
from .base_document import BaseDocument

DOCUMENT_CLASSES: list[Type[BaseDocument]] = [MessageDocument, MessageEdgeDocument, ConversationDocument]


def migrate_documents() -> None:
    for document_class in DOCUMENT_CLASSES:
        document_class.migrate()
