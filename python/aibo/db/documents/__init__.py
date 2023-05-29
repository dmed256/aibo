from .conversation_document import *
from .message_content import *
from .message_document import *
from .message_edge_document import *
from .message_source import *

DOCUMENT_CLASSES = [MessageDocument, MessageEdgeDocument, ConversationDocument]


def migrate_documents():
    for document_class in DOCUMENT_CLASSES:
        document_class.migrate()
