import asyncio
from typing import Type

from .base_document import BaseDocument
from .conversation_document import *
from .git_file_document import *
from .git_repo_document import *
from .message_content import *
from .message_document import *
from .message_edge_document import *
from .message_source import *

DOCUMENT_CLASSES: list[Type[BaseDocument]] = [
    MessageDocument,
    MessageEdgeDocument,
    ConversationDocument,
    GitRepoDocument,
    GitFileDocument,
]


async def migrate_documents() -> None:
    await asyncio.gather(
        *[document_class.migrate() for document_class in DOCUMENT_CLASSES]
    )
