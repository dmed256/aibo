from aibo.core import chat
from aibo.testing import factory


async def test_non_document_factories() -> None:
    await factory.FileImportFactory.build()
    await factory.FileVariableFactory.build()
    await factory.FileFunctionFactory.build()
    await factory.FileSchemaFactory.build()
    await factory.GitFileDocumentFactory.build()
    await factory.GitRepoDocumentFactory.build()
    await factory.HumanSourceFactory.build()
    await factory.OpenAIModelSourceFactory.build()
    await factory.ProgrammaticSourceFactory.build()

    for role in chat.MessageRole:
        await factory.MessageSourceFactory.build(role=role)

    await factory.TextMessageContentFactory.build()
    await factory.CompletionErrorContentFactory.build()
    await factory.ToolRequestContentFactory.build()
    await factory.ToolResponseContentFactory.build()


async def test_document_factories() -> None:
    await factory.MessageDocumentFactory.build()
    await factory.MessageDocumentFactory.create()

    await factory.ConversationDocumentFactory.build()
    await factory.ConversationDocumentFactory.create()

    conversation_doc = await factory.ConversationDocumentFactory.create()
    parent_message_doc = await factory.MessageDocumentFactory.create(
        conversation_doc=conversation_doc,
    )
    child_message_doc = await factory.MessageDocumentFactory.create(
        conversation_doc=conversation_doc,
    )
    await factory.MessageEdgeDocumentFactory.build(
        conversation_doc=conversation_doc,
        parent_doc=parent_message_doc,
        child_doc=child_message_doc,
    )
    await factory.MessageEdgeDocumentFactory.create(
        conversation_doc=conversation_doc,
        parent_doc=parent_message_doc,
        child_doc=child_message_doc,
    )
