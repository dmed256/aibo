from aibo.core import chat
from aibo.testing import factory


async def test_non_model_factories() -> None:
    await factory.HumanSourceFactory.build()
    await factory.OpenAIModelSourceFactory.build()
    await factory.ProgrammaticSourceFactory.build()

    for role in chat.MessageRole:
        await factory.MessageSourceFactory.build(role=role)

    await factory.TextMessageContentFactory.build()
    await factory.CompletionErrorContentFactory.build()
    await factory.FunctionRequestContentFactory.build()
    await factory.FunctionResponseContentFactory.build()


async def test_model_factories() -> None:
    await factory.MessageModelFactory.build()
    await factory.MessageModelFactory.create()

    await factory.ConversationModelFactory.build()
    await factory.ConversationModelFactory.create()
