from aibo.core import chat
from aibo.testing import factory


class TestMessage:
    async def test_change_parent(self) -> None:
        conversation = await factory.ConversationFactory.build()
