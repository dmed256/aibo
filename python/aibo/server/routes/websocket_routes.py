import datetime as dt
from typing import Annotated, AsyncGenerator, Literal, Union
from uuid import UUID

from fastapi import APIRouter, WebSocket
from pydantic import Field

from aibo.core import chat
from aibo.server.routes import api_models
from aibo.server.routes.websocket_route import BaseEvent, WebsocketRouter

router = APIRouter()
ws_router = WebsocketRouter()


class StreamAssistantMessageEventRequest(BaseEvent):
    kind: Literal["stream_assistant_message"] = "stream_assistant_message"
    conversation_id: UUID
    model: str


class RegenerateLastAssistantMessageEventRequest(BaseEvent):
    kind: Literal[
        "regenerate_last_assistant_message"
    ] = "regenerate_last_assistant_message"
    conversation_id: UUID


class StreamAssistantMessageChunksEventRequest(BaseEvent):
    kind: Literal["stream_assistant_message_chunks"] = "stream_assistant_message_chunks"
    conversation_id: UUID
    model: str


class CurrentConversationEventResponse(BaseEvent):
    kind: Literal["current_conversation"] = "current_conversation"
    conversation: api_models.Conversation


class StreamAssistantMessageEventResponse(BaseEvent):
    kind: Literal["stream_assistant_message"] = "stream_assistant_message"
    conversation_id: UUID
    message: api_models.Message


class StreamAssistantMessageChunkEventResponse(BaseEvent):
    kind: Literal["stream_assistant_message_chunk"] = "stream_assistant_message_chunk"
    conversation_id: UUID
    chunk: chat.StreamingMessageResult


WebsocketEventRequest = Annotated[
    Union[
        StreamAssistantMessageEventRequest,
        StreamAssistantMessageChunksEventRequest,
        RegenerateLastAssistantMessageEventRequest,
    ],
    Field(discriminator="kind"),
]


WebsocketEventResponse = Annotated[
    Union[
        CurrentConversationEventResponse,
        StreamAssistantMessageEventResponse,
        StreamAssistantMessageChunkEventResponse,
    ],
    Field(discriminator="kind"),
]


ws_router.set_event_class(WebsocketEventRequest)


@ws_router.route
async def stream_assistant_message(
    websocket: WebSocket,
    event: StreamAssistantMessageEventRequest,
) -> AsyncGenerator[WebsocketEventResponse, None]:
    conversation = await chat.Conversation.get(event.conversation_id)
    assert conversation, f"Conversation doesn't exist: {event.conversation_id}"

    source = chat.OpenAIModelSource.build(model=event.model)
    async for message in conversation.stream_assistant_message(source=source):
        yield StreamAssistantMessageEventResponse(
            id=event.id,
            conversation_id=conversation.id,
            message=api_models.Message.from_chat(message),
        )

    yield CurrentConversationEventResponse(
        id=event.id, conversation=api_models.Conversation.from_chat(conversation)
    )


@ws_router.route
async def regenerate_last_assistant_message(
    websocket: WebSocket,
    event: RegenerateLastAssistantMessageEventRequest,
) -> AsyncGenerator[WebsocketEventResponse, None]:
    conversation = await chat.Conversation.get(event.conversation_id)
    assert conversation, f"Conversation doesn't exist: {event.conversation_id}"

    for message in reversed(conversation.get_current_history()):
        if message.role != chat.MessageRole.ASSISTANT:
            break

        await conversation.delete_message(message)

    yield CurrentConversationEventResponse(
        id=event.id, conversation=api_models.Conversation.from_chat(conversation)
    )

    async for message in conversation.stream_assistant_message():
        yield StreamAssistantMessageEventResponse(
            id=event.id,
            conversation_id=conversation.id,
            message=api_models.Message.from_chat(message),
        )

    yield CurrentConversationEventResponse(
        id=event.id, conversation=api_models.Conversation.from_chat(conversation)
    )


@ws_router.route
async def stream_assistant_message_chunks(
    websocket: WebSocket,
    event: StreamAssistantMessageChunksEventRequest,
) -> AsyncGenerator[WebsocketEventResponse, None]:
    conversation = await chat.Conversation.get(event.conversation_id)
    assert conversation, f"Conversation doesn't exist: {event.conversation_id}"

    source = chat.OpenAIModelSource.build(model=event.model)
    async for chunk in conversation.stream_assistant_message_chunks(source=source):
        yield StreamAssistantMessageChunkEventResponse(
            id=event.id,
            conversation_id=conversation.id,
            chunk=chunk,
        )


@router.websocket("/ws")
async def websocket_connection(websocket: WebSocket) -> None:
    await websocket.accept()
    while True:
        await ws_router.process(websocket)
