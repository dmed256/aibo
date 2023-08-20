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


class StreamAssistantMessageEventResponse(BaseEvent):
    kind: Literal["stream_assistant_message"] = "stream_assistant_message"
    message: api_models.Message


class StreamAssistantMessageChunksEventRequest(BaseEvent):
    kind: Literal["stream_assistant_message_chunks"] = "stream_assistant_message_chunks"
    conversation_id: UUID


class StreamAssistantMessageChunksEventResponse(BaseEvent):
    kind: Literal["stream_assistant_message_chunks"] = "stream_assistant_message_chunks"
    chunk: chat.StreamingMessageResult


WebsocketEventRequest = Annotated[
    Union[
        StreamAssistantMessageEventRequest,
        StreamAssistantMessageChunksEventRequest,
    ],
    Field(discriminator="kind"),
]


ws_router.set_event_class(WebsocketEventRequest)


@ws_router.route
async def stream_assistant_message(
    websocket: WebSocket,
    event: StreamAssistantMessageEventRequest,
) -> AsyncGenerator[StreamAssistantMessageEventResponse, None]:
    conversation = chat.Conversation.get(event.conversation_id)
    assert conversation, f"Conversation doesn't exist: {event.conversation_id}"

    async for message in conversation.stream_assistant_message():
        yield StreamAssistantMessageEventResponse(
            id=event.id,
            message=api_models.Message.from_chat(message),
        )


@ws_router.route
async def stream_assistant_message_chunks(
    websocket: WebSocket,
    event: StreamAssistantMessageChunksEventRequest,
) -> AsyncGenerator[StreamAssistantMessageChunksEventResponse, None]:
    conversation = chat.Conversation.get(event.conversation_id)
    assert conversation, f"Conversation doesn't exist: {event.conversation_id}"

    async for chunk in conversation.stream_assistant_message_chunks():
        yield StreamAssistantMessageChunksEventResponse(
            id=event.id,
            chunk=chunk,
        )


@router.websocket("/ws")
async def websocket_connection(websocket: WebSocket) -> None:
    await websocket.accept()
    while True:
        await ws_router.process(websocket)
