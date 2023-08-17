import datetime as dt
from typing import Annotated, Literal, Union
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


class SubmitUserMessageEventResponse(BaseEvent):
    kind: Literal["stream_assistant_message"] = "stream_assistant_message"
    message: api_models.Message


WebsocketEventRequest = Annotated[
    Union[StreamAssistantMessageEventRequest,],
    Field(discriminator="kind"),
]


ws_router.set_event_class(WebsocketEventRequest)


@ws_router.route
async def stream_assistant_message(
    websocket: WebSocket,
    event: StreamAssistantMessageEventRequest,
):
    conversation = chat.Conversation.get(event.conversation_id)
    async for message in conversation.stream_assistant_message():
        yield SubmitUserMessageEventResponse(
            id=event.id,
            message=api_models.Message.from_chat(message),
        )


@router.websocket("/ws")
async def websocket_connection(websocket: WebSocket):
    await websocket.accept()
    while True:
        await ws_router.process(websocket)
