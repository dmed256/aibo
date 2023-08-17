import datetime as dt
from typing import Annotated, Literal, Union
from uuid import UUID

from fastapi import APIRouter, WebSocket
from pydantic import BaseModel, Field

from aibo.core import chat
from aibo.server.routes import api_models
from aibo.server.routes.websocket_route import WebsocketRouter

router = APIRouter()
ws_router = WebsocketRouter()


class SubmitUserMessageEventRequest(BaseModel):
    kind: Literal["submit_user_message"] = "submit_user_message"
    conversation_id: UUID
    text: str


class SubmitUserMessageEventResponse(BaseModel):
    kind: Literal["submit_user_message"] = "submit_user_message"
    message: chat.Message
    is_done: bool


WebsocketEventRequest = Annotated[
    Union[SubmitUserMessageEventRequest,],
    Field(discriminator="kind"),
]


ws_router.set_event_class(WebsocketEventRequest)


@ws_router.route
async def submit_user_message(
    websocket: WebSocket,
    event: SubmitUserMessageEventRequest,
):
    conversation = chat.Conversation.get(event.conversation_id)
    conversation.insert_user_message(event.text)

    async for streamed_message in conversation.stream_assistant_message():
        await websocket.send_text(
            SubmitUserMessageResponse(
                message=streamed_message.message,
                is_done=streamed_message.is_done,
            ).json()
        )


@router.websocket("/ws")
async def websocket_connection(websocket: WebSocket):
    await websocket.accept()
    while True:
        await ws_router.process(websocket)
