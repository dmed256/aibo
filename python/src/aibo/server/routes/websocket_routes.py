import logging
from typing import Annotated, AsyncGenerator, Literal, Union
from uuid import UUID

from fastapi import APIRouter, WebSocket
from pydantic import Field

from aibo.core import chat
from aibo.server.routes import api_models
from aibo.server.routes.websocket_route import BaseEvent, WebsocketRouter

logger = logging.getLogger(__name__)

router = APIRouter()
ws_router = WebsocketRouter()


class StreamAssistantMessageEventRequest(BaseEvent):
    kind: Literal["stream_assistant_message"] = "stream_assistant_message"
    conversation_id: UUID
    model: str
    temperature: float | None = None


class RegenerateLastAssistantMessageEventRequest(BaseEvent):
    kind: Literal[
        "regenerate_last_assistant_message"
    ] = "regenerate_last_assistant_message"
    conversation_id: UUID
    model: str


class StreamAssistantMessageChunksEventRequest(BaseEvent):
    kind: Literal["stream_assistant_message_chunks"] = "stream_assistant_message_chunks"
    conversation_id: UUID
    model: str
    temperature: float | None = None


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
async def stream_assistant_message_route(
    websocket: WebSocket,
    event: StreamAssistantMessageEventRequest,
) -> AsyncGenerator[WebsocketEventResponse, None]:
    async for message in stream_assistant_message(websocket, event):
        yield message


async def stream_assistant_message(
    websocket: WebSocket,
    event: StreamAssistantMessageEventRequest,
    *,
    is_resample: bool = False,
) -> AsyncGenerator[WebsocketEventResponse, None]:
    conversation = await chat.Conversation.get(event.conversation_id)
    assert conversation, f"Conversation doesn't exist: {event.conversation_id}"

    if is_resample:
        should_generate_title = False
    elif conversation.title == chat.Conversation.default_title():
        should_generate_title = True
    else:
        should_generate_title = all(
            message.role != chat.MessageRole.ASSISTANT
            for message in conversation.get_current_history()
        )

    source = await conversation.maybe_override_openai_model_source(
        model=event.model,
        temperature=event.temperature,
    )
    async for messages in conversation.stream_assistant_messages(source=source):
        # TODO(dmed):
        #   Assumes we only stream 1 message at a time since we haven't added logic in elip
        #   to handle multiple streaming messages
        yield StreamAssistantMessageEventResponse(
            id=event.id,
            conversation_id=conversation.id,
            message=api_models.Message.from_chat(messages[0]),
        )

    yield CurrentConversationEventResponse(
        id=event.id, conversation=api_models.Conversation.from_chat(conversation)
    )

    if should_generate_title:
        await conversation.generate_title()
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

    messages_deleted = await conversation.delete_last_assistant_message()
    if messages_deleted:
        yield CurrentConversationEventResponse(
            id=event.id, conversation=api_models.Conversation.from_chat(conversation)
        )

    async for event_response in stream_assistant_message(
        websocket=websocket,
        event=StreamAssistantMessageEventRequest(
            id=event.id,
            conversation_id=event.conversation_id,
            model=event.model,
        ),
        is_resample=True,
    ):
        yield event_response


@ws_router.route
async def stream_assistant_message_chunks(
    websocket: WebSocket,
    event: StreamAssistantMessageChunksEventRequest,
) -> AsyncGenerator[WebsocketEventResponse, None]:
    conversation = await chat.Conversation.get(event.conversation_id)
    assert conversation, f"Conversation doesn't exist: {event.conversation_id}"

    source = await conversation.maybe_override_openai_model_source(
        model=event.model,
        temperature=event.temperature,
    )
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
