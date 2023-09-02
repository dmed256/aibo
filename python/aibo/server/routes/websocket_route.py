import inspect
import logging
import typing
from typing import Any, Callable, Literal, Type, TypeVar, Union
from uuid import UUID

import pydantic
from fastapi import WebSocket
from pydantic import BaseModel

logger = logging.getLogger(__name__)

T = TypeVar("T")
EventRoute = Callable[[WebSocket, T], Any]


class BaseEvent(BaseModel):
    id: UUID


class EventCompleted(BaseEvent):
    kind: Literal["event_completed"] = "event_completed"


class WebsocketRouter:
    def __init__(self) -> None:
        self._routes_by_event: dict[Type[Any], EventRoute[Any]] = {}

    def set_event_class(self, RequestClass: Any) -> None:
        self._event_request_class = RequestClass
        self._event_request_classes = typing.get_args(RequestClass)[0]

        if typing.get_origin(self._event_request_classes) is not Union:
            self._event_request_classes = [self._event_request_classes]
        else:
            self._event_request_classes = typing.get_args(self._event_request_classes)

    def route(self, event_route: EventRoute[T]) -> EventRoute[T]:
        event_route_signature = inspect.signature(event_route)
        param_types = [
            param.annotation for param in event_route_signature.parameters.values()
        ]

        if not (
            len(param_types) == 2
            and param_types[0] == WebSocket
            and param_types[1] in self._event_request_classes
        ):
            raise ValueError(
                f"{event_route.__name__}: Expected 2 arguments: (WebSocket, {self._event_request_class})"
            )

        event_request_cls = param_types[1]
        if event_request_cls in self._routes_by_event:
            raise ValueError(
                f"{event_route.__name__}: Duplicate event handling for {event_request_cls}"
            )

        self._routes_by_event[event_request_cls] = event_route

        return event_route

    async def process(self, websocket: WebSocket) -> None:
        event_json = await websocket.receive_json()
        event = pydantic.parse_obj_as(self._event_request_class, event_json)

        logger.info(f"Received websocket event: {event.kind} ({event.id})")

        event_route = self._routes_by_event.get(event.__class__)
        if not event_route:
            raise ValueError(f"No route for event: {event.__class__}")

        async for event in event_route(websocket, event):
            await websocket.send_text(event.json())

        await websocket.send_text(
            EventCompleted(id=event.id).json(),
        )

        logger.info(f"Completed websocket event: {event.kind} ({event.id})")
