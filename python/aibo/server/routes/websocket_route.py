import inspect
import typing
from typing import Literal
from uuid import UUID

from fastapi import WebSocket
from pydantic import BaseModel


class BaseEvent(BaseModel):
    id: UUID


class EventCompleted(BaseEvent):
    kind: Literal["event_completed"] = "event_completed"


class WebsocketRouter:
    def __init__(self):
        self._routes_by_event = {}

    def set_event_class(self, RequestClass):
        self._event_request_class = RequestClass
        self._event_request_classes = typing.get_args(RequestClass)[0]

        # Unions of 1 type return itself rather than a list of 1 item
        if not isinstance(self._event_request_classes, list):
            self._event_request_classes = [self._event_request_classes]

    def route(self, event_route):
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

    async def process(self, websocket: WebSocket):
        event_json = await websocket.receive_json()
        event = self._event_request_class.parse_obj(event_json)

        event_route = self._routes_by_event.get(event.__class__)
        if not event_route:
            raise ValueError(f"No route for event: {event.__class__}")

        async for event in event_route(websocket, event):
            await websocket.send_text(event.json())

        await websocket.send_text(
            EventCompleted(id=event.id).json(),
        )
