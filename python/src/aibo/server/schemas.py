import datetime as dt
from pathlib import Path
from uuid import UUID

from pydantic import BaseModel, ConfigDict, Field, computed_field, model_validator

from aibo.core.workspace import (
    ChatKind,
    ChatStatus,
    MessageKind,
    chat_label,
    chat_notice,
    last_active_at,
)
from aibo.db.models import ChatModel, NotificationModel


class Location(BaseModel):
    model_config = ConfigDict(from_attributes=True)

    id: UUID
    name: str
    path: str


class Project(BaseModel):
    model_config = ConfigDict(from_attributes=True)

    id: UUID
    name: str
    description: str
    archived_at: dt.datetime | None


class Attachment(BaseModel):
    path: Path
    name: str
    size: int
    url: str


class Message(BaseModel):
    model_config = ConfigDict(from_attributes=True)

    id: UUID
    kind: MessageKind
    content: str
    data: dict[str, object]
    created_at: dt.datetime

    @computed_field
    def attachments(self) -> list[Attachment]:
        paths = self.data.get("attachments")
        if not isinstance(paths, list):
            return []
        result = []
        for index, raw in enumerate(paths):
            if not isinstance(raw, str):
                continue
            path = Path(raw)
            try:
                size = path.stat().st_size
            except OSError:
                size = 0
            result.append(
                Attachment(
                    path=path,
                    name=path.name,
                    size=size,
                    url=f"/api/messages/{self.id}/attachments/{index}",
                )
            )
        return result


class ChatSummary(BaseModel):
    id: UUID
    kind: ChatKind
    label: str
    bot_number: int | None
    manager_number: int | None
    title: str
    status: ChatStatus
    active: bool
    codex_thread_id: str | None
    active_turn_id: str | None
    goal_enabled: bool
    goal: dict[str, object] | None
    tokens_used: int
    elapsed_seconds: float
    running_since: dt.datetime | None
    location: Location | None
    project: Project | None
    activity_at: dt.datetime
    last_active_at: dt.datetime | None
    created_at: dt.datetime

    @classmethod
    def from_model(cls, chat: ChatModel) -> "ChatSummary":
        status = ChatStatus(chat.status)
        return cls(
            id=chat.id,
            kind=ChatKind(chat.kind),
            label=chat_label(chat),
            bot_number=chat.bot_number,
            manager_number=chat.manager_number,
            title=chat.title,
            status=status,
            active=status.active,
            codex_thread_id=chat.codex_thread_id,
            active_turn_id=chat.active_turn_id,
            goal_enabled=chat.goal_enabled,
            goal=chat.goal,
            tokens_used=chat.tokens_used,
            elapsed_seconds=chat.elapsed_seconds,
            running_since=chat.running_since,
            location=(
                Location.model_validate(chat.location)
                if chat.location and chat.kind != ChatKind.MANAGER
                else None
            ),
            project=Project.model_validate(chat.project) if chat.project else None,
            activity_at=chat.activity_at,
            last_active_at=last_active_at(chat),
            created_at=chat.created_at,
        )


class Chat(ChatSummary):
    messages: list[Message]
    older_before: UUID | None = None
    history_version: str | None = None
    notice: str | None = None
    notice_message_id: UUID | None = None

    @classmethod
    def from_model(cls, chat: ChatModel) -> "Chat":
        summary = ChatSummary.from_model(chat)
        notice, notice_message_id = chat_notice(chat)
        return cls(
            **summary.model_dump(),
            messages=[
                Message.model_validate(message)
                for message in chat.messages
                if not message.data.get("streaming")
            ],
            notice=notice,
            notice_message_id=notice_message_id,
        )


class Notification(BaseModel):
    id: UUID
    chat: ChatSummary
    body: str
    created_at: dt.datetime
    read_at: dt.datetime | None

    @classmethod
    def from_model(cls, notification: NotificationModel) -> "Notification":
        return cls(
            id=notification.id,
            chat=ChatSummary.from_model(notification.chat),
            body=notification.body,
            created_at=notification.created_at,
            read_at=notification.read_at,
        )


class AccountUsage(BaseModel):
    remaining_percent: int | None = None
    resets: int | None = None


class Sidebar(BaseModel):
    active: list[ChatSummary]
    unread: list[Notification]
    read: list[Notification]
    read_count: int = 0
    account_usage: AccountUsage = Field(default_factory=AccountUsage)


class CreateLocation(BaseModel):
    name: str = Field(min_length=1, max_length=80, pattern=r"^[a-zA-Z0-9_-]+$")
    path: str = Field(min_length=1)


class UpdateLocation(BaseModel):
    path: str = Field(min_length=1)


class CreateProject(BaseModel):
    name: str = Field(min_length=1, max_length=80, pattern=r"^[a-zA-Z0-9_-]+$")
    description: str = Field(min_length=1, max_length=500)


class UpdateProject(BaseModel):
    description: str | None = Field(default=None, min_length=1, max_length=500)
    archived: bool | None = None


class CreateChat(BaseModel):
    kind: ChatKind
    goal: bool = True
    title: str = Field(default="New chat", max_length=300)
    user_message: str | None = None
    attachments: list[str] = Field(default_factory=list)
    source_message_id: UUID | None = Field(
        default=None,
        description="Delegate a stored manager user message, copying its text and all attachments.",
    )
    m_context: str | None = None
    location_id: UUID | None = None
    project_id: UUID | None = None

    @model_validator(mode="after")
    def validate_delegation(self) -> "CreateChat":
        if self.source_message_id:
            if self.kind != ChatKind.BOT:
                raise ValueError("source_message_id is only valid for bot chats")
            if self.user_message is not None or self.attachments:
                raise ValueError(
                    "Provide source_message_id or new message content, not both"
                )
        if (
            self.kind == ChatKind.BOT
            and (self.user_message or self.attachments or self.source_message_id)
            and self.m_context is None
        ):
            raise ValueError("bot delegation requires m_context")
        return self


class CreateMessage(BaseModel):
    kind: MessageKind
    content: str = Field(min_length=1)
    data: dict[str, object] = Field(default_factory=dict)


class UpdateStatus(BaseModel):
    status: ChatStatus
    reason: str | None = Field(default=None, min_length=1, max_length=1000)


class UpdateChatLocation(BaseModel):
    location_id: UUID


class UpdateChat(BaseModel):
    title: str | None = Field(default=None, min_length=1, max_length=300)
    project_id: UUID | None = None
    archived: bool | None = None


class SetGoal(BaseModel):
    enabled: bool = True


class CreateNotification(BaseModel):
    chat_id: UUID
    body: str = Field(min_length=1)


class SubmitMessage(BaseModel):
    text: str = ""
    goal: bool | None = None
    attachments: list[str] = Field(default_factory=list)
    message_id: UUID | None = None
    source_message_id: UUID | None = Field(
        default=None,
        description="Delegate a stored manager user message, copying its text and all attachments.",
    )
    m_context: str | None = None

    @model_validator(mode="after")
    def has_content(self) -> "SubmitMessage":
        if self.message_id:
            if (
                self.text
                or self.attachments
                or self.m_context is not None
                or self.source_message_id
            ):
                raise ValueError("Provide message_id or new message content, not both")
        elif self.source_message_id:
            if self.text or self.attachments:
                raise ValueError(
                    "Provide source_message_id or new message content, not both"
                )
            if self.m_context is None:
                raise ValueError("bot delegation requires m_context")
        elif not self.text.strip() and not self.attachments:
            raise ValueError("Provide text, an attachment, or message_id")
        return self


class UploadAttachment(BaseModel):
    name: str = Field(min_length=1, max_length=255)
    media_type: str = Field(pattern=r"^image/(png|jpeg|gif|webp)$")
    data: str = Field(min_length=1)


class StartedTurn(BaseModel):
    chat: Chat
    turn_id: str
