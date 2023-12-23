import datetime as dt
import logging
import re
from functools import cache
from typing import Optional
from uuid import UUID, uuid4

import sqlalchemy as sa
from fastapi import APIRouter
from pydantic import BaseModel, ConfigDict

from aibo.common.constants import Env
from aibo.common.openai import Modality, get_openai_model
from aibo.common.shorthands import (
    expand_contents_shorthands_inplace,
    expand_messages_shorthands_inplace,
    message_content_expands_image,
)
from aibo.core import chat
from aibo.core.package import Package
from aibo.db.client import get_session
from aibo.db.models import ConversationModel, ImageModel, MessageModel
from aibo.server.routes import api_models

logger = logging.getLogger(__name__)

router = APIRouter(prefix="/chat")


class GetModelsResponse(BaseModel):
    model_config = ConfigDict(
        protected_namespaces=tuple(),
    )

    models: list[str]


class GetRegisteredPackagesResponse(BaseModel):
    packages: list[str]


class SearchConversationsRequest(BaseModel):
    after_date: Optional[dt.date] = None
    before_date: Optional[dt.date] = None
    query: Optional[str] = None


class SearchConversationsResponse(BaseModel):
    conversations: list[api_models.ConversationSummary]


class CreateConversationRequest(BaseModel):
    model_config = ConfigDict(
        protected_namespaces=tuple(),
    )

    model: Optional[str] = None
    temperature: Optional[float] = None
    max_tokens: Optional[int] = None
    enabled_package_names: list[str] = []
    messages: list[chat.CreateMessageInputs]
    shorthands: dict[str, str] = {}


class CreateConversationResponse(BaseModel):
    conversation: api_models.Conversation


class GetConversationResponse(BaseModel):
    conversation: api_models.Conversation


class ConversationMessageSearchRequest(BaseModel):
    query: str
    limit: int = 50


class MessageSearchResult(BaseModel):
    conversation_id: UUID
    content_text: str


class ConversationMessageSearchResponse(BaseModel):
    search_results: list[MessageSearchResult]


class EditConversationRequest(BaseModel):
    title: Optional[str]


class EditConversationResponse(BaseModel):
    conversation: api_models.Conversation


class GenerateConversationTitleRequest(BaseModel):
    model_config = ConfigDict(
        protected_namespaces=tuple(),
    )

    model: Optional[str] = None


class GenerateConversationTitleResponse(BaseModel):
    conversation: api_models.Conversation


class SubmitUserMessageRequest(BaseModel):
    text: str


class SubmitUserMessageResponse(BaseModel):
    conversation: api_models.Conversation


class RegenerateAssistantMessageResponse(BaseModel):
    conversation: api_models.Conversation


class EditMessageRequest(BaseModel):
    text: str


class EditMessageResponse(BaseModel):
    conversation: api_models.Conversation


class DeleteMessageResponse(BaseModel):
    conversation: api_models.Conversation


class UpdateConversationPackagesRequest(BaseModel):
    package_enabled_updates: dict[str, bool | None]


class UpdateConversationPackagesResponse(BaseModel):
    conversation: api_models.Conversation


@router.get("/models")
async def get_models() -> GetModelsResponse:
    return GetModelsResponse(models=["gpt-3.5-turbo"])


@router.get("/packages")
async def get_packages() -> GetRegisteredPackagesResponse:
    return GetRegisteredPackagesResponse(
        packages=sorted([package.name for package in Package.registered_packages()])
    )


@router.post("/conversations/search")
async def search_conversations(
    request: Optional[SearchConversationsRequest] = None,
) -> SearchConversationsResponse:
    request = request or SearchConversationsRequest()

    def maybe_to_datetime(maybe_date: Optional[dt.date]) -> Optional[dt.datetime]:
        if maybe_date:
            return dt.datetime.fromordinal(maybe_date.toordinal())
        return None

    conversations = await chat.ConversationSummary.search(
        before_date=maybe_to_datetime(request.before_date),
        after_date=maybe_to_datetime(request.after_date),
        query=request.query,
    )
    return SearchConversationsResponse(
        conversations=[
            api_models.ConversationSummary.from_chat(conversation)
            for conversation in conversations
        ],
    )


@router.post("/conversations")
async def create_conversation(
    request: CreateConversationRequest,
) -> CreateConversationResponse:
    env = Env.get()

    enabled_package_names = [
        package_name
        for package_name in set(request.enabled_package_names)
        if Package.get(package_name)
    ]
    missing_package_names = [
        package_name
        for package_name in set(request.enabled_package_names)
        if not Package.get(package_name)
    ]
    if missing_package_names:
        raise ValueError(f"Package(s) not found: {missing_package_names}")

    trace_id = uuid4()

    modalities: list[Modality] = ["text"]
    if await message_content_expands_image(
        [
            content
            for message_input in request.messages
            for content in message_input.contents
        ]
    ):
        modalities.append("image")

    openai_model = get_openai_model(
        modalities=modalities,
        model=request.model,
    )
    openai_model_source = chat.OpenAIModelSource.build(
        model=openai_model.model,
        temperature=request.temperature,
        max_tokens=request.max_tokens,
    )

    await expand_messages_shorthands_inplace(
        trace_id=trace_id,
        messages=request.messages,
        shorthands=request.shorthands,
    )

    system_message_inputs, *other_message_inputs = request.messages

    conversation = await chat.Conversation.create(
        trace_id=trace_id,
        openai_model_source=openai_model_source,
        enabled_package_names=enabled_package_names,
        system_message_inputs=system_message_inputs,
    )

    for message_input in other_message_inputs:
        await conversation.insert_message(
            source=chat.HumanSource(user=env.CURRENT_USER),
            role=message_input.role,
            contents=message_input.contents,
        )

    return CreateConversationResponse(
        conversation=api_models.Conversation.from_chat(conversation)
    )


@router.get("/conversations/{conversation_id}")
async def get_conversation(
    conversation_id: UUID,
) -> GetConversationResponse:
    conversation = await chat.Conversation.get(conversation_id)
    assert conversation, "Conversation not found"

    return GetConversationResponse(
        conversation=api_models.Conversation.from_chat(conversation)
    )


@router.post("/conversations/message-search")
async def conversation_message_search(
    request: ConversationMessageSearchRequest,
) -> ConversationMessageSearchResponse:
    async with get_session() as session:
        query_result = await session.execute(
            sa.select(MessageModel)
            .join(
                ConversationModel, MessageModel.conversation_id == ConversationModel.id
            )
            .with_only_columns(
                ConversationModel.id.label("conversation_id"),
                MessageModel.content_text,
            )
            .where(
                MessageModel.content_text.ilike(f"%{request.query}%"),
                MessageModel.deleted_at == None,
                ConversationModel.deleted_at == None,
            )
            .order_by(ConversationModel.created_at.desc())
            .limit(request.limit)
        )

    return ConversationMessageSearchResponse(
        search_results=[
            MessageSearchResult(
                conversation_id=row.conversation_id,
                content_text=row.content_text,
            )
            for row in query_result.all()
        ]
    )


@router.patch("/conversations/{conversation_id}")
async def edit_conversation(
    conversation_id: UUID,
    request: EditConversationRequest,
) -> EditConversationResponse:
    conversation = await chat.Conversation.get(conversation_id)
    assert conversation, "Conversation not found"

    if request.title is not None:
        await conversation.set_title(request.title)

    return EditConversationResponse(
        conversation=api_models.Conversation.from_chat(conversation)
    )


@router.post("/conversations/{conversation_id}/generate-title")
async def generate_conversation_title(
    conversation_id: UUID,
    request: GenerateConversationTitleRequest,
) -> GenerateConversationTitleResponse:
    conversation = await chat.Conversation.get(conversation_id)
    assert conversation, "Conversation not found"

    await conversation.generate_title()

    return GenerateConversationTitleResponse(
        conversation=api_models.Conversation.from_chat(conversation)
    )


@router.delete("/conversations/{conversation_id}")
async def delete_conversation(
    conversation_id: UUID,
) -> None:
    conversation = await chat.Conversation.get(conversation_id)
    if not conversation:
        return

    await conversation.soft_delete()


@router.post("/conversations/{conversation_id}/submit-user-message")
async def submit_user_message(
    conversation_id: UUID,
    request: SubmitUserMessageRequest,
) -> SubmitUserMessageResponse:
    env = Env.get()

    conversation = await chat.Conversation.get(conversation_id)
    assert conversation, "Conversation not found"

    message_contents: list[list[chat.MessageContent]] = [
        [chat.TextMessageContent(text=request.text)]
    ]
    await expand_contents_shorthands_inplace(
        trace_id=conversation.trace_id,
        message_contents=message_contents,
        shorthands={},
    )
    await conversation.insert_message(
        source=chat.HumanSource(user=env.CURRENT_USER),
        role=chat.MessageRole.USER,
        contents=message_contents[0],
    )

    return SubmitUserMessageResponse(
        conversation=api_models.Conversation.from_chat(conversation)
    )


@router.post("/conversations/{conversation_id}/regenerate-assistant-message")
async def regenerate_assistant_message(
    conversation_id: UUID,
) -> RegenerateAssistantMessageResponse:
    conversation = await chat.Conversation.get(conversation_id)
    assert conversation, "Conversation not found"

    current_message = conversation.current_message
    if current_message.role == chat.MessageRole.ASSISTANT:
        await conversation.delete_message(current_message)

    await conversation.generate_assistant_message()

    return RegenerateAssistantMessageResponse(
        conversation=api_models.Conversation.from_chat(conversation)
    )


@router.put("/conversations/{conversation_id}/messages/{message_id}")
async def edit_message(
    conversation_id: UUID,
    message_id: UUID,
    request: EditMessageRequest,
) -> EditMessageResponse:
    conversation = await chat.Conversation.get(conversation_id)
    assert conversation, "Conversation not found"

    message = conversation.all_messages.get(message_id)
    assert message, "Message not found"

    if len(message.contents) != 1:
        raise Exception(f"Expected a 1-part message, found {len(message.contents)}")

    if not isinstance(message.contents[0], chat.TextMessageContent):
        raise Exception(
            f"Expected TextMessageContent, found {message.contents[0].__class__}"
        )

    if message.role != chat.MessageRole.USER:
        raise Exception(
            f"Expected last message to be an user role, found: {message.role}"
        )

    await conversation.edit_message(
        message,
        source=message.source,
        role=message.role,
        contents=[chat.TextMessageContent(text=request.text)],
    )

    return EditMessageResponse(
        conversation=api_models.Conversation.from_chat(conversation),
    )


@router.delete("/conversations/{conversation_id}/messages/{message_id}")
async def delete_message(
    conversation_id: UUID,
    message_id: UUID,
    delete_after: bool = False,
) -> DeleteMessageResponse:
    conversation = await chat.Conversation.get(conversation_id)
    assert conversation, "Conversation not found"

    message = conversation.all_messages.get(message_id)
    assert message, "Message not found"

    messages_to_delete = [message]
    if delete_after:
        message_index = len(conversation.get_message_history(message))
        messages_to_delete = conversation.get_current_history()[message_index:]

    for message in messages_to_delete:
        await conversation.delete_message(message)

    return DeleteMessageResponse(
        conversation=api_models.Conversation.from_chat(conversation),
    )


@router.patch("/conversations/{conversation_id}/packages")
async def update_conversation_packages(
    conversation_id: UUID,
    request: UpdateConversationPackagesRequest,
) -> UpdateConversationPackagesResponse:
    conversation = await chat.Conversation.get(conversation_id)
    assert conversation, "Conversation not found"

    # Keep packages that weren't specified or were explicitly set to enabled
    enabled_package_names = {
        package.name
        for package in conversation.packages
        if request.package_enabled_updates.get(package.name, True)
    }

    # Add potentially new enabled packages
    enabled_package_names |= {
        package_name
        for package_name, is_enabled in request.package_enabled_updates.items()
        if is_enabled
    }

    # Sort and remove non-existent packages
    await conversation.set_enabled_packages(list(enabled_package_names))

    return UpdateConversationPackagesResponse(
        conversation=api_models.Conversation.from_chat(conversation),
    )
