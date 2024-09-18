import datetime as dt
import logging
from uuid import UUID, uuid4

import sqlalchemy as sa
from fastapi import APIRouter
from pydantic import BaseModel, ConfigDict

from aibo.common.constants import Env
from aibo.common.openai import get_openai_model
from aibo.common.shorthands import (
    expand_contents_shorthands_inplace,
    expand_messages_shorthands_inplace,
)
from aibo.core import chat
from aibo.core.package import Package
from aibo.db.client import get_session
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
    after_date: dt.date | None = None
    before_date: dt.date | None = None
    query: str | None = None


class SearchConversationsResponse(BaseModel):
    conversations: list[api_models.ConversationSummary]


class CreateConversationRequest(BaseModel):
    model_config = ConfigDict(
        protected_namespaces=tuple(),
    )

    model: str | None = None
    temperature: float | None = None
    enabled_package_names: list[str] | None = None
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
    conversation_title: str
    search_result: str


class ConversationMessageSearchResponse(BaseModel):
    search_results: list[MessageSearchResult]


class EditConversationRequest(BaseModel):
    title: str | None


class EditConversationResponse(BaseModel):
    conversation: api_models.Conversation


class GenerateConversationTitleRequest(BaseModel):
    model_config = ConfigDict(
        protected_namespaces=tuple(),
    )

    model: str | None = None


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
    request: SearchConversationsRequest | None = None,
) -> SearchConversationsResponse:
    request = request or SearchConversationsRequest()

    def maybe_to_datetime(maybe_date: dt.date | None) -> dt.datetime | None:
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

    request_package_names = set(request.enabled_package_names or [])
    enabled_package_names = [
        package_name
        for package_name in request_package_names
        if Package.get(package_name)
    ]
    missing_package_names = [
        package_name
        for package_name in request_package_names
        if not Package.get(package_name)
    ]
    if missing_package_names:
        raise ValueError(f"Package(s) not found: {missing_package_names}")

    trace_id = uuid4()

    openai_model = get_openai_model(model=request.model, modalities=["text"])
    openai_model_source = chat.OpenAIModelSource.build(
        model=openai_model.model,
        temperature=request.temperature,
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

    # Make sure that we have a model set with the right modalities
    await conversation.maybe_override_openai_model_source(
        model=conversation.openai_model_source.model,
        temperature=request.temperature,
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
    if not request.query:
        return ConversationMessageSearchResponse(search_results=[])

    query = " AND ".join(
        [clean_part for part in request.query.split() if (clean_part := part.strip())]
    )
    async with get_session() as session:
        query_result = await session.execute(
            sa.text(
                """
                SELECT
                    conversations.id as conversation_id,
                    conversations.title as conversation_title,
                    snippet(message_content_search, 2, '', '', '...', 5) AS search_result
                FROM message_content_search
                JOIN conversations
                    ON conversations.id = message_content_search.conversation_id
                WHERE
                    message_content_search MATCH :query
                LIMIT :limit
                """
            ).params(
                query=query,
                limit=request.limit,
            )
        )

    return ConversationMessageSearchResponse(
        search_results=[
            MessageSearchResult(
                conversation_id=row.conversation_id,
                conversation_title=row.conversation_title,
                search_result=row.search_result,
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
