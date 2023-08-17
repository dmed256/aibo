import datetime as dt
from typing import Optional
from uuid import UUID

from fastapi import APIRouter
from pydantic import BaseModel

from aibo.common.constants import Env
from aibo.core import chat
from aibo.server.routes import api_models

router = APIRouter(prefix="/chat")


class GetModelsResponse(BaseModel):
    model_names: list[str]


class GetToolsResponse(BaseModel):
    tool_names: list[str]


class SearchConversationsRequest(BaseModel):
    after_date: Optional[dt.date]
    before_date: Optional[dt.date]
    query: Optional[str]


class SearchConversationsResponse(BaseModel):
    conversations: list[api_models.ConversationSummary]


class CreateConversationRequest(BaseModel):
    model_name: Optional[str] = None
    temperature: Optional[float] = None
    max_tokens: Optional[int] = None
    enabled_tool_names: list[str] = []
    messages: list[chat.CreateMessageInputs]


class CreateConversationResponse(BaseModel):
    conversation: api_models.Conversation


class GetConversationResponse(BaseModel):
    conversation: api_models.Conversation


class EditConversationRequest(BaseModel):
    title: Optional[str]


class EditConversationResponse(BaseModel):
    conversation: api_models.Conversation


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


class GetConversationEdgesResponse(BaseModel):
    message_edges: list[api_models.MessageEdge]


@router.get("/models")
async def get_models() -> GetModelsResponse:
    return GetModelsResponse(model_names=["gpt-3.5-turbo"])


@router.get("/tools")
async def get_tools() -> GetToolsResponse:
    return GetToolsResponse(
        tool_names=[],
        # tool_names=[tool.name for tool in Tool.get_all_tools()],
    )


@router.post("/conversations/search")
async def search_conversations(
    request: Optional[SearchConversationsRequest] = None,
) -> SearchConversationsResponse:
    request = request or SearchConversationsRequest()

    def maybe_to_datetime(maybe_date: Optional[dt.date]) -> Optional[dt.date]:
        if maybe_date:
            return dt.datetime.fromordinal(maybe_date.toordinal())
        return None

    conversations = chat.ConversationSummary.search(
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

    enabled_tool_names = set(request.enabled_tool_names)
    enabled_tools = [
        tool for tool_name in enabled_tool_names if (tool := Tool.get(tool_name))
    ]
    if len(enabled_tools) != len(enabled_tool_names):
        missing_tool_names = [
            tool_name for tool_name in enabled_tool_names if Tool.get(tool_name) is None
        ]
        raise Exception(f"Tool(s) not found: {missing_tool_names}")

    openai_model_source = chat.OpenAIModelSource.build(
        model=request.model_name,
        temperature=request.temperature,
        max_tokens=request.max_tokens,
    )

    system_message_inputs, *other_message_inputs = request.messages

    conversation = chat.Conversation.create(
        openai_model_source=openai_model_source,
        enabled_tools=enabled_tools,
        system_message_inputs=system_message_inputs,
    )

    for message_input in other_message_inputs:
        conversation.insert_message(
            source=chat.HumanSource(user=env.CURRENT_USER),
            role=message_input.role,
            content=message_input.content,
        )

    return CreateConversationResponse(
        conversation=api_models.Conversation.from_chat(conversation)
    )


@router.get("/conversations/{conversation_id}")
async def get_conversation(
    conversation_id: UUID,
) -> GetConversationResponse:
    conversation = chat.Conversation.get(conversation_id)
    return GetConversationResponse(
        conversation=api_models.Conversation.from_chat(conversation)
    )


@router.patch("/conversations/{conversation_id}")
async def edit_conversation(
    conversation_id: UUID,
    request: EditConversationRequest,
) -> GetConversationResponse:
    conversation = chat.Conversation.get(conversation_id)

    if request.title is not None:
        conversation.set_title(request.title)

    return EditConversationResponse(
        conversation=api_models.Conversation.from_chat(conversation)
    )


@router.post("/conversations/{conversation_id}/generate-title")
async def generate_conversation_title(
    conversation_id: UUID,
) -> GenerateConversationTitleResponse:
    conversation = chat.Conversation.get(conversation_id)
    await conversation.generate_title()

    return GenerateConversationTitleResponse(
        conversation=api_models.Conversation.from_chat(conversation)
    )


@router.delete("/conversations/{conversation_id}")
async def delete_conversation(
    conversation_id: UUID,
) -> None:
    conversation = chat.Conversation.get(conversation_id)
    if not conversation:
        return

    conversation.soft_delete()


@router.post("/conversations/{conversation_id}/submit-user-message")
async def submit_user_message(
    conversation_id: UUID,
    request: SubmitUserMessageRequest,
) -> SubmitUserMessageResponse:
    conversation = chat.Conversation.get(conversation_id)
    conversation.insert_user_message(request.text)

    return SubmitUserMessageResponse(
        conversation=api_models.Conversation.from_chat(conversation)
    )


@router.post("/conversations/{conversation_id}/regenerate-assistant-message")
async def regenerate_assistant_message(
    conversation_id: UUID,
) -> RegenerateAssistantMessageResponse:
    conversation = chat.Conversation.get(conversation_id)

    current_message = conversation.current_message
    if current_message.role == chat.MessageRole.ASSISTANT:
        conversation.delete_message(current_message)

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
    conversation = chat.Conversation.get(conversation_id)
    message = conversation.all_messages.get(message_id)

    if not message:
        raise Exception("Message not found")

    if not isinstance(message.content, TextMessageContent):
        raise Exception(
            f"Expected TextMessageContent, found {message.content.__class__}"
        )

    if message.role != chat.MessageRole.USER:
        raise Exception(
            f"Expected last message to be an user role, found: {message.role}"
        )

    conversation.edit_message(
        message,
        source=message.source,
        role=message.role,
        content=TextMessageContent(text=request.text),
    )

    return EditMessageResponse(
        conversation=api_models.Conversation.from_chat(conversation),
    )


@router.delete("/conversations/{conversation_id}/messages/{message_id}")
async def delete_message(
    conversation_id: UUID,
    message_id: UUID,
    delete_after: bool = False,
) -> None:
    conversation = chat.Conversation.get(conversation_id)
    message = conversation.all_messages.get(message_id)

    if not message:
        raise Exception("Message not found")

    messages_to_delete = [message]
    if delete_after:
        message_index = len(conversation.get_message_history(message))
        messages_to_delete = conversation.get_current_history()[message_index:]

    for message in messages_to_delete:
        conversation.delete_message(message)

    return DeleteMessageResponse(
        conversation=api_models.Conversation.from_chat(conversation),
    )


@router.get("/conversations/{conversation_id}/edges")
async def get_conversation_edges(
    conversation_id: UUID,
) -> GetConversationEdgesResponse:
    return GetConversationEdgesResponse(
        message_edges=[
            api_models.MessageEdge.from_chat(edge)
            for edge in conversation.get_message_edges(include_deletions=True)
        ]
    )
