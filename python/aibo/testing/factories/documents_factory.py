import datetime as dt
from typing import Any, Optional

from aibo.common.constants import NULL_UUID
from aibo.common.time import now_utc
from aibo.core import chat
from aibo.db.documents import (
    CompletionErrorContent,
    ConversationDocument,
    FileFunction,
    FileImport,
    FileSchema,
    FileVariable,
    GitFileDocument,
    GitRepoDocument,
    HumanSource,
    MessageContent,
    MessageDocument,
    MessageEdgeDocument,
    MessageRole,
    MessageSource,
    OpenAIModelSource,
    ProgrammaticSource,
    TextMessageContent,
    ToolRequestContent,
    ToolResponseContent,
    ToolResponseErrorType,
    ToolResponseStatus,
)
from aibo.testing.factories.base_factory import BaseFactory, BaseMongoFactory, fake

__all__ = [
    "FileImportFactory",
    "FileVariableFactory",
    "FileFunctionFactory",
    "FileSchemaFactory",
    "GitFileDocumentFactory",
    "GitRepoDocumentFactory",
    "HumanSourceFactory",
    "OpenAIModelSourceFactory",
    "ProgrammaticSourceFactory",
    "MessageSourceFactory",
    "TextMessageContentFactory",
    "CompletionErrorContentFactory",
    "ToolRequestContentFactory",
    "ToolResponseContentFactory",
    "MessageEdgeDocumentFactory",
    "MessageDocumentFactory",
    "ConversationDocumentFactory",
]


# ---[ Git ]---------------------------------------
class FileImportFactory(BaseFactory[FileImport]):
    @staticmethod
    async def build(
        *,
        name: Optional[str] = None,
        alias: Optional[str] = None,
        imported_names: Optional[list[str]] = None,
        imports_all: Optional[bool] = None,
    ) -> FileImport:
        return FileImport(
            name=name or fake.word(),
            alias=alias or fake.word(),
            imported_names=imported_names or fake.words(nb=3),
            imports_all=imports_all or fake.boolean(),
        )


class FileVariableFactory(BaseFactory[FileVariable]):
    @staticmethod
    async def build(
        *,
        name: Optional[str] = None,
        type: Optional[str] = None,
        summary: Optional[str] = None,
    ) -> FileVariable:
        return FileVariable(
            name=name or fake.word(),
            type=type or fake.word(),
            summary=summary or fake.sentence(),
        )


class FileFunctionFactory(BaseFactory[FileFunction]):
    @staticmethod
    async def build(
        *,
        name: Optional[str] = None,
        summary: Optional[str] = None,
        arguments: Optional[list[FileVariable]] = None,
    ) -> FileFunction:
        return FileFunction(
            name=name or fake.word(),
            summary=summary or fake.sentence(),
            arguments=arguments or await FileVariableFactory.build_many(2),
        )


class FileSchemaFactory(BaseFactory[FileSchema]):
    @staticmethod
    async def build(
        *,
        version: Optional[str] = None,
        filename: Optional[str] = None,
        language: Optional[str] = None,
        summary: Optional[str] = None,
        imports: Optional[list[FileImport]] = None,
        functions: Optional[list[FileFunction]] = None,
        variables: Optional[list[FileVariable]] = None,
    ) -> FileSchema:
        return FileSchema(
            version=version or "v1",
            filename=filename or fake.file_name(),
            language=language or fake.word(),
            summary=summary or fake.sentence(),
            imports=imports or await FileImportFactory.build_many(3),
            functions=functions or await FileFunctionFactory.build_many(3),
            variables=variables or await FileVariableFactory.build_many(3),
        )


class GitFileDocumentFactory(BaseMongoFactory[GitFileDocument]):
    @staticmethod
    async def build(
        *,
        git_repo_doc: Optional[GitRepoDocument] = None,
        filename: Optional[str] = None,
        git_commit: Optional[str] = None,
        embedding: Optional[list[float]] = None,
        embedding_model: Optional[str] = None,
        file_schema: Optional[FileSchema] = None,
        created_at: Optional[dt.datetime] = None,
        deleted_at: Optional[dt.datetime] = None,
    ) -> GitFileDocument:
        git_repo_doc = git_repo_doc or await GitRepoDocumentFactory.create()
        return GitFileDocument(
            git_repo_id=git_repo_doc.id,
            filename=filename or fake.file_name(),
            git_commit=git_commit or fake.sha1(),
            embedding=embedding,
            embedding_model=embedding_model,
            file_schema=file_schema or await FileSchemaFactory.build(),
            created_at=created_at or now_utc(),
            deleted_at=deleted_at,
        )


class GitRepoDocumentFactory(BaseMongoFactory[GitRepoDocument]):
    @staticmethod
    async def build(
        *,
        name: Optional[str] = None,
        origin: Optional[str] = None,
        update_locked_at: Optional[dt.datetime] = None,
        created_at: Optional[dt.datetime] = None,
        deleted_at: Optional[dt.datetime] = None,
    ) -> GitRepoDocument:
        return GitRepoDocument(
            name=name or fake.word(),
            origin=origin or fake.url(),
            update_locked_at=update_locked_at,
            created_at=created_at or now_utc(),
            deleted_at=deleted_at,
        )


# ---[ Chat ]--------------------------------------
class HumanSourceFactory(BaseFactory[HumanSource]):
    @staticmethod
    async def build(*, user: Optional[str] = None) -> HumanSource:
        return HumanSource(
            user=user or fake.name(),
        )


class OpenAIModelSourceFactory(BaseFactory[OpenAIModelSource]):
    @staticmethod
    async def build(
        *,
        model: Optional[str] = None,
        temperature: Optional[float] = None,
        max_tokens: Optional[int] = None,
    ) -> OpenAIModelSource:
        return OpenAIModelSource(
            model=model or "your_model_name",
            temperature=temperature or 0.5,
            max_tokens=max_tokens,
        )


class ProgrammaticSourceFactory(BaseFactory[ProgrammaticSource]):
    @staticmethod
    async def build(*, source: Optional[str] = None) -> ProgrammaticSource:
        return ProgrammaticSource(
            source=source or fake.word(),
        )


class MessageSourceFactory(BaseFactory[MessageSource]):
    @staticmethod
    async def build(*, role: MessageRole) -> MessageSource:
        if role == chat.MessageRole.USER:
            return await HumanSourceFactory.build()

        if role == chat.MessageRole.ASSISTANT:
            return await OpenAIModelSourceFactory.build()

        return await ProgrammaticSourceFactory.build()


class TextMessageContentFactory(BaseFactory[TextMessageContent]):
    @staticmethod
    async def build(*, text: Optional[str] = None) -> TextMessageContent:
        return TextMessageContent(
            text=text or fake.sentence(),
        )


class CompletionErrorContentFactory(BaseFactory[CompletionErrorContent]):
    @staticmethod
    async def build(
        *,
        error_type: Optional[CompletionErrorContent.ErrorType] = None,
        text: Optional[str] = None,
    ) -> CompletionErrorContent:
        return CompletionErrorContent(
            error_type=error_type or fake.enum(CompletionErrorContent.ErrorType),
            text=text or fake.sentence(),
        )


class ToolRequestContentFactory(BaseFactory[ToolRequestContent]):
    @staticmethod
    async def build(
        *, to: Optional[str] = None, request: Optional[dict[str, Any]] = None
    ) -> ToolRequestContent:
        return ToolRequestContent(
            to=to or fake.word(),
            request=request or {"timestamp": fake.date_time()},
        )


class ToolResponseContentFactory(BaseFactory[ToolResponseContent]):
    @staticmethod
    async def build(
        *,
        tool: Optional[str] = None,
        status: Optional[ToolResponseStatus] = None,
        error_type: Optional[ToolResponseErrorType] = None,
        error_message: Optional[str] = None,
        response: Optional[dict[str, Any]] = None,
    ) -> ToolResponseContent:
        return ToolResponseContent(
            tool=tool or fake.word(),
            status=status or fake.enum(ToolResponseStatus),
            error_type=error_type or fake.enum(ToolResponseErrorType),
            error_message=error_message or fake.sentence(),
            response=response
            or {
                "isodate": fake.date(pattern="%Y-%m-%d"),
                "human_date": fake.date(pattern="%A - %B %d, %Y"),
            },
        )


class MessageEdgeDocumentFactory(BaseMongoFactory[MessageEdgeDocument]):
    @staticmethod
    async def build(
        *,
        conversation_doc: ConversationDocument,
        parent_doc: MessageDocument,
        child_doc: MessageDocument,
        created_at: Optional[dt.datetime] = None,
    ) -> MessageEdgeDocument:
        return MessageEdgeDocument(
            conversation_id=conversation_doc.id,
            parent_id=parent_doc.id,
            child_id=child_doc.id,
            created_at=created_at or now_utc(),
        )


class MessageDocumentFactory(BaseMongoFactory[MessageDocument]):
    @staticmethod
    async def build(
        *,
        conversation_doc: Optional[ConversationDocument] = None,
        parent_doc: Optional[MessageDocument] = None,
        source: Optional[MessageSource] = None,
        role: Optional[MessageRole] = None,
        content: Optional[MessageContent] = None,
        created_at: Optional[dt.datetime] = None,
        deleted_at: Optional[dt.datetime] = None,
    ) -> MessageDocument:
        conversation_doc = (
            conversation_doc or await ConversationDocumentFactory.create()
        )
        role = role or fake.random_element(elements=MessageRole)
        source = source or await MessageSourceFactory.build(role=role)
        content = content or await TextMessageContentFactory.build()

        return MessageDocument(
            conversation_id=conversation_doc.id,
            parent_id=parent_doc and parent_doc.id,
            source=source,
            source_text=str(source),
            role=role or fake.enum(MessageRole),
            content=content,
            content_text=str(content),
            created_at=created_at or now_utc(),
            deleted_at=deleted_at,
        )


class ConversationDocumentFactory(BaseMongoFactory[ConversationDocument]):
    @staticmethod
    async def build(
        *,
        title: Optional[str] = None,
        openai_model_source: Optional[OpenAIModelSource] = None,
        enabled_tool_names: Optional[list[str]] = None,
        conversation_depth: Optional[int] = None,
        created_at: Optional[dt.datetime] = None,
        deleted_at: Optional[dt.datetime] = None,
    ) -> ConversationDocument:
        return ConversationDocument(
            title=title or fake.sentence(),
            openai_model_source=openai_model_source
            or await OpenAIModelSourceFactory.build(),
            enabled_tool_names=enabled_tool_names or [],
            root_message_id=NULL_UUID,
            current_message_id=NULL_UUID,
            origin_message_id=NULL_UUID,
            conversation_depth=conversation_depth or 0,
            created_at=created_at or now_utc(),
            deleted_at=deleted_at,
        )

    @classmethod
    async def post_create(cls, obj: ConversationDocument) -> ConversationDocument:
        conversation_doc = obj
        system_message_doc = await MessageDocumentFactory.create(
            conversation_doc=conversation_doc,
            role=MessageRole.SYSTEM,
        )

        return ConversationDocument.partial_update(
            id=conversation_doc.id,
            root_message_id=system_message_doc.id,
            current_message_id=system_message_doc.id,
        )
