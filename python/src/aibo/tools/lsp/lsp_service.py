import asyncio
import os
import subprocess
import sys
import typing
from typing import Any, Optional, TypeVar

from pydantic import TypeAdapter

import aibo.tools.lsp.types as lsp_types
from aibo.tools.lsp.lsp_server import LspServer, get_lsp_server
from aibo.tools.lsp.pipe_comms import PipeComms
from aibo.tools.lsp.utils import filepath_to_uri, uri_to_filepath

__all__ = ["LspService"]

TLspResponse = TypeVar("TLspResponse", bound=lsp_types.LspResponse)


class LspService:
    def __init__(
        self,
        *,
        name: str,
        root_uri: str,
        language: str,
        log: bool = False,
    ):
        maybe_lsp_server = get_lsp_server(language)
        assert maybe_lsp_server, f"Valid {language} LSP server not found"

        self.name = name
        self.root_uri = root_uri

        self.is_logging = log
        self.comms = PipeComms(log=log)

        self.client_task: Optional[asyncio.Task] = None

        self.lsp_server: LspServer = maybe_lsp_server

        self.server_process: Optional[subprocess.Popen] = None
        self.server_capabilities: Optional[lsp_types.ServerCapabilities] = None
        self.server_responses: dict[str, lsp_types.LspResponse] = {}

    @classmethod
    def _get_args_uri(cls, *, uri: Optional[str], filepath: Optional[str]) -> str:
        if uri:
            return uri

        if filepath:
            return filepath_to_uri(filepath)

        raise ValueError("Missing uri or filepath")

    @property
    def is_server_initialized(self) -> bool:
        return self.server_capabilities is not None

    @property
    def is_server_alive(self) -> bool:
        if self.server_process is None:
            return False

        return self.server_process.poll() is None

    @property
    def is_client_alive(self) -> bool:
        if self.client_task is None:
            return False

        return not self.client_task.done()

    async def start(self, *, force_restart: bool = False) -> None:
        if force_restart:
            self.stop()

        self._start_server()
        await self._start_client()

    def _start_server(self) -> None:
        if self.is_server_alive:
            return

        self.server_process = subprocess.Popen(
            self.lsp_server.command,
            stdin=self.comms.client_read,
            stdout=self.comms.server_write,
            universal_newlines=True,
        )

    async def _start_client(self) -> None:
        if self.is_client_alive:
            return

        async def read_jsonrpc_responses() -> None:
            while True:
                try:
                    response = await lsp_types.LspResponse.read(self.comms.server_read)
                    if response:
                        self.server_responses[response.id] = response
                except Exception as exc:
                    import traceback

                    print(f"EXCEPTION: {exc}")
                    print(traceback.format_exc())

        self.client_task = asyncio.create_task(read_jsonrpc_responses())
        await self._initialize_client()

    def stop(self) -> None:
        if self.client_task and self.is_client_alive:
            self.client_task.cancel()

        if self.server_process and self.is_server_alive:
            self.server_process.kill()

        self.client_task = None

        self.server_process = None
        self.server_capabilities = None
        self.server_responses = {}

    async def request(
        self, request: lsp_types.LspRequest
    ) -> Optional[lsp_types.LspResponse]:
        request.write(self.comms.client_write)
        for _ in range(200):
            await asyncio.sleep(0.05)
            if request.id not in self.server_responses:
                continue

            response = self.server_responses[request.id]
            del self.server_responses[request.id]
            return response

        return None

    async def _initialize_client(self) -> None:
        initialization_request = lsp_types.InitializationRequest(
            params=lsp_types.InitializeParams(
                process_id=os.getpid(),
                root_uri=self.root_uri,
                client_info=lsp_types.ClientInfo(name=self.name),
                initialization_options=self.lsp_server.initialization_options,
                capabilities=lsp_types.ClientCapabilities(
                    text_document=lsp_types.TextDocumentClientCapabilities(
                        completion=lsp_types.CompletionClientCapabilities(
                            completion_item=lsp_types.CompletionClientCompletionItem(
                                commit_characters_support=True,
                                documentation_format=[
                                    lsp_types.MarkupKind.PLAINTEXT,
                                    lsp_types.MarkupKind.MARKDOWN,
                                ],
                            ),
                        ),
                        hover=lsp_types.HoverClientCapabilities(
                            content_format=[
                                lsp_types.MarkupKind.PLAINTEXT,
                                lsp_types.MarkupKind.MARKDOWN,
                            ],
                        ),
                        signature_help=lsp_types.SignatureHelpClientCapabilities(
                            signature_information=lsp_types.SignatureHelpClientSignatureInformation(
                                documentation_format=[
                                    lsp_types.MarkupKind.PLAINTEXT,
                                    lsp_types.MarkupKind.MARKDOWN,
                                ],
                            ),
                            context_support=True,
                        ),
                        definition=lsp_types.DefinitionClientCapabilities(
                            link_support=True,
                        ),
                        type_definition=lsp_types.TypeDefinitionClientCapabilities(
                            link_support=True,
                        ),
                        implementation=lsp_types.ImplementationClientCapabilities(
                            link_support=True,
                        ),
                        declaration=lsp_types.DeclarationClientCapabilities(
                            link_support=True,
                        ),
                    ),
                ),
            ),
        )

        response = await self.request(initialization_request)
        if response is not None and response.result is not None:
            result = lsp_types.InitializationResult.validate(response.result)
            self.server_capabilities = result.capabilities

    async def get_definition(
        self,
        *,
        uri: Optional[str] = None,
        filepath: Optional[str] = None,
        line: int,
        character: int,
    ) -> Optional[lsp_types.DefinitionResult]:
        uri = self._get_args_uri(uri=uri, filepath=filepath)

        response = await self.request(
            lsp_types.DefinitionRequest(
                params=lsp_types.DefinitionParams(
                    text_document=lsp_types.TextDocumentIdentifier(
                        uri=uri,
                    ),
                    position=lsp_types.Position(
                        line=line,
                        character=character,
                    ),
                )
            )
        )
        if response is None or response.result is None:
            return None

        return typing.cast(
            lsp_types.DefinitionResult,
            lsp_types.DefinitionResultAdapter.validate_python(response.result),
        )

    async def get_document_highlight(
        self,
        *,
        uri: Optional[str] = None,
        filepath: Optional[str] = None,
        line: int,
        character: int,
    ) -> Optional[lsp_types.DocumentHighlightResult]:
        uri = self._get_args_uri(uri=uri, filepath=filepath)

        response = await self.request(
            lsp_types.DocumentHighlightRequest(
                params=lsp_types.DocumentHighlightParams(
                    text_document=lsp_types.TextDocumentIdentifier(
                        uri=uri,
                    ),
                    position=lsp_types.Position(
                        line=line,
                        character=character,
                    ),
                )
            )
        )
        if response is None or response.result is None:
            return None

        return typing.cast(
            lsp_types.DocumentHighlightResult,
            lsp_types.DocumentHighlightResultAdapter.validate_python(response.result),
        )

    async def get_document_symbols(
        self,
        *,
        uri: Optional[str] = None,
        filepath: Optional[str] = None,
    ) -> Optional[lsp_types.DocumentSymbolResult]:
        uri = self._get_args_uri(uri=uri, filepath=filepath)

        response = await self.request(
            lsp_types.DocumentSymbolRequest(
                params=lsp_types.DocumentSymbolParams(
                    text_document=lsp_types.TextDocumentIdentifier(
                        uri=uri,
                    ),
                )
            )
        )
        if response is None or response.result is None:
            return None

        return typing.cast(
            lsp_types.DocumentSymbolResult,
            lsp_types.DocumentSymbolResultAdapter.validate_python(response.result),
        )

    async def get_references(
        self,
        *,
        uri: Optional[str] = None,
        filepath: Optional[str] = None,
        line: int,
        character: int,
        include_declaration: bool = True,
    ) -> Optional[lsp_types.ReferencesResult]:
        uri = self._get_args_uri(uri=uri, filepath=filepath)

        response = await self.request(
            lsp_types.ReferencesRequest(
                params=lsp_types.ReferencesParams(
                    text_document=lsp_types.TextDocumentIdentifier(
                        uri=uri,
                    ),
                    position=lsp_types.Position(
                        line=line,
                        character=character,
                    ),
                    context=lsp_types.ReferenceContext(
                        include_declaration=include_declaration,
                    ),
                )
            )
        )
        if response is None or response.result is None:
            return None

        return typing.cast(
            lsp_types.ReferencesResult,
            lsp_types.ReferencesResultAdapter.validate_python(response.result),
        )

    async def get_type_definition(
        self,
        *,
        uri: Optional[str] = None,
        filepath: Optional[str] = None,
        line: int,
        character: int,
    ) -> Optional[lsp_types.TypeDefinitionResult]:
        uri = self._get_args_uri(uri=uri, filepath=filepath)

        response = await self.request(
            lsp_types.TypeDefinitionRequest(
                params=lsp_types.TypeDefinitionParams(
                    text_document=lsp_types.TextDocumentIdentifier(
                        uri=uri,
                    ),
                    position=lsp_types.Position(
                        line=line,
                        character=character,
                    ),
                )
            )
        )
        if response is None or response.result is None:
            return None

        return typing.cast(
            lsp_types.TypeDefinitionResult,
            lsp_types.TypeDefinitionResultAdapter.validate_python(response.result),
        )

    async def query_workspace_symbols(
        self,
        *,
        query: str,
    ) -> Optional[lsp_types.WorkspaceSymbolResult]:
        response = await self.request(
            lsp_types.WorkspaceSymbolRequest(
                params=lsp_types.WorkspaceSymbolParams(
                    query=query,
                )
            )
        )
        if response is None or response.result is None:
            return None

        return typing.cast(
            lsp_types.WorkspaceSymbolResult,
            lsp_types.WorkspaceSymbolResultAdapter.validate_python(response.result),
        )

    async def get_signature_help(
        self,
        *,
        uri: Optional[str] = None,
        filepath: Optional[str] = None,
        line: int,
        character: int,
    ) -> Optional[lsp_types.SignatureHelpResult]:
        uri = self._get_args_uri(uri=uri, filepath=filepath)

        response = await self.request(
            lsp_types.SignatureHelpRequest(
                params=lsp_types.SignatureHelpParams(
                    text_document=lsp_types.TextDocumentIdentifier(
                        uri=uri,
                    ),
                    position=lsp_types.Position(
                        line=line,
                        character=character,
                    ),
                    context=lsp_types.SignatureHelpContext(
                        trigger_kind=lsp_types.SignatureHelpTriggerKind.INVOKED,
                    ),
                )
            )
        )
        if response is None or response.result is None:
            return None

        return typing.cast(
            lsp_types.SignatureHelpResult,
            lsp_types.SignatureHelpResultAdapter.validate_python(response.result),
        )

    async def get_completion(
        self,
        *,
        uri: Optional[str] = None,
        filepath: Optional[str] = None,
        line: int,
        character: int,
    ) -> Optional[lsp_types.CompletionResult]:
        uri = self._get_args_uri(uri=uri, filepath=filepath)

        response = await self.request(
            lsp_types.CompletionRequest(
                params=lsp_types.CompletionParams(
                    text_document=lsp_types.TextDocumentIdentifier(
                        uri=uri,
                    ),
                    position=lsp_types.Position(
                        line=line,
                        character=character,
                    ),
                    context=lsp_types.CompletionContext(
                        trigger_kind=lsp_types.CompletionTriggerKind.INVOKED,
                    ),
                )
            )
        )
        if response is None or response.result is None:
            return None

        return typing.cast(
            lsp_types.CompletionResult,
            lsp_types.CompletionResultAdapter.validate_python(response.result),
        )

    async def resolve_completion_item(
        self,
        *,
        completion_item: lsp_types.CompletionItem,
    ) -> Optional[lsp_types.CompletionItemResolveResult]:
        response = await self.request(
            lsp_types.CompletionItemResolveRequest(
                params=completion_item,
            )
        )
        if response is None or response.result is None:
            return None

        return lsp_types.CompletionItemResolveResultAdapter.validate_python(
            response.result
        )

    async def get_hover(
        self,
        *,
        uri: Optional[str] = None,
        filepath: Optional[str] = None,
        line: int,
        character: int,
    ) -> Optional[lsp_types.HoverResult]:
        uri = self._get_args_uri(uri=uri, filepath=filepath)

        response = await self.request(
            lsp_types.HoverRequest(
                params=lsp_types.HoverParams(
                    text_document=lsp_types.TextDocumentIdentifier(
                        uri=uri,
                    ),
                    position=lsp_types.Position(
                        line=line,
                        character=character,
                    ),
                )
            )
        )
        if response is None or response.result is None:
            return None

        return typing.cast(
            lsp_types.HoverResult,
            lsp_types.HoverResultAdapter.validate_python(response.result),
        )
