from __future__ import annotations

import os
import re
import subprocess

from pydantic import BaseModel, ConfigDict

import aibo.tools.lsp.types as lsp_types
import aibo.tools.lsp.utils as lsp_utils
from aibo.tools.lang.python import ast_types
from aibo.tools.lang.python.source_finder import SourceFinder
from aibo.tools.lsp.lsp_service import LspService

__all__ = ["Finder"]


class SymbolSourceInfo(BaseModel):
    model_config = ConfigDict(arbitrary_types_allowed=True)

    symbol: lsp_types.WorkspaceSymbol | lsp_types.DocumentSymbol
    source_location: lsp_types.Location
    source: str

    @property
    def symbol_location(self) -> lsp_types.Location:
        return self.symbol.location

    @property
    def source_uri(self) -> str:
        return self.source_location.uri

    @property
    def source_filepath(self) -> str:
        return lsp_utils.uri_to_filepath(self.source_location.uri)


class Finder(BaseModel):
    model_config = ConfigDict(arbitrary_types_allowed=True)

    root_filepath: str
    root_uri: str
    uses_git: bool
    lsp_service: LspService

    @classmethod
    async def build(
        cls,
        *,
        root_filepath: str,
    ) -> "Finder":
        root_filepath = os.path.abspath(os.path.expanduser(root_filepath))
        root_uri = lsp_utils.filepath_to_uri(root_filepath)
        lsp_service = LspService(
            name="Finder",
            root_uri=root_uri,
            language="python",
            log=True,
        )
        await lsp_service.start()

        uses_git = any(".git" in dirs for root, dirs, files in os.walk(root_filepath))

        return cls(
            root_filepath=root_filepath,
            root_uri=root_uri,
            uses_git=uses_git,
            lsp_service=lsp_service,
        )

    def _get_filepaths(self, *, use_git: bool | None = None) -> list[str]:
        if use_git is None:
            use_git = self.uses_git

        if use_git:
            result = subprocess.run(
                ["git", "ls-files"],
                stdout=subprocess.PIPE,
                text=True,
                cwd=self.root_filepath,
            )
            return result.stdout.splitlines()

        return [
            os.path.join(root, filename)
            for root, _, filenames in os.walk(self.root_filepath)
            for filename in filenames
        ]

    def find_file(self, query: str, *, use_git: bool | None = None) -> list[str]:
        """
        Matches sequential characters, for example:
        - query="pyaichapy"
        - match="[py]thon/src/[ai]bo/core/[cha]t.[py]"
        """
        query_pattern = re.compile(
            "".join(
                [
                    ".*?",
                    "(.*?)".join([f"[{letter}]" for letter in query]),
                    ".*?",
                ]
            )
        )
        matched_scores_and_filepath: list[tuple[tuple[int, int], str]] = []

        for filepath in self._get_filepaths(use_git=use_git):
            if not (result := query_pattern.match(filepath)):
                continue

            # If a letter is matched, the group will be '' so we want to find
            # how many consecutive '' there are to find a better match
            groups = list(result.groups())
            index = 0
            segment_counts: list[int] = []
            while index < len(groups):
                start_index = index
                while index < len(groups) and not groups[index]:
                    index += 1
                segment_counts.append(index - start_index)
                index += 1

            score = sum(2**segment_count for segment_count in segment_counts)
            leftover_length = sum(len(group) for group in groups)

            matched_scores_and_filepath.append(((score, -leftover_length), filepath))

        return [
            filepath
            for _, filepath in sorted(matched_scores_and_filepath, reverse=True)
        ]

    async def find_symbol(
        self,
        symbol: str,
        *,
        kind: list[lsp_types.SymbolKind] | None = None,
    ) -> list[SymbolSourceInfo]:
        results = await self.lsp_service.query_workspace_symbols(
            query=symbol,
        )

        # Apply filters
        results = [
            result
            for result in (results or [])
            if isinstance(result, lsp_types.WorkspaceSymbol) and result.name == symbol
        ]
        if kind:
            results = [result for result in results if result.kind in kind]

        return [
            source_info
            for symbol in results
            if (source_info := self.get_source_info(symbol))
        ]

    def get_source_info(
        self, symbol: lsp_types.WorkspaceSymbol | lsp_types.DocumentSymbol
    ) -> SymbolSourceInfo | None:
        source_finder = SourceFinder(
            filepath=lsp_utils.uri_to_filepath(symbol.location.uri),
        )
        source_info = source_finder.get_source(
            position=symbol.location.range.start,
            expected_source_type=ast_types.get_symbol_kind_ast_type(
                symbol.kind,
            ),
        )
        if not source_info:
            return None

        return SymbolSourceInfo(
            symbol=symbol,
            source_location=source_info.location,
            source=source_info.source,
        )

    async def autocomplete_symbol(
        self,
        *,
        filepath: str,
        line: int,
        character: int,
    ) -> list[str]:
        """
        Fetch all visible symbols that could auto-complete at the given line:character position
        It's case-insensitive (e.g. `pa` could autocomplete `Package`)
        """
        result = await self.lsp_service.get_completion(
            filepath=filepath,
            line=line,
            character=character,
        )
        if result and isinstance(result, list) and isinstance(result[0], str):
            return result

        return []

    async def find_references(
        self,
        symbol: lsp_types.WorkspaceSymbol,
    ) -> list[lsp_types.Location]:
        loc = symbol.location
        pos = loc.range.start
        result = await self.lsp_service.get_references(
            filepath=lsp_utils.uri_to_filepath(loc.uri),
            line=pos.line,
            character=pos.character,
        )
        if isinstance(result, lsp_types.Location):
            return [result]

        return result or []

    async def find_definitions(
        self,
        symbol: lsp_types.WorkspaceSymbol,
    ) -> list[lsp_types.Location]:
        loc = symbol.location
        pos = loc.range.start
        result = await self.lsp_service.get_definition(
            filepath=lsp_utils.uri_to_filepath(loc.uri),
            line=pos.line,
            character=pos.character,
        )
        if isinstance(result, lsp_types.Location):
            return [result]

        return result or []
