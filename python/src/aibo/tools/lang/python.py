import ast
import os
import typing
from typing import Optional, Type

from pydantic import BaseModel

import aibo.tools.lsp.types as lsp_types
from aibo.tools.lsp.utils import filepath_to_uri

__all__ = ["SourceFinder"]

AST_NESTED_SOURCE_TYPES = (
    ast.AsyncFor,
    ast.AsyncFunctionDef,
    ast.AsyncWith,
    ast.Call,
    ast.ClassDef,
    ast.Expr,
    ast.Expression,
    ast.FunctionDef,
    ast.FunctionType,
    ast.GeneratorExp,
    ast.If,
    ast.IfExp,
    ast.Import,
    ast.ImportFrom,
    ast.Lambda,
    ast.Module,
    ast.Raise,
    ast.Return,
    ast.Try,
    ast.While,
    ast.With,
    ast.Yield,
    ast.YieldFrom,
)


class SourceInfo(BaseModel):
    location: lsp_types.Location
    source: str


class SourceFinder:
    def __init__(self, *, filepath: str):
        self._filepath = os.path.abspath(os.path.expanduser(filepath))
        self._uri = filepath_to_uri(self._filepath)

        self._source: Optional[str] = None
        self._root_node: Optional[ast.Module] = None

    def _assert_source(self) -> None:
        if self._source:
            return

        with open(self._filepath, "r") as fd:
            self._source = fd.read()
            self._root_node = ast.parse(self._source, self._filepath)

    @property
    def filepath(self) -> str:
        return self.filepath

    @property
    def source(self) -> str:
        self._assert_source()
        return typing.cast(str, self._source)

    @property
    def root_node(self) -> ast.AST:
        self._assert_source()
        return typing.cast(ast.AST, self._root_node)

    def get_node_location(self, node: ast.AST) -> lsp_types.Location:
        node_lineno = node.lineno  # type: ignore[attr-defined]
        node_col_offset = node.col_offset  # type: ignore[attr-defined]

        end_lineno = node.end_lineno or node_lineno  # type: ignore[attr-defined]
        end_col_offset = node.end_col_offset or node_col_offset  # type: ignore[attr-defined]

        return lsp_types.Location(
            uri=self._uri,
            range=lsp_types.Range(
                start=lsp_types.Position(
                    line=node_lineno - 1, character=node_col_offset - 1
                ),
                end=lsp_types.Position(
                    line=end_lineno - 1, character=end_col_offset - 1
                ),
            ),
        )

    def get_source(
        self,
        *,
        position: lsp_types.Position,
        expected_source_type: tuple[Type[ast.AST]],
    ) -> Optional[SourceInfo]:
        return self._get_source_at_node(
            node=self.root_node,
            point=lsp_types.Location(
                uri=self._uri,
                range=lsp_types.Range(
                    start=position,
                    end=position,
                ),
            ),
            expected_source_type=expected_source_type,
        )

    def _get_source_at_node(
        self,
        *,
        node: ast.AST,
        point: lsp_types.Location,
        expected_source_type: tuple[Type[ast.AST], ...],
    ) -> Optional[SourceInfo]:
        found_child_node: Optional[ast.AST] = None
        for child_node in ast.iter_child_nodes(node):
            # We can't traverse this node
            if not isinstance(child_node, AST_NESTED_SOURCE_TYPES):
                continue

            if point in self.get_node_location(child_node):
                found_child_node = child_node
                break

        # No more nodes found at initial point
        if not found_child_node:
            return None

        # Keep traversing to see if we find a deeper node
        # of type [expected_source_type]
        child_source = self._get_source_at_node(
            node=found_child_node,
            point=point,
            expected_source_type=expected_source_type,
        )
        if child_source:
            return child_source

        if not isinstance(found_child_node, expected_source_type):
            return None

        return SourceInfo(
            location=self.get_node_location(found_child_node),
            source=ast.get_source_segment(self.source, found_child_node, padded=True),
        )

    def find_symbols(
        self,
        *,
        source_info: SourceInfo,
        name: str,
    ) -> list[SourceInfo]:
        return []
