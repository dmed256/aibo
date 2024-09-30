import ast
from typing import Type

import aibo.tools.lsp.types as lsp_types

__all__ = ["get_symbol_kind_ast_type"]


def get_symbol_kind_ast_type(
    symbol_kind: lsp_types.SymbolKind,
) -> tuple[Type[ast.AST], ...]:
    if symbol_kind in (lsp_types.SymbolKind.MODULE, lsp_types.SymbolKind.PACKAGE):
        return (ast.Module,)
    if symbol_kind in (lsp_types.SymbolKind.CLASS, lsp_types.SymbolKind.ENUM_MEMBER):
        return (ast.ClassDef,)
    if symbol_kind in (
        lsp_types.SymbolKind.METHOD,
        lsp_types.SymbolKind.CONSTRUCTOR,
        lsp_types.SymbolKind.FUNCTION,
    ):
        return (ast.AsyncFunctionDef, ast.FunctionDef)
    if symbol_kind in (
        lsp_types.SymbolKind.STRING,
        lsp_types.SymbolKind.NUMBER,
        lsp_types.SymbolKind.BOOLEAN,
        lsp_types.SymbolKind.ARRAY,
        lsp_types.SymbolKind.OBJECT,
    ):
        return (ast.Expression, ast.stmt)
    if symbol_kind == lsp_types.SymbolKind.OPERATOR:
        return (ast.operator,)

    return (ast.stmt,)
