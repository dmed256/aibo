import shutil
import sys
from typing import Any, Optional

from pydantic import BaseModel

__all__ = [
    "LspServer",
    "lsp_servers_by_language",
    "get_lsp_server",
]


class LspServer(BaseModel):
    language: str
    command: list[str]
    initialization_options: Optional[dict[str, Any]] = None


jedi_server = LspServer(
    language="python",
    command=["jedi-language-server"],
    initialization_options={
        "markupKindPreferred": "markdown",
        "workspace": {
            "extraPaths": [],
            "environmentPath": sys.executable,
            "symbols": {"maxSymbols": 200},
        },
    },
)

pyright_server = LspServer(
    language="python",
    command=["pyright-langserver", "--stdio"],
    initialization_options={},
)

lsp_servers_by_language = {
    "python": [jedi_server, pyright_server],
}


def get_lsp_server(language: str) -> Optional[LspServer]:
    lsp_servers = lsp_servers_by_language.get(language)
    if not lsp_servers:
        return None

    for lsp_server in lsp_servers:
        if shutil.which(lsp_server.command[0]):
            return lsp_server

    return None
