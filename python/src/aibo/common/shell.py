import asyncio
import json
import re
from typing import Optional

from pydantic import BaseModel

__all__ = ["CommandOutput", "strip_ansi", "run_shell_command"]

ANSI_REGEX_PATTERN = re.compile(r"\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])")


class CommandOutput(BaseModel):
    exit_code: int
    stdout: str
    stderr: str


def strip_ansi(value: str) -> str:
    return re.sub(ANSI_REGEX_PATTERN, "", value)


async def run_shell_command(
    cmd: str,
    *,
    cwd: Optional[str] = None,
    check: bool = True,
    ansi: bool = False,
) -> CommandOutput:
    proc = await asyncio.create_subprocess_shell(
        cmd,
        stdout=asyncio.subprocess.PIPE,
        stderr=asyncio.subprocess.PIPE,
        shell=True,
        cwd=cwd,
    )

    stdout, stderr = await proc.communicate()
    output = CommandOutput(
        exit_code=proc.returncode,
        stdout=stdout.decode("utf-8"),
        stderr=stderr.decode("utf-8"),
    )

    if not ansi:
        output.stdout = strip_ansi(output.stdout)
        output.stderr = strip_ansi(output.stderr)

    if check:
        assert output.exit_code == 0, output.model_dump_json(indent=2)

    return output
