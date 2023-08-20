import os
import subprocess
from pathlib import Path
from typing import Any

from termcolor import colored

PKG_DIR = os.path.abspath(Path(__file__) / ".." / "..")
ENV = dict(os.environ)


def call_command(cmd: str, **kwargs: Any) -> None:
    print(colored(cmd, "green"))
    subprocess.check_call(cmd, shell=True, env=ENV, **kwargs)
