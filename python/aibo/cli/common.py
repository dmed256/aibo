import os
import subprocess
from pathlib import Path

from termcolor import colored

PKG_DIR = os.path.abspath(Path(__file__) / ".." / "..")
ENV = dict(os.environ)


def call_command(cmd, **kwargs):
    print(colored(cmd, "green"))
    subprocess.check_call(cmd, shell=True, env=ENV, **kwargs)
