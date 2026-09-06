from importlib.resources import files
from pathlib import Path

from aibo.common.constants import Env

ROLE_FILENAMES = {
    "manager": "m.md",
    "bot": "b.md",
    "shadow": "sb.md",
}


def ensure_runtime_docs(env: Env | None = None) -> None:
    """Seed customizable roles once and refresh the managed API reference."""
    env = env or Env.get()
    source_dir = files("aibo").joinpath("resources/docs")
    destinations = {
        "m.md": env.docs_dir / "m.md",
        "b.md": env.docs_dir / "b.md",
        "sb.md": env.docs_dir / "sb.md",
        "api.md": env.docs_dir / "aibo/api.md",
    }

    for filename, destination in destinations.items():
        if filename != "api.md" and destination.exists():
            continue
        destination.parent.mkdir(parents=True, exist_ok=True)
        content = source_dir.joinpath(filename).read_text(encoding="utf-8")
        try:
            mode = "w" if filename == "api.md" else "x"
            with destination.open(mode, encoding="utf-8") as output:
                output.write(content)
        except FileExistsError:
            # Another process may have seeded or customized the role meanwhile.
            pass


def role_instructions(kind: str, env: Env | None = None) -> str:
    env = env or Env.get()
    ensure_runtime_docs(env)
    return (env.docs_dir / ROLE_FILENAMES[kind]).read_text().strip()


def ensure_project_readme(name: str, env: Env | None = None) -> Path:
    env = env or Env.get()
    readme = env.projects_dir / name / "README.md"
    if not readme.exists():
        readme.parent.mkdir(parents=True, exist_ok=True)
        readme.write_text(f"# {name}\n\nAdd durable project instructions here.\n")
    return readme
