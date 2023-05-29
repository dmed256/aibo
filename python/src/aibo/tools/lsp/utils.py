import os

__all__ = ["filepath_to_uri", "uri_to_filepath"]


def filepath_to_uri(filepath: str) -> str:
    filepath = os.path.abspath(os.path.expanduser(filepath))
    return f"file://{filepath}"


def uri_to_filepath(uri: str) -> str:
    return uri.removeprefix("file://")
