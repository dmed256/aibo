import datetime as dt
import os
import subprocess
from typing import Annotated, Literal, Optional, Self, Union
from uuid import UUID

import numpy as np
import pymongo
import tqdm
from pydantic import BaseModel, Field

from aibo.common.classproperty import classproperty
from aibo.common.constants import Env
from aibo.common.openai import get_file_embedding, get_string_embedding
from aibo.common.time import now_utc
from aibo.db.documents.base_document import BaseDocument, Index
from aibo.db.documents.git_file_document import GitFileDocument

__all__ = [
    "GitRepoDocument",
]

UPDATE_LOCK_TIME = dt.timedelta(minutes=5)


def _get_nearest_git_dir(filepath: str) -> Optional[str]:
    filepath = os.path.expanduser(filepath)
    if not os.path.isdir(filepath):
        filepath = os.path.dirname(filepath)

    while filepath and filepath != "/":
        if os.path.isdir(os.path.join(filepath, ".git")):
            return filepath
        filepath = os.path.dirname(filepath)
        print(filepath)

    return None


class GitRepoDocument(BaseDocument):
    name: str
    origin: str
    update_locked_at: Optional[dt.datetime] = None
    created_at: dt.datetime = Field(default_factory=now_utc)
    deleted_at: Optional[dt.datetime] = None

    @classproperty
    def collection_name(cls) -> str:
        return "git_repos"

    @classmethod
    def indices(cls) -> list[Index]:
        return [
            Index(
                name="name",
                fields=[("name", pymongo.DESCENDING)],
                unique=False,
            ),
            Index(
                name="origin",
                fields=[("origin", pymongo.DESCENDING)],
                unique=True,
            ),
            Index(
                name="created_at",
                fields=[("created_at", pymongo.DESCENDING)],
                unique=False,
            ),
        ]

    @classmethod
    def get(cls, filepath: str) -> Self:
        git_dir = _get_nearest_git_dir(filepath)
        assert git_dir, f"File is not in a git directory: {filepath}"

        output = subprocess.check_output(
            "git remote get-url origin",
            shell=True,
            cwd=git_dir,
        )
        origin = output.decode("utf-8").strip()
        name = os.path.basename(origin)

        existing_doc = cls.find_one(
            {
                "name": name,
                "origin": origin,
            }
        )

        return (
            existing_doc
            or cls(
                name=name,
                origin=origin,
            ).insert()
        )

    @property
    def cache_dir(self) -> str:
        return os.path.expanduser(f"~/.cache/aibo/{self.name}")

    def _call_in_cache_dir(self, cmd: str) -> str:
        output = subprocess.check_output(
            cmd,
            shell=True,
            cwd=self.cache_dir,
        )
        return output.decode("utf-8")

    def get_git_commit(self) -> str:
        return self._call_in_cache_dir("git rev-parse --short HEAD").strip()

    def cache_git_repo(self, *, commitish: Optional[str] = None) -> None:
        commitish = commitish or "$(git symbolic-ref --short HEAD)"

        if not os.path.exists(self.cache_dir):
            os.makedirs(self.cache_dir, exist_ok=True)
            subprocess.check_call(
                f"git clone {self.origin} {self.cache_dir}",
                shell=True,
            )

        self._call_in_cache_dir("git fetch")
        self._call_in_cache_dir(f"git reset --hard origin/{commitish}")

    def get_git_filenames(self) -> set[str]:
        output = self._call_in_cache_dir("git ls-tree HEAD -r --name-only").strip()
        return {
            clean_entry
            for entry in output.split("\n")
            if (clean_entry := entry.strip())
        }

    @property
    def is_update_locked(self) -> bool:
        if self.update_locked_at is None:
            return False

        now_without_offset = now_utc().astimezone().replace(tzinfo=None)
        updated_locked_at = self.update_locked_at.astimezone().replace(tzinfo=None)
        is_locked = (now_without_offset - updated_locked_at) < UPDATE_LOCK_TIME
        if not is_locked:
            self.clear_update_lock()

        return is_locked

    def clear_update_lock(self) -> None:
        self.update_locked_at = None
        self.partial_update(
            id=self.id,
            update_locked_at=None,
        )

    def set_update_lock(self) -> None:
        self.update_locked_at = now_utc()
        self.partial_update(
            id=self.id,
            update_locked_at=self.update_locked_at,
        )

    def update_file_documents(self, *, bypass_update_lock: bool = False) -> None:
        """
        Updates the related GitFileDocument entries based on the current commit
        """
        if self.is_update_locked and not bypass_update_lock:
            return None

        self.cache_git_repo()
        self.set_update_lock()

        git_commit = self.get_git_commit()
        git_filenames = self.get_git_filenames()

        # Find up-to-date files that exist
        up_to_date_files_info = GitFileDocument.collection.find(
            {
                "git_repo_id": self.id,
                "git_commit": git_commit,
            },
            {"filename": 1},
        )

        up_to_date_filenames = {info["filename"] for info in up_to_date_files_info}
        missing_filenames = git_filenames - up_to_date_filenames

        if not missing_filenames:
            self.clear_update_lock()
            return None

        stale_files_info = GitFileDocument.collection.find(
            {
                "git_repo_id": self.id,
                "git_commit": {"$ne": git_commit},
            },
            {"_id": 1, "filename": 1},
        )
        # TODO(dmed): Only recompute embeddings if the file contents changed

        stale_file_info_by_filename = {
            info["filename"]: info for info in stale_files_info
        }
        stale_filenames = set(stale_file_info_by_filename.keys())

        # Soft-delete stale files
        removed_file_ids = [
            stale_file_info_by_filename[filename]
            for filename in (stale_filenames - git_filenames)
        ]
        if removed_file_ids:
            deleted_at = now_utc()
            GitFileDocument.collection.update_many(
                {"_id": {"$in": removed_file_ids}},
                {"$set": {"deleted_at": deleted_at}},
            )

        # Create or update missing files
        git_file_ids_to_sync: list[UUID] = []
        for filename in missing_filenames:
            file_info = stale_file_info_by_filename.get(filename)
            if file_info:
                file_id = file_info["_id"]
                GitFileDocument.partial_update(
                    id=file_id,
                    git_commit=git_commit,
                    embedding=None,
                    file_schema=None,
                )
                git_file_ids_to_sync.append(file_id)
            else:
                git_file_doc = GitFileDocument(
                    git_repo_id=self.id,
                    filename=filename,
                    git_commit=git_commit,
                ).insert()
                git_file_ids_to_sync.append(git_file_doc.id)

        self.clear_update_lock()

    def set_embeddings(
        self, *, bypass_update_lock: bool = False, show_progress: bool = False
    ) -> None:
        """
        Updates the (current) related GitFileDocument embedding values
        """
        if self.is_update_locked and not bypass_update_lock:
            return None

        env = Env.get()
        embedding_model = env.OPENAI_EMBEDDING_MODEL_NAME
        self.set_update_lock()

        git_commit = self.get_git_commit()

        # Find up-to-date files that exist
        file_infos = GitFileDocument.collection.find(
            {
                "git_repo_id": self.id,
                "git_commit": git_commit,
                "embedding": None,
            },
            {"_id": 1, "filename": 1},
        )
        for info in tqdm.tqdm(file_infos, disable=not show_progress):
            git_file_id = info["_id"]
            filename = info["filename"]
            GitFileDocument.partial_update(
                id=git_file_id,
                embedding=get_file_embedding(
                    f"{self.cache_dir}/{filename}",
                    model=embedding_model,
                ),
                embedding_model=embedding_model,
            )

    def query_file_contents(
        self,
        query: str,
        *,
        limit: int,
    ) -> list[GitFileDocument]:
        """
        Gets the top `limit` GitFileDocument results based on embedding similarity
        """
        query_embedding = np.array(get_string_embedding(query))

        file_infos = GitFileDocument.collection.find(
            {
                "git_repo_id": self.id,
                "git_commit": self.get_git_commit(),
            },
            {"_id": 1, "embedding": 1},
        )

        sorted_id_distance = sorted(
            [
                (info["_id"], np.dot(query_embedding, info["embedding"]))
                for info in file_infos
            ],
            key=lambda row: row[1],
            reverse=True,
        )

        git_file_indices = {
            id: index for index, (id, _) in enumerate(sorted_id_distance[:limit])
        }
        git_file_docs = GitFileDocument.find(
            {
                "_id": {"$in": list(git_file_indices.keys())},
            }
        )

        return sorted(
            git_file_docs,
            key=lambda doc: git_file_indices[doc.id],
        )
