from typing import Any

import numpy as np
from pydantic import BaseModel

from aibo.common.openai import EMBEDDING_LENGTH, Embeddings

__all__ = ["VectorDB"]


class VectorDB:
    def __init__(self, *, embedding_length: int = EMBEDDING_LENGTH) -> None:
        self.embedding_matrix = np.empty([0, embedding_length], dtype=np.float32)
        self.embedding_ids: list[str] = []
        self.embedding_id_set: set[str] = set()
        self.embedding_start_matrix_indices = np.empty([0], dtype=np.int32)

    @property
    def embedding_matrix_entries(self) -> Any:
        return self.embedding_matrix.shape[0]

    @property
    def embedding_length(self) -> Any:
        return self.embedding_matrix.shape[1]

    def has_id(self, id: str) -> bool:
        return id in self.embedding_id_set

    def clear(self) -> None:
        self.embedding_matrix = np.empty([0, self.embedding_length], dtype=np.float32)
        self.embedding_ids = []
        self.embedding_id_set = set()
        self.embedding_start_matrix_indices = np.empty([0], dtype=np.int32)

    def find_max_id(self, embeddings: Embeddings) -> str:
        return self.find_max_ids(embeddings, count=1)[0]

    def find_max_ids(self, embeddings: Embeddings, *, count: int) -> list[str]:
        embedding_dots = np.dot(self.embedding_matrix, embeddings.average_embedding())
        top_matrix_indices = np.argsort(embedding_dots)[-count:][::-1]

        # Find the ID associated with the top_matrix_index and handle the "min" edge case
        top_id_indices = [
            max(
                0,
                np.searchsorted(
                    self.embedding_start_matrix_indices, top_matrix_index, side="right"
                )
                - 1,
            )
            for top_matrix_index in top_matrix_indices
        ]

        return [self.embedding_ids[top_id_index] for top_id_index in top_id_indices]

    def register_embedding(
        self, *, id: str, embeddings: Embeddings, force_update: bool = False
    ) -> None:
        if id in self.embedding_id_set:
            if force_update:
                self.remove_embedding(id=id)
            else:
                return

        self.embedding_ids.append(id)
        self.embedding_id_set.add(id)
        self.embedding_start_matrix_indices = np.append(
            self.embedding_start_matrix_indices, [self.embedding_matrix_entries]
        )

        embedding_arrays = np.array(embeddings.embeddings, dtype=np.float32)
        self.embedding_matrix = np.vstack((self.embedding_matrix, embedding_arrays))

    def remove_embedding(self, *, id: str) -> None:
        if id not in self.embedding_id_set:
            return

        # Find and remove the embedding ID
        id_index = self.embedding_ids.index(id)
        del self.embedding_ids[id_index]

        # Remove from the set
        self.embedding_id_set.remove(id)

        # Remove the embedding vectors
        start_index = self.embedding_start_matrix_indices[id_index]

        if id_index < len(self.embedding_ids) - 1:
            end_index = self.embedding_start_matrix_indices[id_index + 1]
        else:
            end_index = self.embedding_matrix_entries

        self.embedding_matrix = np.delete(
            self.embedding_matrix, np.s_[start_index:end_index], axis=0
        )

        # Shift the start indices since we deleted matrix columns
        # Update start_indices
        entries_removed = end_index - start_index
        self.embedding_start_matrix_indices[
            self.embedding_start_matrix_indices > id_index
        ] -= entries_removed
        self.embedding_start_matrix_indices = np.delete(
            self.embedding_start_matrix_indices, id_index
        )
