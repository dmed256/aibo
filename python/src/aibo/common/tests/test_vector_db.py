from aibo.common.openai import Embeddings
from aibo.common.vector_db import VectorDB


def test_max_id() -> None:
    vector_db = VectorDB(embedding_length=2)
    embeddings1 = Embeddings(
        model="model",
        embeddings=[[1, 0], [1, 1]],
    )
    embeddings2 = Embeddings(
        model="model",
        embeddings=[[1.5, 0]],
    )
    embeddings3 = Embeddings(
        model="model",
        embeddings=[[0, 0], [0, 0.1], [0, 1.5]],
    )

    max_embeddings_1 = Embeddings(
        model="model",
        embeddings=[[1, 1]],
    )
    max_embeddings_2 = Embeddings(
        model="model",
        embeddings=[[2, 0]],
    )
    max_embeddings_3 = Embeddings(
        model="model",
        embeddings=[[0, 2]],
    )

    vector_db.register_embedding(id="1", embeddings=embeddings1)
    assert vector_db.find_max_id(max_embeddings_1) == "1"
    assert vector_db.find_max_id(max_embeddings_2) == "1"
    assert vector_db.find_max_id(max_embeddings_3) == "1"

    assert vector_db.find_max_ids(max_embeddings_1, count=1) == ["1"]
    assert vector_db.find_max_ids(max_embeddings_2, count=2) == ["1", "1"]
    assert vector_db.find_max_ids(max_embeddings_3, count=100) == ["1", "1"]

    vector_db.register_embedding(id="2", embeddings=embeddings2)
    assert vector_db.find_max_id(max_embeddings_1) == "1"
    assert vector_db.find_max_id(max_embeddings_2) == "2"
    assert vector_db.find_max_id(max_embeddings_3) == "1"

    assert vector_db.find_max_ids(max_embeddings_1, count=1) == ["1"]
    assert vector_db.find_max_ids(max_embeddings_2, count=2) == ["2", "1"]
    assert vector_db.find_max_ids(max_embeddings_3, count=100) == ["1", "2", "1"]

    vector_db.register_embedding(id="3", embeddings=embeddings3)
    assert vector_db.find_max_id(max_embeddings_1) == "1"
    assert vector_db.find_max_id(max_embeddings_2) == "2"
    assert vector_db.find_max_id(max_embeddings_3) == "3"

    assert vector_db.find_max_ids(max_embeddings_1, count=1) == ["1"]
    assert vector_db.find_max_ids(max_embeddings_2, count=2) == ["2", "1"]
    assert vector_db.find_max_ids(max_embeddings_3, count=100) == [
        "3",
        "1",
        "3",
        "3",
        "2",
        "1",
    ]

    vector_db.remove_embedding(id="3")
    assert vector_db.find_max_id(max_embeddings_1) == "1"
    assert vector_db.find_max_id(max_embeddings_2) == "2"
    assert vector_db.find_max_id(max_embeddings_3) == "1"

    assert vector_db.find_max_ids(max_embeddings_1, count=1) == ["1"]
    assert vector_db.find_max_ids(max_embeddings_2, count=2) == ["2", "1"]
    assert vector_db.find_max_ids(max_embeddings_3, count=100) == ["1", "2", "1"]

    vector_db.remove_embedding(id="2")
    assert vector_db.find_max_id(max_embeddings_1) == "1"
    assert vector_db.find_max_id(max_embeddings_2) == "1"
    assert vector_db.find_max_id(max_embeddings_3) == "1"

    assert vector_db.find_max_ids(max_embeddings_1, count=1) == ["1"]
    assert vector_db.find_max_ids(max_embeddings_2, count=2) == ["1", "1"]
    assert vector_db.find_max_ids(max_embeddings_3, count=100) == ["1", "1"]
