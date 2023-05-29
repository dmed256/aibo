from pydantic import BaseModel

from aibo.common.types import StrEnum


class Order(StrEnum):
    ASC = "asc"
    DESC = "desc"


class OrderBy(BaseModel):
    field: str
    order: Order
