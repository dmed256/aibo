from enum import StrEnum

from pydantic import BaseModel


class Order(StrEnum):
    ASC = "asc"
    DESC = "desc"


class OrderBy(BaseModel):
    field: str
    order: Order
