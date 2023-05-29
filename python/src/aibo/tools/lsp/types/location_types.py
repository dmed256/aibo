from typing import Optional, Self

from aibo.tools.lsp.types.base_lsp_types import BaseLspModel

__all__ = [
    "Position",
    "Range",
    "Location",
    "LocationLink",
]


class Position(BaseLspModel):
    line: int
    character: int

    def __le__(self, other: Self) -> bool:
        return (self.line, self.character) < (other.line, other.character)


class Range(BaseLspModel):
    start: Position
    end: Position

    def __contains__(self, other: Position | Self) -> bool:
        if isinstance(other, Position):
            return self.start <= other and other <= self.end
        elif isinstance(other, Range):
            return self.start <= other.start and other.end <= self.end

        raise ValueError(f"Invalid type: {type(other)}")


class Location(BaseLspModel):
    uri: str
    range: Range

    def content(self) -> str:
        filepath = self.uri.removeprefix("file://")
        with open(filepath, "r") as fd:
            lines = fd.readlines()[self.range.start.line : self.range.end.line + 1]
            if len(lines) == 1:
                lines[0] = lines[0][
                    self.range.start.character : self.range.end.character
                ]
            else:
                lines[0] = lines[0][self.range.start.character :]
                lines[-1] = lines[-1][: self.range.end.character]

        return "".join(lines)

    def __contains__(self, other: Self) -> bool:
        return other.range in self.range


class LocationLink(BaseLspModel):
    origin_selection_range: Optional[Range]
    target_uri: str
    target_range: Range
    target_selection_range: Range
