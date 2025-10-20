"""Discrete grid utilities shared across the Python stack."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, Iterator, List, Sequence, Tuple

import numpy as np
from shapely.geometry import Point, Polygon

Cell = Tuple[int, int]


@dataclass(frozen=True)
class MapSpec:
    """Container for grid map data loaded from ``map.json``."""

    width: int
    height: int
    seed: int
    obstacles: List[Cell]
    no_fly_polygons: List[Polygon]
    depots: List[Cell]

    @classmethod
    def from_json(cls, data: dict) -> "MapSpec":
        polys = [Polygon([(v["x"], v["y"]) for v in poly["vertices"]]).buffer(0) for poly in data.get("no_fly", [])]
        return cls(
            width=data["width"],
            height=data["height"],
            seed=data["seed"],
            obstacles=[(cell["x"], cell["y"]) for cell in data.get("obstacles", [])],
            no_fly_polygons=polys,
            depots=[(cell["x"], cell["y"]) for cell in data.get("depots", [])],
        )

    def blocked_cells(self) -> set[Cell]:
        blocked = set(self.obstacles)
        blocked.update(rasterize_no_fly(self.no_fly_polygons, self.width, self.height))
        return blocked

    def free_cells(self) -> set[Cell]:
        return set(all_cells(self.width, self.height)) - self.blocked_cells()


def all_cells(width: int, height: int) -> Iterator[Cell]:
    """Yield every integer grid coordinate within the workspace."""

    for x in range(width):
        for y in range(height):
            yield (x, y)


def rasterize_no_fly(polygons: Sequence[Polygon], width: int, height: int) -> set[Cell]:
    """Return the set of cells whose centers fall inside any polygon."""

    blocked: set[Cell] = set()
    if not polygons:
        return blocked
    for x in range(width):
        for y in range(height):
            center = Point(x + 0.5, y + 0.5)
            if any(poly.contains(center) for poly in polygons):
                blocked.add((x, y))
    return blocked


def is_inside(width: int, height: int, cell: Cell) -> bool:
    x, y = cell
    return 0 <= x < width and 0 <= y < height


def neighbors4(cell: Cell) -> Tuple[Cell, Cell, Cell, Cell]:
    x, y = cell
    return ((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1))


def manhattan(a: Cell, b: Cell) -> int:
    ax, ay = a
    bx, by = b
    return abs(ax - bx) + abs(ay - by)


def numpy_rng(seed: int) -> np.random.Generator:
    return np.random.default_rng(seed)
