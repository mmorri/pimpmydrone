"""Discrete grid utilities shared across the Python stack."""

from __future__ import annotations

import heapq
import math
from collections import deque
from dataclasses import dataclass
from typing import Iterable, Iterator, List, Sequence, Tuple

import numpy as np
from shapely import contains_xy
from shapely.geometry import MultiPolygon, Polygon

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
        polys = [
            Polygon([(v["x"], v["y"]) for v in poly["vertices"]]).buffer(0)
            for poly in data.get("no_fly", [])
        ]
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

    def reachable_cells(self) -> set[Cell]:
        """Free cells that some depot can fly to (4-connected flood fill)."""

        free = self.free_cells()
        return flood_fill(free, [d for d in self.depots if d in free])


def all_cells(width: int, height: int) -> Iterator[Cell]:
    """Yield every integer grid coordinate within the workspace."""

    for x in range(width):
        for y in range(height):
            yield (x, y)


def rasterize_no_fly(polygons: Sequence[Polygon], width: int, height: int) -> set[Cell]:
    """Return the set of cells whose centres fall inside any polygon.

    Only the cells inside each polygon's bounding box are tested, and the test
    itself is vectorised, so large maps stay cheap.
    """

    blocked: set[Cell] = set()
    for poly in polygons:
        if poly.is_empty:
            continue
        minx, miny, maxx, maxy = poly.bounds
        x0, x1 = max(0, int(np.floor(minx))), min(width - 1, int(np.ceil(maxx)))
        y0, y1 = max(0, int(np.floor(miny))), min(height - 1, int(np.ceil(maxy)))
        if x1 < x0 or y1 < y0:
            continue
        xs, ys = np.meshgrid(np.arange(x0, x1 + 1), np.arange(y0, y1 + 1))
        inside = contains_xy(poly, xs.ravel() + 0.5, ys.ravel() + 0.5)
        blocked.update(
            (int(x), int(y))
            for x, y in zip(xs.ravel()[inside], ys.ravel()[inside], strict=True)
        )
    return blocked


def polygon_rings(poly: Polygon | MultiPolygon) -> List[List[Tuple[float, float]]]:
    """Exterior rings of a (multi)polygon, without the closing vertex."""

    parts = poly.geoms if isinstance(poly, MultiPolygon) else [poly]
    rings = []
    for part in parts:
        coords = list(part.exterior.coords)
        if len(coords) > 1 and coords[0] == coords[-1]:
            coords = coords[:-1]
        rings.append(coords)
    return rings


def flood_fill(free: set[Cell], sources: Iterable[Cell]) -> set[Cell]:
    """All cells of ``free`` connected (4-neighbourhood) to any source."""

    seen: set[Cell] = set()
    queue = deque()
    for source in sources:
        if source in free and source not in seen:
            seen.add(source)
            queue.append(source)
    while queue:
        cell = queue.popleft()
        for nxt in neighbors4(cell):
            if nxt in free and nxt not in seen:
                seen.add(nxt)
                queue.append(nxt)
    return seen


def min_travel_cost(spec: "MapSpec", speed: float = 1.0) -> dict[Cell, float]:
    """Cheapest flight time from any depot to every reachable free cell.

    Unlike :func:`depot_distances` this pays the same turn penalty the planner
    does (one time unit per heading change), so it is a true lower bound on
    what any route to that cell can cost. States are (cell, heading) pairs;
    the first time a cell is settled the cost is optimal over all headings.
    """

    free = spec.free_cells()
    step = 1.0 / (speed or 1.0)
    best: dict[Cell, float] = {}
    # (cost, cell, heading); heading None means "not yet moving", so the first
    # step out of a depot is never charged a turn.
    queue = [(0.0, depot, None) for depot in spec.depots if depot in free]
    heapq.heapify(queue)
    settled: dict[tuple[Cell, object], float] = {}
    while queue:
        cost, cell, heading = heapq.heappop(queue)
        if settled.get((cell, heading), math.inf) < cost:
            continue
        if cell not in best:
            best[cell] = cost
        for nxt in neighbors4(cell):
            if nxt not in free:
                continue
            next_heading = (nxt[0] - cell[0], nxt[1] - cell[1])
            turn = 1.0 if heading is not None and next_heading != heading else 0.0
            next_cost = cost + step + turn
            key = (nxt, next_heading)
            if next_cost < settled.get(key, math.inf):
                settled[key] = next_cost
                heapq.heappush(queue, (next_cost, nxt, next_heading))
    return best


def depot_distances(spec: "MapSpec") -> dict[Cell, int]:
    """Grid distance from the nearest depot to every reachable free cell."""

    free = spec.free_cells()
    distances: dict[Cell, int] = {}
    queue = deque()
    for depot in spec.depots:
        if depot in free and depot not in distances:
            distances[depot] = 0
            queue.append(depot)
    while queue:
        cell = queue.popleft()
        for nxt in neighbors4(cell):
            if nxt in free and nxt not in distances:
                distances[nxt] = distances[cell] + 1
                queue.append(nxt)
    return distances


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
