"""Random workspace generation utilities."""

from __future__ import annotations

import math
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, List, Tuple

import numpy as np
from shapely.geometry import Point, Polygon

from . import io
from .map_utils import MapSpec, all_cells, manhattan, numpy_rng, rasterize_no_fly

Cell = Tuple[int, int]


@dataclass
class GeneratorConfig:
    width: int = 100
    height: int = 100
    obstacle_density: float = 0.15
    no_fly_count: int = 2
    depot_count: int = 1
    seed: int = 1337
    speed: float = 1.0
    battery: float = 300.0
    takeoff_cost: float = 5.0
    landing_cost: float = 5.0
    max_drones: int = 5

    def validate(self) -> None:
        if not (0 <= self.obstacle_density <= 0.4):
            raise ValueError("Obstacle density must lie in [0, 0.4]")
        if self.no_fly_count < 0:
            raise ValueError("Number of no-fly zones cannot be negative")
        if self.depot_count < 1:
            raise ValueError("At least one depot is required")


def generate_workspace(config: GeneratorConfig) -> tuple[dict, dict]:
    """Generate ``map.json`` and ``params.json`` structures."""

    config.validate()
    rng = numpy_rng(config.seed)
    width, height = config.width, config.height

    obstacle_cells = sample_obstacles(rng, width, height, config.obstacle_density)
    no_fly_polys = build_no_fly_zones(rng, width, height, config.no_fly_count)
    no_fly_cells = rasterize_no_fly(no_fly_polys, width, height)

    available_cells = [cell for cell in all_cells(width, height) if cell not in obstacle_cells and cell not in no_fly_cells]
    if not available_cells:
        raise RuntimeError("Random generation produced a fully blocked map; try reducing density")

    depot_cells = choose_depots(rng, available_cells, config.depot_count)
    obstacle_cells = [cell for cell in obstacle_cells if cell not in depot_cells]

    map_payload = {
        "width": width,
        "height": height,
        "seed": config.seed,
        "obstacles": [cell_to_json(cell) for cell in obstacle_cells],
        "no_fly": [polygon_to_json(poly) for poly in no_fly_polys],
        "depots": [cell_to_json(cell) for cell in depot_cells],
        "params": {
            "v": config.speed,
            "B": config.battery,
            "takeoff_cost": config.takeoff_cost,
            "landing_cost": config.landing_cost,
            "max_drones": config.max_drones,
        },
    }

    params_payload = {
        "v": config.speed,
        "B": config.battery,
        "takeoff_cost": config.takeoff_cost,
        "landing_cost": config.landing_cost,
        "max_drones": config.max_drones,
    }

    return map_payload, params_payload


def write_workspace(map_path: Path, params_path: Path, config: GeneratorConfig) -> tuple[dict, dict]:
    """Generate and persist a workspace, returning the JSON data."""

    map_payload, params_payload = generate_workspace(config)
    io.write_json(map_path, map_payload)
    io.write_json(params_path, params_payload)
    return map_payload, params_payload


def sample_obstacles(rng: np.random.Generator, width: int, height: int, density: float) -> List[Cell]:
    mask = rng.random((height, width)) < density
    cells: List[Cell] = []
    for y in range(height):
        for x in range(width):
            if mask[y, x]:
                cells.append((x, y))
    return cells


def build_no_fly_zones(rng: np.random.Generator, width: int, height: int, count: int) -> List[Polygon]:
    polygons: List[Polygon] = []
    attempts = 0
    while len(polygons) < count and attempts < count * 10:
        attempts += 1
        poly = random_polygon(rng, width, height)
        if poly is None or poly.area < 1.0:
            continue
        polygons.append(poly)
    return polygons


def random_polygon(rng: np.random.Generator, width: int, height: int) -> Polygon | None:
    cx = rng.uniform(5, width - 5) if width > 10 else rng.uniform(0, width)
    cy = rng.uniform(5, height - 5) if height > 10 else rng.uniform(0, height)
    radius = max(3.0, min(width, height) * (0.05 + rng.random() * 0.1))
    vertex_count = rng.integers(5, 9)
    angles = np.sort(rng.uniform(0, 2 * math.pi, size=vertex_count))
    points = []
    for angle in angles:
        r = radius * (0.6 + 0.4 * rng.random())
        x = cx + r * math.cos(angle)
        y = cy + r * math.sin(angle)
        x = min(max(x, 0.5), width - 0.5)
        y = min(max(y, 0.5), height - 0.5)
        points.append((x, y))
    polygon = Polygon(points).buffer(0)
    if not polygon.is_valid:
        return None
    return polygon


def choose_depots(rng: np.random.Generator, candidates: List[Cell], count: int) -> List[Cell]:
    if len(candidates) < count:
        raise RuntimeError("Not enough free cells to place requested depots")
    indices = rng.choice(len(candidates), size=count, replace=False)
    return [candidates[i] for i in np.atleast_1d(indices)]


def cell_to_json(cell: Cell) -> dict:
    x, y = cell
    return {"x": int(x), "y": int(y)}


def polygon_to_json(poly: Polygon) -> dict:
    coords = list(poly.exterior.coords)
    if len(coords) > 1 and coords[0] == coords[-1]:
        coords = coords[:-1]
    vertices = [{"x": float(round(x, 3)), "y": float(round(y, 3))} for x, y in coords]
    return {"vertices": vertices}


__all__ = ["GeneratorConfig", "generate_workspace", "write_workspace"]
