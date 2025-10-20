"""Discrete-time simulator for validating optimized tours."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Set, Tuple

from . import io
from .map_utils import MapSpec, all_cells, rasterize_no_fly

Cell = Tuple[int, int]


@dataclass
class SimulationResult:
    metrics: dict
    coverage_ok: bool
    collision_free: bool


def simulate(map_path: Path, params_path: Path, tours_path: Path, metrics_path: Path) -> SimulationResult:
    map_data = io.read_json(map_path)
    params = io.read_json(params_path)
    tours = io.read_json(tours_path)

    specification = MapSpec.from_json(map_data)
    blocked = specification.blocked_cells()
    free = specification.free_cells()

    visited: Set[Cell] = set()
    per_drone = []
    collision_free = True

    for tour in tours:
        drone_id = tour.get("drone_id")
        waypoints = sorted(tour.get("waypoints", []), key=lambda wp: wp["index"])
        segments = tour.get("segments", [])
        coverage_cells = []
        for waypoint in waypoints:
            cell = (int(waypoint["x"]), int(waypoint["y"]))
            coverage_cells.append(cell)
            if cell in blocked:
                collision_free = False
        visited.update(coverage_cells)
        per_drone.append(
            {
                "drone_id": drone_id,
                "path_length": float(tour["cost"].get("length", 0.0)),
                "turns": int(tour["cost"].get("turns", 0)),
                "recharges": len(tour.get("recharge_stops", [])),
                "time": float(tour["cost"].get("time", 0.0)),
            }
        )

    uncovered = sorted(list(free - visited))
    coverage_percentage = 100.0 * len(visited) / len(free) if free else 100.0
    coverage_ok = len(uncovered) == 0

    max_drones_allowed = int(params.get("max_drones", 5))
    if len(tours) > max_drones_allowed:
        raise RuntimeError("Optimized plan uses more drones than allowed")

    metrics = {
        "coverage": {
            "percentage": coverage_percentage,
            "uncovered_cells": [cell_to_json(cell) for cell in uncovered],
        },
        "drones": {
            "used": len(tours),
            "max": max_drones_allowed,
        },
        "makespan": max((entry["time"] for entry in per_drone), default=0.0),
        "total_time": sum(entry["time"] for entry in per_drone),
        "per_drone": per_drone,
    }

    io.write_json(metrics_path, metrics)

    if not coverage_ok or not collision_free:
        raise RuntimeError("Simulation uncovered coverage gaps or collisions")

    return SimulationResult(metrics, coverage_ok, collision_free)


def cell_to_json(cell: Cell) -> dict:
    x, y = cell
    return {"x": x, "y": y}


__all__ = ["simulate", "SimulationResult"]
