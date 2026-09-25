"""Discrete-time simulator for validating optimized tours.

The simulator replays every tour waypoint by waypoint and checks that

* the path is continuous (consecutive waypoints are identical or 4-adjacent),
* it never enters a blocked cell,
* every sortie between recharge stops fits the battery budget, and
* every free cell reachable from a depot is visited at least once.

Cells that no depot can reach are excluded from the coverage denominator;
they are reported separately so that the gap is visible in the metrics.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import List, Set, Tuple

from . import io
from .map_utils import MapSpec, manhattan

Cell = Tuple[int, int]

EPSILON = 1e-6


@dataclass
class SimulationResult:
    metrics: dict
    coverage_ok: bool
    collision_free: bool
    continuous: bool
    battery_ok: bool
    problems: List[str]

    @property
    def ok(self) -> bool:
        return (
            self.coverage_ok
            and self.collision_free
            and self.continuous
            and self.battery_ok
        )


def simulate(
    map_path: Path, params_path: Path, tours_path: Path, metrics_path: Path
) -> SimulationResult:
    map_data = io.read_json(map_path)
    params = io.read_json(params_path)
    tours = io.read_json(tours_path)

    spec = MapSpec.from_json(map_data)
    blocked = spec.blocked_cells()
    free = spec.free_cells()
    reachable = spec.reachable_cells()

    visited: Set[Cell] = set()
    per_drone = []
    problems: List[str] = []
    collision_free = True
    continuous = True
    battery_ok = True

    for tour in tours:
        drone_id = tour.get("drone_id")
        waypoints = sorted(tour.get("waypoints", []), key=lambda wp: wp["index"])
        cells = [(int(wp["x"]), int(wp["y"])) for wp in waypoints]

        collisions = [cell for cell in cells if cell in blocked]
        if collisions:
            collision_free = False
            problems.append(f"drone {drone_id} enters blocked cell {collisions[0]}")

        jumps = [
            (a, b)
            for a, b in zip(cells, cells[1:], strict=False)
            if manhattan(a, b) > 1
        ]
        if jumps:
            continuous = False
            problems.append(
                f"drone {drone_id} jumps from {jumps[0][0]} to {jumps[0][1]}"
            )

        battery_problem = check_battery(tour, params)
        if battery_problem:
            battery_ok = False
            problems.append(f"drone {drone_id}: {battery_problem}")

        visited.update(cells)
        per_drone.append(
            {
                "drone_id": drone_id,
                "path_length": float(tour["cost"].get("length", 0.0)),
                "turns": int(tour["cost"].get("turns", 0)),
                "recharges": len(tour.get("recharge_stops", [])),
                "time": float(tour["cost"].get("time", 0.0)),
            }
        )

    uncovered = sorted(reachable - visited)
    unreachable = sorted(free - reachable)
    covered = len(reachable & visited)
    coverage_percentage = 100.0 * covered / len(reachable) if reachable else 100.0
    coverage_ok = not uncovered
    if uncovered:
        problems.append(f"{len(uncovered)} reachable cell(s) never visited")

    max_drones_allowed = int(params.get("max_drones", 5))
    if len(tours) > max_drones_allowed:
        raise RuntimeError("Optimized plan uses more drones than allowed")

    metrics = {
        "coverage": {
            "percentage": coverage_percentage,
            "reachable_cells": len(reachable),
            "unreachable_cells": len(unreachable),
            "uncovered_cells": [cell_to_json(cell) for cell in uncovered],
        },
        "drones": {"used": len(tours), "max": max_drones_allowed},
        "makespan": max((entry["time"] for entry in per_drone), default=0.0),
        "total_time": sum(entry["time"] for entry in per_drone),
        "total_length": sum(entry["path_length"] for entry in per_drone),
        "per_drone": per_drone,
        "checks": {
            "collision_free": collision_free,
            "continuous": continuous,
            "battery_ok": battery_ok,
            "problems": problems,
        },
    }

    io.write_json(metrics_path, metrics)
    return SimulationResult(
        metrics, coverage_ok, collision_free, continuous, battery_ok, problems
    )


def check_battery(tour: dict, params: dict) -> str | None:
    """Replay the battery accounting of a tour; return a message on violation.

    Each sortie starts with the takeoff cost, accumulates ``battery_used`` per
    segment and must be able to absorb the landing cost at any time. Landing
    at a recharge stop resets the budget.
    """

    budget = float(params.get("B", 0.0))
    takeoff = float(params.get("takeoff_cost", 0.0))
    landing = float(params.get("landing_cost", 0.0))
    stops = set(tour.get("recharge_stops", []))
    segments = sorted(tour.get("segments", []), key=lambda seg: seg["from"])

    used = takeoff
    for segment in segments:
        used += float(segment.get("battery_used", 0.0))
        if used + landing > budget + EPSILON:
            return f"battery exhausted before waypoint {segment['to']}"
        if segment["to"] in stops:
            used = takeoff
    if used + landing > budget + EPSILON:
        return "battery exhausted before final landing"
    return None


def cell_to_json(cell: Cell) -> dict:
    x, y = cell
    return {"x": x, "y": y}


__all__ = ["simulate", "SimulationResult", "check_battery"]
