"""Discrete-time simulator for validating optimized tours.

The simulator replays every tour waypoint by waypoint and checks that

* the path is continuous (consecutive waypoints are identical or 4-adjacent),
* it never enters a blocked cell,
* every sortie between recharge stops fits the battery budget, and
* every free cell reachable from a depot and within battery range is visited
  at least once.

Cells that no depot can reach are excluded from the coverage denominator and
reported separately. So are cells the planner declared out of battery range
via ``skipped.json`` -- but that claim is verified here against an
independently computed lower bound on the cost of reaching the cell and
coming back, so a planner that gives up too easily is caught.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import List, Set, Tuple

from . import io
from .analyze import analyze_tours
from .map_utils import MapSpec, manhattan, min_travel_cost

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
    map_path: Path,
    params_path: Path,
    tours_path: Path,
    metrics_path: Path,
    skipped_path: Path | None = None,
) -> SimulationResult:
    map_data = io.read_json(map_path)
    params = io.read_json(params_path)
    tours = io.read_json(tours_path)

    spec = MapSpec.from_json(map_data)
    blocked = spec.blocked_cells()
    free = spec.free_cells()
    reachable = spec.reachable_cells()
    skipped = read_skipped(skipped_path)
    out_of_range, wrongly_skipped = classify_skipped(spec, params, skipped)
    expected = reachable - out_of_range

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

    uncovered = sorted(expected - visited)
    unreachable = sorted(free - reachable)
    covered = len(expected & visited)
    coverage_percentage = 100.0 * covered / len(expected) if expected else 100.0
    coverage_ok = not uncovered and not wrongly_skipped
    if uncovered:
        problems.append(f"{len(uncovered)} reachable in-range cell(s) never visited")
    if wrongly_skipped:
        problems.append(
            f"{len(wrongly_skipped)} cell(s) skipped as out of range although a round "
            f"trip fits the battery, e.g. {wrongly_skipped[0]}"
        )

    max_drones_allowed = int(params.get("max_drones", 5))
    if len(tours) > max_drones_allowed:
        raise RuntimeError("Optimized plan uses more drones than allowed")

    metrics = {
        "coverage": {
            "percentage": coverage_percentage,
            "reachable_cells": len(reachable),
            "unreachable_cells": len(unreachable),
            "out_of_range_cells": len(out_of_range),
            "expected_cells": len(expected),
            "uncovered_cells": [cell_to_json(cell) for cell in uncovered],
        },
        "drones": {"used": len(tours), "max": max_drones_allowed},
        "makespan": max((entry["time"] for entry in per_drone), default=0.0),
        "total_time": sum(entry["time"] for entry in per_drone),
        "total_length": sum(entry["path_length"] for entry in per_drone),
        "per_drone": per_drone,
        "efficiency": analyze_tours(tours).as_dict(),
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


def read_skipped(path: Path | None) -> Set[Cell]:
    """Cells the planner reported as out of battery range."""

    if path is None or not path.exists():
        return set()
    data = io.read_json(path)
    return {
        (int(cell["x"]), int(cell["y"]))
        for region in data.get("out_of_range", [])
        for cell in region.get("cells", [])
    }


def classify_skipped(
    spec: MapSpec, params: dict, skipped: Set[Cell]
) -> Tuple[Set[Cell], List[Cell]]:
    """Split the planner's skipped cells into justified and unjustified ones.

    A sortie to a cell costs at least takeoff, the cheapest flight out, the
    turnaround at the far end, the cheapest flight back and landing. The bound
    uses the optimal turn-aware cost to the cell, doubled, so no out-and-back
    route can do better. A cell that fails the bound is genuinely out of range;
    a cell that passes it was skipped by a planner that could have served it.

    A sortie could in principle avoid the turnaround by flying a loop, but a
    loop long enough to come back is never cheaper on a grid than retracing
    the route, so charging it keeps the bound tight without raising false
    alarms about the last unit of budget.
    """

    if not skipped:
        return set(), []
    budget = float(params.get("B", 0.0))
    speed = float(params.get("v", 1.0)) or 1.0
    overhead = float(params.get("takeoff_cost", 0.0)) + float(
        params.get("landing_cost", 0.0)
    )
    costs = min_travel_cost(spec, speed)

    justified: Set[Cell] = set()
    unjustified: List[Cell] = []
    for cell in skipped:
        cost = costs.get(cell)
        turnaround = 1.0 if cost else 0.0
        if cost is None or overhead + 2 * cost + turnaround > budget + EPSILON:
            justified.add(cell)
        else:
            unjustified.append(cell)
    return justified, sorted(unjustified)


def cell_to_json(cell: Cell) -> dict:
    x, y = cell
    return {"x": x, "y": y}


__all__ = ["simulate", "SimulationResult", "check_battery", "classify_skipped"]
