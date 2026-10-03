"""OR-Tools scheduling of coverage tours (or sorties) onto a bounded fleet.

Each tour from the Haskell planner is a self-contained path that starts and
ends at its depot, possibly with mid-tour recharge stops. The optimizer can:

* **tour mode** (legacy): assign whole region tours to drones;
* **sortie mode** (default): split each tour at recharge stops so drones can
  share a region's work and interleave sorties, subject to depot affinity;
* **columns mode**: generate same-depot sortie packs and solve a
  set-partitioning master (column-generation style) over that pool.

All modes minimise ``makespan + drone_cost * drones`` (or drones-first under
a deadline). After scheduling, :func:`fleet.deconflict.deconflict_tours`
staggers clocks so two drones do not occupy the same cell at the same time.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional, Tuple

from ortools.sat.python import cp_model

from . import io
from .deconflict import deconflict_tours

SECONDS_SCALE = 1000  # convert float seconds to millis for the integer solver


@dataclass
class TourTask:
    index: int
    duration: float
    length: float
    turns: int
    depot: Tuple[int, int]
    tour: dict


@dataclass
class OptimizerResult:
    tours: List[dict]
    objective_value: float
    drone_count: int
    makespan: float
    total_length: float
    deconflict: dict | None = None
    mode: str = "sortie"
    columns_generated: int | None = None
    columns_selected: int | None = None


class TourOptimizer:
    def __init__(
        self,
        tours: List[dict],
        max_drones: int,
        drone_cost: float = 1.0,
        deadline: Optional[float] = None,
        time_limit: float = 30.0,
        mode: str = "sortie",
        deconflict: bool = True,
    ) -> None:
        if not tours:
            raise ValueError("No tours provided")
        if max_drones < 1:
            raise ValueError("max_drones must be at least 1")
        if mode not in {"tour", "sortie", "columns"}:
            raise ValueError("mode must be 'tour', 'sortie', or 'columns'")
        self.raw_tours = tours
        self.max_drones = max_drones
        self.drone_cost = drone_cost
        self.deadline = deadline
        self.time_limit = time_limit
        self.mode = mode
        self.deconflict = deconflict
        units = (
            split_into_sorties(tours)
            if mode in {"sortie", "columns"}
            else list(tours)
        )
        self.tasks = [
            TourTask(
                index=i,
                duration=_tour_duration(tour),
                length=float(tour["cost"]["length"]),
                turns=int(tour["cost"].get("turns", 0)),
                depot=tour_depot(tour),
                tour=tour,
            )
            for i, tour in enumerate(units)
        ]
        self.horizon = (
            int(math.ceil(sum(task.duration for task in self.tasks) * SECONDS_SCALE))
            + 1
        )

    def solve(self) -> OptimizerResult:
        if self.mode == "columns":
            return self._solve_columns()
        return self._solve_intervals()

    def _solve_columns(self) -> OptimizerResult:
        from .columns import (
            columns_to_tours,
            generate_columns,
            solve_set_partition,
        )

        columns = generate_columns(self.tasks, max_drones=self.max_drones)
        selected, objective, drone_count, makespan_s = solve_set_partition(
            self.tasks,
            columns,
            max_drones=self.max_drones,
            drone_cost=self.drone_cost,
            deadline=self.deadline,
            time_limit=self.time_limit,
        )
        optimized = columns_to_tours(selected, self.tasks)
        deconflict_info = None
        if self.deconflict and len(optimized) > 1:
            result = deconflict_tours(optimized)
            optimized = result.tours
            deconflict_info = result.as_dict()
            makespan_s = max(
                (float(t["cost"].get("time", 0.0)) for t in optimized), default=0.0
            )
        return OptimizerResult(
            tours=optimized,
            objective_value=objective,
            drone_count=drone_count,
            makespan=makespan_s,
            total_length=sum(task.length for task in self.tasks),
            deconflict=deconflict_info,
            mode=self.mode,
            columns_generated=len(columns),
            columns_selected=len(selected),
        )

    def _solve_intervals(self) -> OptimizerResult:
        model = cp_model.CpModel()
        drones = range(self.max_drones)
        assignment: Dict[Tuple[int, int], cp_model.IntVar] = {}
        starts: Dict[int, cp_model.IntVar] = {}
        ends: Dict[int, cp_model.IntVar] = {}
        drone_used = [model.NewBoolVar(f"drone_used_{k}") for k in drones]
        makespan = model.NewIntVar(0, self.horizon, "makespan")

        for task in self.tasks:
            duration = max(1, int(round(task.duration * SECONDS_SCALE)))
            start = model.NewIntVar(0, self.horizon, f"start_{task.index}")
            end = model.NewIntVar(0, self.horizon, f"end_{task.index}")
            starts[task.index], ends[task.index] = start, end
            model.Add(end == start + duration)
            model.Add(end <= makespan)
            literals = []
            for k in drones:
                assign = model.NewBoolVar(f"assign_{task.index}_{k}")
                assignment[(task.index, k)] = assign
                model.AddImplication(assign, drone_used[k])
                literals.append(assign)
            model.AddExactlyOne(literals)

        for k in drones:
            model.AddNoOverlap(
                [
                    model.NewOptionalIntervalVar(
                        starts[task.index],
                        max(1, int(round(task.duration * SECONDS_SCALE))),
                        ends[task.index],
                        assignment[(task.index, k)],
                        f"interval_{task.index}_{k}",
                    )
                    for task in self.tasks
                ]
            )
            model.Add(
                sum(assignment[(t.index, k)] for t in self.tasks) >= 1
            ).OnlyEnforceIf(drone_used[k])

        for k in range(1, self.max_drones):
            model.AddImplication(drone_used[k], drone_used[k - 1])

        # A drone cannot teleport between depots.
        for a in self.tasks:
            for b in self.tasks:
                if a.index < b.index and a.depot != b.depot:
                    for k in drones:
                        model.AddBoolOr(
                            [
                                assignment[(a.index, k)].Not(),
                                assignment[(b.index, k)].Not(),
                            ]
                        )

        drone_count = model.NewIntVar(0, self.max_drones, "drone_count")
        model.Add(drone_count == sum(drone_used))

        if self.deadline is not None:
            model.Add(makespan <= int(math.floor(self.deadline * SECONDS_SCALE)))
            drone_weight = self.horizon + 1
        else:
            drone_weight = int(round(self.drone_cost * SECONDS_SCALE))
        model.Minimize(drone_weight * drone_count + makespan)

        solver = cp_model.CpSolver()
        solver.parameters.max_time_in_seconds = self.time_limit
        solver.parameters.num_search_workers = 8
        status = solver.Solve(model)
        if status not in (cp_model.OPTIMAL, cp_model.FEASIBLE):
            raise RuntimeError(
                "Optimizer found no schedule"
                + (" within the deadline" if self.deadline is not None else "")
            )

        schedule: Dict[int, List[Tuple[int, float]]] = {k: [] for k in drones}
        for task in self.tasks:
            for k in drones:
                if solver.Value(assignment[(task.index, k)]):
                    schedule[k].append(
                        (task.index, solver.Value(starts[task.index]) / SECONDS_SCALE)
                    )
                    break

        optimized = []
        for jobs in schedule.values():
            if jobs:
                jobs.sort(key=lambda item: item[1])
                optimized.append(merge_tours(len(optimized), jobs, self.tasks))

        deconflict_info = None
        if self.deconflict and len(optimized) > 1:
            result = deconflict_tours(optimized)
            optimized = result.tours
            deconflict_info = result.as_dict()
            # Recompute makespan after delays.
            makespan_s = max(
                (float(t["cost"].get("time", 0.0)) for t in optimized), default=0.0
            )
        else:
            makespan_s = solver.Value(makespan) / SECONDS_SCALE

        return OptimizerResult(
            tours=optimized,
            objective_value=solver.ObjectiveValue(),
            drone_count=int(solver.Value(drone_count)),
            makespan=makespan_s,
            total_length=sum(task.length for task in self.tasks),
            deconflict=deconflict_info,
            mode=self.mode,
        )


def _tour_duration(tour: dict) -> float:
    cost_t = float(tour.get("cost", {}).get("time", 0.0))
    if cost_t > 0:
        return cost_t
    wps = tour.get("waypoints") or []
    if not wps:
        return 1.0
    return max(float(wp.get("t", 0.0)) for wp in wps) or 1.0


def tour_depot(tour: dict) -> Tuple[int, int]:
    """The depot of a tour is where its first waypoint sits."""

    first = min(tour["waypoints"], key=lambda wp: wp["index"])
    return (int(first["x"]), int(first["y"]))


def split_into_sorties(tours: List[dict]) -> List[dict]:
    """Split each tour into sorties at recharge stops (inclusive landings)."""

    sorties: List[dict] = []
    for tour in tours:
        waypoints = sorted(tour.get("waypoints") or [], key=lambda wp: wp["index"])
        if len(waypoints) < 2:
            continue
        stops = sorted(set(tour.get("recharge_stops") or []))
        if not stops:
            sorties.append(_slice_tour(tour, waypoints, len(sorties)))
            continue
        bounds: List[int] = []
        for idx in [waypoints[0]["index"], *stops, waypoints[-1]["index"]]:
            if not bounds or idx != bounds[-1]:
                bounds.append(idx)
        for i in range(len(bounds) - 1):
            start_idx, end_idx = bounds[i], bounds[i + 1]
            window = [wp for wp in waypoints if start_idx <= wp["index"] <= end_idx]
            if len(window) < 2:
                continue
            sorties.append(_slice_tour(tour, window, len(sorties)))
    return sorties if sorties else list(tours)


def _slice_tour(tour: dict, window: List[dict], drone_hint: int) -> dict:
    """Extract a contiguous waypoint window as its own tour with remapped ids."""

    index_map = {wp["index"]: i for i, wp in enumerate(window)}
    t0 = float(window[0].get("t", 0.0))
    new_wps = [
        {
            "index": index_map[wp["index"]],
            "x": wp["x"],
            "y": wp["y"],
            "t": float(wp.get("t", 0.0)) - t0,
        }
        for wp in window
    ]
    segs = []
    for seg in tour.get("segments") or []:
        if seg["from"] in index_map and seg["to"] in index_map:
            segs.append(
                {
                    "from": index_map[seg["from"]],
                    "to": index_map[seg["to"]],
                    "length": float(seg["length"]),
                    "turn_cost": float(seg.get("turn_cost", 0.0)),
                    "battery_used": float(seg.get("battery_used", 0.0)),
                }
            )
    length = sum(float(s["length"]) for s in segs) or float(
        tour.get("cost", {}).get("length", 0.0)
    ) * (len(window) / max(len(tour.get("waypoints") or [1]), 1))
    turns = sum(int(s.get("turn_cost", 0) > 0) for s in segs)
    duration = max((wp["t"] for wp in new_wps), default=0.0) or 1.0
    return {
        "drone_id": drone_hint,
        "waypoints": new_wps,
        "segments": segs,
        "recharge_stops": [],
        "cost": {"time": duration, "turns": turns, "length": length},
    }


def merge_tours(
    drone_id: int, schedule: List[Tuple[int, float]], tasks: List[TourTask]
) -> dict:
    """Concatenate the tours a drone flies, in start-time order.

    The landing that ends one tour becomes a recharge stop of the merged tour
    so battery accounting stays valid across the boundary.
    """

    next_index = 0
    merged_waypoints: List[dict] = []
    merged_segments: List[dict] = []
    recharge_stops: List[int] = []
    total_turns = 0
    total_length = 0.0
    for job_index, start_time in schedule:
        tour = tasks[job_index].tour
        if merged_waypoints:
            recharge_stops.append(merged_waypoints[-1]["index"])
        index_mapping: Dict[int, int] = {}
        for waypoint in sorted(tour["waypoints"], key=lambda wp: wp["index"]):
            index_mapping[waypoint["index"]] = next_index
            merged_waypoints.append(
                {
                    "index": next_index,
                    "x": waypoint["x"],
                    "y": waypoint["y"],
                    "t": float(waypoint.get("t", 0.0)) + start_time,
                }
            )
            next_index += 1
        for segment in tour["segments"]:
            if segment["from"] not in index_mapping or segment["to"] not in index_mapping:
                continue
            merged_segments.append(
                {
                    "from": index_mapping[segment["from"]],
                    "to": index_mapping[segment["to"]],
                    "length": float(segment["length"]),
                    "turn_cost": float(segment.get("turn_cost", 0.0)),
                    "battery_used": float(segment.get("battery_used", 0.0)),
                }
            )
        for stop in tour.get("recharge_stops", []):
            if stop in index_mapping:
                recharge_stops.append(index_mapping[stop])
        total_turns += int(tour["cost"].get("turns", 0))
        total_length += float(tour["cost"].get("length", 0.0))

    return {
        "drone_id": drone_id,
        "waypoints": merged_waypoints,
        "segments": merged_segments,
        "recharge_stops": sorted(set(recharge_stops)),
        "cost": {
            "time": max((wp["t"] for wp in merged_waypoints), default=0.0),
            "turns": total_turns,
            "length": total_length,
        },
    }


def optimize_file(
    in_path: Path,
    out_path: Path,
    max_drones: int,
    drone_cost: float = 1.0,
    deadline: Optional[float] = None,
    time_limit: float = 30.0,
    mode: str = "sortie",
    deconflict: bool = True,
) -> OptimizerResult:
    data = io.read_json(in_path)
    optimizer = TourOptimizer(
        data,
        max_drones=max_drones,
        drone_cost=drone_cost,
        deadline=deadline,
        time_limit=time_limit,
        mode=mode,
        deconflict=deconflict,
    )
    result = optimizer.solve()
    io.write_json(out_path, result.tours)
    return result


__all__ = [
    "TourOptimizer",
    "optimize_file",
    "OptimizerResult",
    "merge_tours",
    "split_into_sorties",
]
