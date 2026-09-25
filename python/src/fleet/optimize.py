"""OR-Tools scheduling of coverage tours onto a bounded fleet.

Each tour produced by the Haskell planner is a self-contained sortie set that
starts and ends at its depot. The optimizer decides which physical drone
flies which tours and in what order (a drone can fly several tours back to
back after recharging), so that

1. the fleet never exceeds ``max_drones``,
2. tours flown by the same drone start and end at the same depot, and
3. the objective ``makespan + drone_cost * drones`` is minimised, where
   ``drone_cost`` expresses how many seconds of makespan one extra drone is
   worth (the default of one second only breaks ties toward smaller fleets).
   With ``deadline`` set the number of drones is minimised first, subject to
   the makespan not exceeding the deadline.

Total path length is fixed by the planner (re-ordering tours does not change
it) so it is reported but not optimised here.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional, Tuple

from ortools.sat.python import cp_model

from . import io

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


class TourOptimizer:
    def __init__(
        self,
        tours: List[dict],
        max_drones: int,
        drone_cost: float = 1.0,
        deadline: Optional[float] = None,
        time_limit: float = 30.0,
    ) -> None:
        if not tours:
            raise ValueError("No tours provided")
        if max_drones < 1:
            raise ValueError("max_drones must be at least 1")
        self.tours = tours
        self.max_drones = max_drones
        self.drone_cost = drone_cost
        self.deadline = deadline
        self.time_limit = time_limit
        self.tasks = [
            TourTask(
                index=i,
                duration=float(tour["cost"]["time"]),
                length=float(tour["cost"]["length"]),
                turns=int(tour["cost"].get("turns", 0)),
                depot=tour_depot(tour),
                tour=tour,
            )
            for i, tour in enumerate(tours)
        ]
        self.horizon = (
            int(math.ceil(sum(task.duration for task in self.tasks) * SECONDS_SCALE))
            + 1
        )

    def solve(self) -> OptimizerResult:
        model = cp_model.CpModel()
        drones = range(self.max_drones)
        assignment: Dict[Tuple[int, int], cp_model.IntVar] = {}
        starts: Dict[int, cp_model.IntVar] = {}
        ends: Dict[int, cp_model.IntVar] = {}
        drone_used = [model.NewBoolVar(f"drone_used_{k}") for k in drones]
        makespan = model.NewIntVar(0, self.horizon, "makespan")

        for task in self.tasks:
            duration = int(round(task.duration * SECONDS_SCALE))
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
                        int(round(task.duration * SECONDS_SCALE)),
                        ends[task.index],
                        assignment[(task.index, k)],
                        f"interval_{task.index}_{k}",
                    )
                    for task in self.tasks
                ]
            )
            # A used drone must fly something.
            model.Add(
                sum(assignment[(t.index, k)] for t in self.tasks) >= 1
            ).OnlyEnforceIf(drone_used[k])

        # Drones are interchangeable: break the symmetry by filling low ids first.
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
            # Lexicographic: drones first, then makespan.
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

        return OptimizerResult(
            tours=optimized,
            objective_value=solver.ObjectiveValue(),
            drone_count=int(solver.Value(drone_count)),
            makespan=solver.Value(makespan) / SECONDS_SCALE,
            total_length=sum(task.length for task in self.tasks),
        )


def tour_depot(tour: dict) -> Tuple[int, int]:
    """The depot of a tour is where its first waypoint sits."""

    first = min(tour["waypoints"], key=lambda wp: wp["index"])
    return (int(first["x"]), int(first["y"]))


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
            recharge_stops.append(index_mapping.get(stop, stop))
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
) -> OptimizerResult:
    data = io.read_json(in_path)
    optimizer = TourOptimizer(
        data,
        max_drones=max_drones,
        drone_cost=drone_cost,
        deadline=deadline,
        time_limit=time_limit,
    )
    result = optimizer.solve()
    io.write_json(out_path, result.tours)
    return result


__all__ = ["TourOptimizer", "optimize_file", "OptimizerResult", "merge_tours"]
