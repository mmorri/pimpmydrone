"""OR-Tools refinement of coverage tours."""

from __future__ import annotations

import math
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Tuple

from ortools.sat.python import cp_model

from . import io

SECONDS_SCALE = 1000  # convert float seconds to millis for integer solver


@dataclass
class TourTask:
    index: int
    duration: float
    length: float
    turns: int
    tour: dict


@dataclass
class OptimizerResult:
    tours: List[dict]
    objective_value: float
    drone_count: int
    makespan: float


class TourOptimizer:
    def __init__(self, tours: List[dict], max_drones: int) -> None:
        if not tours:
            raise ValueError("No tours provided")
        self.tours = tours
        self.max_drones = max_drones
        self.tasks = [
            TourTask(
                index=i,
                duration=float(tour["cost"]["time"]),
                length=float(tour["cost"]["length"]),
                turns=int(tour["cost"].get("turns", 0)),
                tour=tour,
            )
            for i, tour in enumerate(tours)
        ]
        self.horizon = int(math.ceil(sum(task.duration for task in self.tasks) * SECONDS_SCALE)) + 1

    def solve(self) -> OptimizerResult:
        model = cp_model.CpModel()
        num_tasks = len(self.tasks)
        assignment: Dict[Tuple[int, int], cp_model.IntVar] = {}
        interval_vars: Dict[Tuple[int, int], cp_model.IntervalVar] = {}
        start_vars: Dict[int, cp_model.IntVar] = {}
        end_vars: Dict[int, cp_model.IntVar] = {}

        for task in self.tasks:
            start = model.NewIntVar(0, self.horizon, f"start_{task.index}")
            end = model.NewIntVar(0, self.horizon, f"end_{task.index}")
            start_vars[task.index] = start
            end_vars[task.index] = end

        drone_used = [model.NewBoolVar(f"drone_used_{k}") for k in range(self.max_drones)]
        makespan = model.NewIntVar(0, self.horizon, "makespan")

        for task in self.tasks:
            duration = int(round(task.duration * SECONDS_SCALE))
            assign_literals = []
            for k in range(self.max_drones):
                assign = model.NewBoolVar(f"assign_{task.index}_{k}")
                assignment[(task.index, k)] = assign
                interval = model.NewOptionalIntervalVar(
                    start_vars[task.index],
                    duration,
                    end_vars[task.index],
                    assign,
                    f"interval_{task.index}_{k}",
                )
                interval_vars[(task.index, k)] = interval
                model.Add(start_vars[task.index] >= 0).OnlyEnforceIf(assign)
                model.Add(end_vars[task.index] == start_vars[task.index] + duration).OnlyEnforceIf(assign)
                assign_literals.append(assign)
                model.Add(assign <= drone_used[k])
            model.Add(sum(assign_literals) == 1)
            model.Add(end_vars[task.index] <= makespan)

        for k in range(self.max_drones):
            intervals = [interval_vars[(task.index, k)] for task in self.tasks]
            if intervals:
                model.AddNoOverlap(intervals)

        for k in range(self.max_drones):
            model.Add(sum(assignment[(task.index, k)] for task in self.tasks) >= drone_used[k])

        total_drone_use = model.NewIntVar(0, self.max_drones, "total_drone_use")
        model.Add(total_drone_use == sum(drone_used))

        total_length = int(round(sum(task.length for task in self.tasks) * 1000))
        weight_drone = 1_000_000
        weight_makespan = 1_000
        weight_length = 1
        model.Minimize(weight_drone * total_drone_use + weight_makespan * makespan + weight_length * total_length)

        solver = cp_model.CpSolver()
        solver.parameters.max_time_in_seconds = 30
        solver.parameters.num_search_workers = 8
        status = solver.Solve(model)

        if status not in (cp_model.OPTIMAL, cp_model.FEASIBLE):
            raise RuntimeError("Optimizer failed to find a solution")

        assignments: Dict[int, List[Tuple[int, float]]] = {k: [] for k in range(self.max_drones)}
        for task in self.tasks:
            for k in range(self.max_drones):
                if solver.Value(assignment[(task.index, k)]):
                    start_val = solver.Value(start_vars[task.index]) / SECONDS_SCALE
                    assignments[k].append((task.index, start_val))
                    break

        optimized = []
        for drone_id, task_list in assignments.items():
            if not task_list:
                continue
            task_list.sort(key=lambda item: item[1])
            merged = merge_tours(drone_id, task_list, self.tasks)
            optimized.append(merged)

        makespan_value = solver.Value(makespan) / SECONDS_SCALE
        total_drone_value = solver.Value(total_drone_use)
        obj_value = solver.ObjectiveValue()
        return OptimizerResult(optimized, obj_value, int(total_drone_value), makespan_value)


def merge_tours(drone_id: int, schedule: List[Tuple[int, float]], tasks: List[TourTask]) -> dict:
    next_index = 0
    merged_waypoints: List[dict] = []
    merged_segments: List[dict] = []
    recharge_stops: List[int] = []
    total_turns = 0
    total_length = 0.0
    for job_index, start_time in schedule:
        task = tasks[job_index]
        tour = task.tour
        index_mapping: Dict[int, int] = {}
        sorted_waypoints = sorted(tour["waypoints"], key=lambda wp: wp["index"])
        for waypoint in sorted_waypoints:
            new_idx = next_index
            next_index += 1
            index_mapping[waypoint["index"]] = new_idx
            merged_waypoints.append(
                {
                    "index": new_idx,
                    "x": waypoint["x"],
                    "y": waypoint["y"],
                    "t": float(waypoint.get("t", 0.0)) + start_time,
                }
            )
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

    time_horizon = max((wp["t"] for wp in merged_waypoints), default=0.0)
    merged = {
        "drone_id": drone_id,
        "waypoints": merged_waypoints,
        "segments": merged_segments,
        "recharge_stops": sorted(set(recharge_stops)),
        "cost": {
            "time": time_horizon,
            "turns": total_turns,
            "length": total_length,
        },
    }
    return merged


def optimize_file(in_path: Path, out_path: Path, max_drones: int) -> OptimizerResult:
    data = io.read_json(in_path)
    optimizer = TourOptimizer(data, max_drones=max_drones)
    result = optimizer.solve()
    io.write_json(out_path, result.tours)
    return result


__all__ = ["TourOptimizer", "optimize_file", "OptimizerResult"]
