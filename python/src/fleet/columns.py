"""Set-partitioning over generated sortie columns (column-generation style).

The Haskell planner still partitions and routes; this module then:

1. splits tours into recharge sorties,
2. generates a pool of same-depot drone columns (singleton + packed sequences),
3. solves a set-partitioning CP-SAT master that covers each sortie exactly once.

Full iterative dual pricing is future work; a rich static pool already lets the
optimizer choose multi-sortie packs instead of only atomic sortie intervals.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from itertools import combinations
from typing import Dict, List, Optional, Sequence, Tuple

from ortools.sat.python import cp_model

from .optimize import (
    SECONDS_SCALE,
    TourTask,
    merge_tours,
    tour_depot,
)


@dataclass(frozen=True)
class Column:
    """One drone's work: an ordered list of sortie task indices."""

    sortie_indices: Tuple[int, ...]
    duration: float
    depot: Tuple[int, int]

    @property
    def key(self) -> Tuple[int, ...]:
        return self.sortie_indices


def generate_columns(
    tasks: Sequence[TourTask],
    *,
    max_drones: int,
    max_combo: int = 4,
) -> List[Column]:
    """Build a feasible column pool covering every sortie.

    Always includes singletons (so the master is feasible). Adds same-depot
    packs: full depot load, LPT bin packs for 2..K bins, and small combinations.
    """

    if not tasks:
        return []

    by_depot: Dict[Tuple[int, int], List[TourTask]] = {}
    for task in tasks:
        by_depot.setdefault(task.depot, []).append(task)

    pool: Dict[Tuple[int, ...], Column] = {}

    def add(indices: Sequence[int], depot: Tuple[int, int]) -> None:
        if not indices:
            return
        ordered = tuple(indices)
        if ordered in pool:
            return
        duration = sum(tasks[i].duration for i in ordered)
        pool[ordered] = Column(ordered, duration, depot)

    for depot, group in by_depot.items():
        ids = [t.index for t in group]
        for i in ids:
            add([i], depot)

        # Longest-processing-time order for packing heuristics.
        lpt = sorted(group, key=lambda t: (-t.duration, t.index))
        lpt_ids = [t.index for t in lpt]
        add(lpt_ids, depot)

        n = len(lpt_ids)
        k_max = min(max_drones, n)
        for bins in range(2, k_max + 1):
            for packed in _lpt_pack(lpt, bins):
                add(packed, depot)

        # Small combinations among the longest sorties only (bound pool size).
        combo_pool = lpt_ids[: min(12, n)]
        limit = min(max_combo, len(combo_pool))
        for r in range(2, limit + 1):
            for combo in combinations(combo_pool, r):
                members = [tasks[i] for i in combo]
                ordered = [
                    t.index
                    for t in sorted(members, key=lambda t: (-t.duration, t.index))
                ]
                add(ordered, depot)

    return list(pool.values())


def _lpt_pack(tasks: Sequence[TourTask], bins: int) -> List[List[int]]:
    """Greedy LPT multifit: assign each task to the currently lightest bin."""

    if bins < 1:
        return []
    loads = [0.0] * bins
    packs: List[List[int]] = [[] for _ in range(bins)]
    for task in tasks:
        k = min(range(bins), key=lambda i: (loads[i], i))
        packs[k].append(task.index)
        loads[k] += task.duration
    return [p for p in packs if p]


def solve_set_partition(
    tasks: Sequence[TourTask],
    columns: Sequence[Column],
    *,
    max_drones: int,
    drone_cost: float = 1.0,
    deadline: Optional[float] = None,
    time_limit: float = 30.0,
) -> Tuple[List[Column], float, int, float]:
    """Pick columns covering each sortie once. Returns selected, obj, drones, makespan."""

    if not tasks:
        raise ValueError("No tasks to cover")
    if not columns:
        raise ValueError("No columns generated")
    if max_drones < 1:
        raise ValueError("max_drones must be at least 1")

    n_sorties = len(tasks)
    horizon = int(math.ceil(sum(t.duration for t in tasks) * SECONDS_SCALE)) + 1
    model = cp_model.CpModel()
    select = [model.NewBoolVar(f"col_{c}") for c in range(len(columns))]
    makespan = model.NewIntVar(0, horizon, "makespan")
    drone_count = model.NewIntVar(0, max_drones, "drone_count")
    model.Add(drone_count == sum(select))
    model.Add(drone_count <= max_drones)

    # Each sortie in exactly one selected column.
    for s in range(n_sorties):
        covering = [select[c] for c, col in enumerate(columns) if s in col.sortie_indices]
        if not covering:
            raise RuntimeError(f"Sortie {s} has no covering column")
        model.AddExactlyOne(covering)

    for c, col in enumerate(columns):
        dur = max(1, int(round(col.duration * SECONDS_SCALE)))
        model.Add(makespan >= dur).OnlyEnforceIf(select[c])

    if deadline is not None:
        model.Add(makespan <= int(math.floor(deadline * SECONDS_SCALE)))
        drone_weight = horizon + 1
    else:
        drone_weight = int(round(drone_cost * SECONDS_SCALE))
    model.Minimize(drone_weight * drone_count + makespan)

    solver = cp_model.CpSolver()
    solver.parameters.max_time_in_seconds = time_limit
    solver.parameters.num_search_workers = 8
    status = solver.Solve(model)
    if status not in (cp_model.OPTIMAL, cp_model.FEASIBLE):
        raise RuntimeError(
            "Column set-partition found no schedule"
            + (" within the deadline" if deadline is not None else "")
        )

    chosen = [columns[c] for c in range(len(columns)) if solver.Value(select[c])]
    # Prefer shorter makespan columns first for stable drone_id ordering.
    chosen.sort(key=lambda col: (-col.duration, col.depot, col.sortie_indices))
    return (
        chosen,
        float(solver.ObjectiveValue()),
        int(solver.Value(drone_count)),
        solver.Value(makespan) / SECONDS_SCALE,
    )


def columns_to_tours(selected: Sequence[Column], tasks: Sequence[TourTask]) -> List[dict]:
    """Materialise selected columns as merged drone tours."""

    tours: List[dict] = []
    for drone_id, col in enumerate(selected):
        schedule: List[Tuple[int, float]] = []
        t = 0.0
        for idx in col.sortie_indices:
            schedule.append((idx, t))
            t += tasks[idx].duration
        tours.append(merge_tours(drone_id, schedule, list(tasks)))
    return tours


def build_tasks_from_sorties(sorties: Sequence[dict]) -> List[TourTask]:
    return [
        TourTask(
            index=i,
            duration=_duration(tour),
            length=float(tour["cost"]["length"]),
            turns=int(tour["cost"].get("turns", 0)),
            depot=tour_depot(tour),
            tour=tour,
        )
        for i, tour in enumerate(sorties)
    ]


def _duration(tour: dict) -> float:
    cost_t = float(tour.get("cost", {}).get("time", 0.0))
    if cost_t > 0:
        return cost_t
    wps = tour.get("waypoints") or []
    if not wps:
        return 1.0
    return max(float(wp.get("t", 0.0)) for wp in wps) or 1.0


__all__ = [
    "Column",
    "generate_columns",
    "solve_set_partition",
    "columns_to_tours",
    "build_tasks_from_sorties",
]
