"""Set-partitioning over generated sortie columns with iterative dual pricing.

The Haskell planner still partitions and routes; this module then:

1. splits tours into recharge sorties,
2. seeds a same-depot column pool (region packs + singletons + LPT packs +
   small combos),
3. enriches the pool by pricing against LP duals of the restricted master,
4. solves a set-partitioning CP-SAT master so each sortie is covered once.

The pricing RMP uses a separable column cost ``drone_weight + duration`` so
cover duals match the reduced-cost proxy ``w + duration - π``. The final
CP-SAT master keeps the true integer makespan objective.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from itertools import combinations
from typing import Dict, List, Optional, Sequence, Tuple

from ortools.linear_solver import pywraplp
from ortools.sat.python import cp_model

from .optimize import (
    SECONDS_SCALE,
    TourTask,
    merge_tours,
    tour_depot,
)

_PRICING_EPS = 1e-3


@dataclass(frozen=True)
class Column:
    """One drone's work: an ordered list of sortie task indices."""

    sortie_indices: Tuple[int, ...]
    duration: float
    depot: Tuple[int, int]

    @property
    def key(self) -> Tuple[int, ...]:
        return self.sortie_indices


@dataclass
class ColumnGenResult:
    selected: List[Column]
    objective_value: float
    drone_count: int
    makespan: float
    columns_generated: int
    columns_selected: int
    pricing_rounds: int
    columns_added_by_pricing: int


def generate_columns(
    tasks: Sequence[TourTask],
    *,
    max_drones: int,
    max_combo: int = 4,
) -> List[Column]:
    """Build a feasible column pool covering every sortie.

    Always includes singletons (so the master is feasible). Adds same-depot
    packs: planner region groups, full depot load, LPT bin packs for 2..K bins,
    and small combinations.
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

    # Fold the Haskell partition into the pool: one column per region.
    by_region: Dict[int, List[TourTask]] = {}
    for task in tasks:
        if task.region_id is None:
            continue
        by_region.setdefault(task.region_id, []).append(task)
    for group in by_region.values():
        if len(group) < 2:
            continue
        depot = group[0].depot
        if any(t.depot != depot for t in group):
            continue
        ordered = [
            t.index for t in sorted(group, key=lambda t: (-t.duration, t.index))
        ]
        add(ordered, depot)

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


def _drone_weight(
    tasks: Sequence[TourTask],
    drone_cost: float,
    deadline: Optional[float],
) -> float:
    horizon = sum(t.duration for t in tasks)
    if deadline is not None:
        return horizon + 1.0
    return float(drone_cost)


@dataclass(frozen=True)
class RmpDuals:
    """Cover duals and the drone-cap dual from the pricing RMP."""

    cover: List[float]
    drone: float = 0.0


def solve_rmp_lp_duals(
    tasks: Sequence[TourTask],
    columns: Sequence[Column],
    *,
    max_drones: int,
    drone_cost: float = 1.0,
    deadline: Optional[float] = None,
) -> Optional[List[float]]:
    """LP duals on sortie-cover constraints of the restricted master.

    If the drone cap makes the RMP infeasible (e.g. singleton-only pool),
    retries with ``max_drones = n_sorties`` so pricing can still enrich.
    """

    result = solve_rmp_lp_duals_full(
        tasks,
        columns,
        max_drones=max_drones,
        drone_cost=drone_cost,
        deadline=deadline,
    )
    return None if result is None else result.cover


def solve_rmp_lp_duals_full(
    tasks: Sequence[TourTask],
    columns: Sequence[Column],
    *,
    max_drones: int,
    drone_cost: float = 1.0,
    deadline: Optional[float] = None,
) -> Optional[RmpDuals]:
    """Cover + drone-cap duals from the separable-cost pricing RMP."""

    if not tasks or not columns:
        return None
    caps = [max_drones]
    if max_drones < len(tasks):
        caps.append(len(tasks))
    for cap in caps:
        duals = _solve_rmp_lp_duals_once(
            tasks,
            columns,
            max_drones=cap,
            drone_cost=drone_cost,
            deadline=deadline,
        )
        if duals is not None:
            return duals
    return None


def _solve_rmp_lp_duals_once(
    tasks: Sequence[TourTask],
    columns: Sequence[Column],
    *,
    max_drones: int,
    drone_cost: float,
    deadline: Optional[float],
) -> Optional[RmpDuals]:
    solver = pywraplp.Solver.CreateSolver("GLOP")
    if solver is None:
        return None

    weight = _drone_weight(tasks, drone_cost, deadline)
    x = [solver.NumVar(0.0, 1.0, f"x_{c}") for c in range(len(columns))]

    cover = []
    for s in range(len(tasks)):
        ct = solver.Constraint(1.0, 1.0, f"cover_{s}")
        for c, col in enumerate(columns):
            if s in col.sortie_indices:
                ct.SetCoefficient(x[c], 1.0)
        cover.append(ct)

    # sum x <= max_drones  → dual ≤ 0 in the usual min-primal convention.
    drones = solver.Constraint(-solver.infinity(), float(max_drones), "drones")
    for var in x:
        drones.SetCoefficient(var, 1.0)

    objective = solver.Objective()
    for c, col in enumerate(columns):
        # Separable proxy matching price_columns: cost = w + duration.
        objective.SetCoefficient(x[c], weight + col.duration)
    objective.SetMinimization()

    status = solver.Solve()
    if status != pywraplp.Solver.OPTIMAL:
        return None
    return RmpDuals(
        cover=[ct.dual_value() for ct in cover],
        drone=float(drones.dual_value()),
    )


def price_columns(
    tasks: Sequence[TourTask],
    duals: Sequence[float],
    *,
    drone_weight: float,
    drone_dual: float = 0.0,
    existing: Optional[Dict[Tuple[int, ...], Column]] = None,
    max_new: int = 32,
) -> List[Column]:
    """Find same-depot columns with negative reduced cost under the LP duals."""

    if len(duals) != len(tasks):
        raise ValueError("duals must align with tasks")
    existing = existing or {}
    by_depot: Dict[Tuple[int, int], List[TourTask]] = {}
    for task in tasks:
        by_depot.setdefault(task.depot, []).append(task)

    candidates: List[Tuple[float, Column]] = []
    for depot, group in by_depot.items():
        for rc, col in _price_depot(group, duals, drone_weight, drone_dual):
            if col.key in existing:
                continue
            if rc < -_PRICING_EPS:
                candidates.append((rc, col))

    candidates.sort(key=lambda item: item[0])
    return [col for _, col in candidates[:max_new]]


def _price_depot(
    group: Sequence[TourTask],
    duals: Sequence[float],
    drone_weight: float,
    drone_dual: float = 0.0,
) -> List[Tuple[float, Column]]:
    """Enumerate (or greedily search) improving subsets for one depot."""

    n = len(group)
    if n == 0:
        return []
    depot = group[0].depot
    found: List[Tuple[float, Column]] = []

    def consider(indices: Sequence[int]) -> None:
        if not indices:
            return
        ordered = tuple(sorted(indices, key=lambda i: (-_task_duration(group, i), i)))
        duration = sum(_task_duration(group, i) for i in ordered)
        pi = sum(duals[i] for i in ordered)
        # Primal min (w+d)x; cover Ax=1 (π); drones 1·x ≤ K (μ).
        # Reduced cost: (w + d) - π·a_c - μ.
        rc = drone_weight + duration - pi - drone_dual
        found.append((rc, Column(ordered, duration, depot)))

    if n <= 16:
        for mask in range(1, 1 << n):
            idxs = [group[i].index for i in range(n) if mask & (1 << i)]
            consider(idxs)
    else:
        # Density greedy + positive (π - d) pack + leave-one-out around it.
        ranked = sorted(
            group,
            key=lambda t: (-(duals[t.index] / max(t.duration, 1e-9)), t.index),
        )
        greedy: List[int] = []
        for task in ranked:
            greedy.append(task.index)
            consider(greedy)
        positive = [t.index for t in group if duals[t.index] > t.duration]
        consider(positive)
        for drop in list(positive):
            consider([i for i in positive if i != drop])

    # Dedup by key, keep best (lowest) reduced cost.
    best: Dict[Tuple[int, ...], Tuple[float, Column]] = {}
    for rc, col in found:
        prev = best.get(col.key)
        if prev is None or rc < prev[0]:
            best[col.key] = (rc, col)
    return list(best.values())


def _task_duration(group: Sequence[TourTask], index: int) -> float:
    for task in group:
        if task.index == index:
            return task.duration
    raise KeyError(index)


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
    # Prefer longer columns first for stable drone_id ordering.
    chosen.sort(key=lambda col: (-col.duration, col.depot, col.sortie_indices))
    return (
        chosen,
        float(solver.ObjectiveValue()),
        int(solver.Value(drone_count)),
        solver.Value(makespan) / SECONDS_SCALE,
    )


def solve_column_generation(
    tasks: Sequence[TourTask],
    *,
    max_drones: int,
    drone_cost: float = 1.0,
    deadline: Optional[float] = None,
    time_limit: float = 30.0,
    max_pricing_rounds: int = 8,
) -> ColumnGenResult:
    """Seed pool → dual pricing loop → integer set-partition master."""

    seed = generate_columns(tasks, max_drones=max_drones)
    pool: Dict[Tuple[int, ...], Column] = {col.key: col for col in seed}
    weight = _drone_weight(tasks, drone_cost, deadline)
    pricing_added = 0
    rounds_done = 0

    for round_idx in range(max_pricing_rounds):
        rounds_done = round_idx + 1
        duals = solve_rmp_lp_duals_full(
            tasks,
            list(pool.values()),
            max_drones=max_drones,
            drone_cost=drone_cost,
            deadline=deadline,
        )
        if duals is None:
            break
        fresh = price_columns(
            tasks,
            duals.cover,
            drone_weight=weight,
            drone_dual=duals.drone,
            existing=pool,
        )
        if not fresh:
            break
        for col in fresh:
            if col.key not in pool:
                pool[col.key] = col
                pricing_added += 1

    selected, objective, drone_count, makespan = solve_set_partition(
        tasks,
        list(pool.values()),
        max_drones=max_drones,
        drone_cost=drone_cost,
        deadline=deadline,
        time_limit=time_limit,
    )
    return ColumnGenResult(
        selected=selected,
        objective_value=objective,
        drone_count=drone_count,
        makespan=makespan,
        columns_generated=len(pool),
        columns_selected=len(selected),
        pricing_rounds=rounds_done,
        columns_added_by_pricing=pricing_added,
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
    from .optimize import tour_region_id

    return [
        TourTask(
            index=i,
            duration=_duration(tour),
            length=float(tour["cost"]["length"]),
            turns=int(tour["cost"].get("turns", 0)),
            depot=tour_depot(tour),
            tour=tour,
            region_id=tour_region_id(tour),
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
    "ColumnGenResult",
    "RmpDuals",
    "generate_columns",
    "solve_rmp_lp_duals",
    "solve_rmp_lp_duals_full",
    "price_columns",
    "solve_set_partition",
    "solve_column_generation",
    "columns_to_tours",
    "build_tasks_from_sorties",
]
