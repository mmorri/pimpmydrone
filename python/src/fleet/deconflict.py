"""Spatial deconfliction: detect and resolve same-cell / same-time clashes.

The optimizer only no-overlaps *per drone*. Multi-drone fleets can still share a
grid cell at the same simulated time. This module:

1. Builds a discrete occupancy timeline from tour waypoints.
2. Reports conflicts (two drones in one cell within a time tolerance).
3. Resolves them by delaying later drones' clocks until clear (or giving up).
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Iterable, Sequence

Cell = tuple[int, int]


@dataclass
class Conflict:
    cell: Cell
    time: float
    drone_a: int
    drone_b: int


@dataclass
class DeconflictResult:
    tours: list[dict[str, Any]]
    conflicts_before: list[Conflict]
    conflicts_after: list[Conflict]
    delays: dict[int, float]
    resolved: bool

    def as_dict(self) -> dict[str, Any]:
        return {
            "conflicts_before": len(self.conflicts_before),
            "conflicts_after": len(self.conflicts_after),
            "resolved": self.resolved,
            "delays_s": {str(k): v for k, v in sorted(self.delays.items())},
            "sample_before": [_conflict_json(c) for c in self.conflicts_before[:5]],
            "sample_after": [_conflict_json(c) for c in self.conflicts_after[:5]],
        }


def _conflict_json(c: Conflict) -> dict[str, Any]:
    return {
        "x": c.cell[0],
        "y": c.cell[1],
        "t": c.time,
        "drones": [c.drone_a, c.drone_b],
    }


def occupancy(
    tours: Sequence[dict[str, Any]],
    *,
    time_tol: float = 0.5,
) -> dict[tuple[Cell, int], list[int]]:
    """Map (cell, time_bucket) → drone ids present."""

    bucket = max(time_tol, 1e-6)
    table: dict[tuple[Cell, int], list[int]] = {}
    for tour in tours:
        drone = int(tour.get("drone_id", 0))
        for wp in tour.get("waypoints") or []:
            cell = (int(wp["x"]), int(wp["y"]))
            t_bucket = int(float(wp.get("t", 0.0)) // bucket)
            table.setdefault((cell, t_bucket), []).append(drone)
    return table


def find_conflicts(
    tours: Sequence[dict[str, Any]],
    *,
    time_tol: float = 0.5,
) -> list[Conflict]:
    """Return pairwise same-cell / same-bucket conflicts."""

    table = occupancy(tours, time_tol=time_tol)
    conflicts: list[Conflict] = []
    seen: set[tuple[int, int, Cell, int]] = set()
    for (cell, bucket), drones in table.items():
        unique = sorted(set(drones))
        if len(unique) < 2:
            continue
        t = bucket * time_tol
        for i, a in enumerate(unique):
            for b in unique[i + 1 :]:
                key = (a, b, cell, bucket)
                if key in seen:
                    continue
                seen.add(key)
                conflicts.append(Conflict(cell=cell, time=t, drone_a=a, drone_b=b))
    conflicts.sort(key=lambda c: (c.time, c.cell, c.drone_a, c.drone_b))
    return conflicts


def shift_tour(tour: dict[str, Any], delay: float) -> dict[str, Any]:
    """Return a copy of tour with all waypoint times increased by ``delay``."""

    if delay <= 0:
        return tour
    shifted = dict(tour)
    shifted["waypoints"] = [
        {**wp, "t": float(wp.get("t", 0.0)) + delay} for wp in tour.get("waypoints") or []
    ]
    cost = dict(tour.get("cost") or {})
    if "time" in cost:
        cost["time"] = float(cost["time"]) + delay
    shifted["cost"] = cost
    return shifted


def resolve_conflicts(
    tours: Sequence[dict[str, Any]],
    *,
    time_tol: float = 0.5,
    step: float = 1.0,
    max_delay: float = 10_000.0,
) -> DeconflictResult:
    """Stagger later drones until no same-cell time clashes remain.

    Strategy: keep drone 0 fixed; for each later drone in id order, delay its
    whole timeline in ``step`` increments until it no longer conflicts with
    already-frozen drones, or ``max_delay`` is hit.
    """

    if len(tours) <= 1:
        copied = [dict(t) for t in tours]
        return DeconflictResult(copied, [], [], {}, True)

    before = find_conflicts(tours, time_tol=time_tol)
    ordered = sorted((dict(t) for t in tours), key=lambda t: int(t.get("drone_id", 0)))
    delays: dict[int, float] = {}
    frozen: list[dict[str, Any]] = [ordered[0]]

    for tour in ordered[1:]:
        drone = int(tour.get("drone_id", 0))
        delay = 0.0
        candidate = tour
        while delay <= max_delay:
            candidate = shift_tour(tour, delay)
            trial = frozen + [candidate]
            hits = [
                c
                for c in find_conflicts(trial, time_tol=time_tol)
                if drone in (c.drone_a, c.drone_b)
            ]
            if not hits:
                break
            delay += step
        delays[drone] = delay
        frozen.append(candidate)

    after = find_conflicts(frozen, time_tol=time_tol)
    return DeconflictResult(
        tours=frozen,
        conflicts_before=before,
        conflicts_after=after,
        delays=delays,
        resolved=not after,
    )


def deconflict_tours(
    tours: Sequence[dict[str, Any]],
    *,
    time_tol: float = 0.5,
    step: float | None = None,
) -> DeconflictResult:
    """Public entry: resolve with a step derived from median segment duration."""

    if step is None:
        step = _default_step(tours)
    return resolve_conflicts(tours, time_tol=time_tol, step=step)


def _default_step(tours: Iterable[dict[str, Any]]) -> float:
    deltas: list[float] = []
    for tour in tours:
        wps = sorted(tour.get("waypoints") or [], key=lambda wp: wp["index"])
        for a, b in zip(wps, wps[1:], strict=False):
            dt = float(b.get("t", 0.0)) - float(a.get("t", 0.0))
            if dt > 1e-9:
                deltas.append(dt)
    if not deltas:
        return 1.0
    deltas.sort()
    return max(0.5, deltas[len(deltas) // 2])


__all__ = [
    "Conflict",
    "DeconflictResult",
    "find_conflicts",
    "resolve_conflicts",
    "deconflict_tours",
    "shift_tour",
]
