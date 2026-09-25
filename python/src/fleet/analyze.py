"""Quality analysis of planned tours.

The simulator answers "is this plan valid?"; this module answers "is it any
good?". Two ratios drive almost every planning decision in the project:

``commute_share``
    Fraction of flight spent travelling between a depot and the part of the
    region the drone is actually covering. It is pure overhead paid once per
    sortie, so it grows with battery pressure and with the distance from the
    depot to the region.

``steps_per_cell``
    Total flown steps divided by the number of distinct cells covered. A
    perfect single sweep of a region approaches 1.0; a spanning-tree walk that
    traverses every edge twice approaches 2.0 before commuting is counted.
"""

from __future__ import annotations

from dataclasses import asdict, dataclass
from typing import Dict, List, Sequence, Tuple

Cell = Tuple[int, int]


@dataclass(frozen=True)
class TourStats:
    """Flight breakdown of a single tour."""

    drone_id: int
    sorties: int
    steps: int
    commute_steps: int
    in_region_steps: int
    covered_cells: int
    revisit_steps: int


@dataclass(frozen=True)
class PlanStats:
    """Flight breakdown of a whole plan."""

    sorties: int
    steps: int
    commute_steps: int
    in_region_steps: int
    covered_cells: int
    revisit_steps: int
    commute_share: float
    steps_per_cell: float
    per_drone: List[Dict]

    def as_dict(self) -> dict:
        data = asdict(self)
        data["per_drone"] = list(self.per_drone)
        return data


def split_sorties(tour: dict) -> List[List[Cell]]:
    """Split a tour into sorties at its recharge stops.

    A recharge stop is a waypoint index where the drone lands and takes off
    again, so the cell it happened on belongs to both the sortie that ends
    there and the one that starts there.
    """

    stops = set(tour.get("recharge_stops", []))
    waypoints = sorted(tour.get("waypoints", []), key=lambda wp: wp["index"])
    sorties: List[List[Cell]] = []
    current: List[Cell] = []
    for waypoint in waypoints:
        current.append((int(waypoint["x"]), int(waypoint["y"])))
        if waypoint["index"] in stops and len(current) > 1:
            sorties.append(current)
            current = [current[-1]]
    if len(current) > 1 or not sorties:
        sorties.append(current)
    return sorties


def analyze_tours(tours: Sequence[dict]) -> PlanStats:
    """Break the flight of every tour down into commuting and useful work.

    Within one sortie, the steps before the first cell the plan had not seen
    yet and the steps after the last such cell are commuting; what lies
    between them is work in the region. Cells are counted as seen across the
    whole plan and in flight order, so a second drone crossing ground that a
    first one already covered is charged as commuting.
    """

    seen: set[Cell] = set()
    per_drone: List[TourStats] = []

    for position, tour in enumerate(tours):
        drone_id = int(tour.get("drone_id", position))
        sorties = split_sorties(tour)
        commute = 0
        in_region = 0
        revisits = 0
        covered = 0
        for sortie in sorties:
            fresh = [i for i, cell in enumerate(sortie) if cell not in seen]
            steps = len(sortie) - 1
            if fresh:
                commute += fresh[0] + (len(sortie) - 1 - fresh[-1])
                in_region += fresh[-1] - fresh[0]
                covered += len(set(sortie[fresh[0] : fresh[-1] + 1]) - seen)
            else:
                commute += steps
            revisits += sum(1 for cell in sortie[1:] if cell in seen)
            seen.update(sortie)
        per_drone.append(
            TourStats(
                drone_id=drone_id,
                sorties=len(sorties),
                steps=commute + in_region,
                commute_steps=commute,
                in_region_steps=in_region,
                covered_cells=covered,
                revisit_steps=revisits,
            )
        )

    steps = sum(stat.steps for stat in per_drone)
    commute_steps = sum(stat.commute_steps for stat in per_drone)
    covered_cells = len(seen)
    return PlanStats(
        sorties=sum(stat.sorties for stat in per_drone),
        steps=steps,
        commute_steps=commute_steps,
        in_region_steps=sum(stat.in_region_steps for stat in per_drone),
        covered_cells=covered_cells,
        revisit_steps=sum(stat.revisit_steps for stat in per_drone),
        commute_share=commute_steps / steps if steps else 0.0,
        steps_per_cell=steps / covered_cells if covered_cells else 0.0,
        per_drone=[asdict(stat) for stat in per_drone],
    )


__all__ = ["PlanStats", "TourStats", "analyze_tours", "split_sorties"]
