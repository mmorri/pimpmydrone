"""Deconfliction, sortie splitting, and sensor inspection metrics."""

from __future__ import annotations

from fleet.deconflict import deconflict_tours, find_conflicts, shift_tour
from fleet.optimize import split_into_sorties
from fleet.sensor import SensorSpec, cells_under_footprint, inspection_coverage


def _tour(drone_id: int, cells: list[tuple[int, int]], t0: float = 0.0) -> dict:
    wps = [
        {"index": i, "x": x, "y": y, "t": t0 + float(i)}
        for i, (x, y) in enumerate(cells)
    ]
    segs = [
        {
            "from": i,
            "to": i + 1,
            "length": 1.0,
            "turn_cost": 0.0,
            "battery_used": 1.0,
        }
        for i in range(len(cells) - 1)
    ]
    return {
        "drone_id": drone_id,
        "waypoints": wps,
        "segments": segs,
        "recharge_stops": [],
        "cost": {"time": float(len(cells) - 1), "turns": 0, "length": float(len(cells) - 1)},
    }


def test_find_conflicts_detects_same_cell_time():
    a = _tour(0, [(0, 0), (1, 0), (2, 0)])  # (2,0) at t=2
    b = _tour(1, [(3, 0), (2, 0), (1, 0)], t0=1.0)  # (2,0) at t=2
    conflicts = find_conflicts([a, b], time_tol=1.0)
    assert conflicts
    assert any(c.cell == (2, 0) for c in conflicts)


def test_deconflict_staggers_until_clear():
    a = _tour(0, [(0, 0), (1, 0), (2, 0)])
    b = _tour(1, [(3, 0), (2, 0), (1, 0)], t0=1.0)
    result = deconflict_tours([a, b], time_tol=1.0, step=1.0)
    assert result.resolved
    assert not result.conflicts_after
    assert result.delays.get(1, 0) > 0


def test_shift_tour_preserves_geometry():
    tour = _tour(0, [(0, 0), (1, 0)])
    shifted = shift_tour(tour, 5.0)
    assert shifted["waypoints"][0]["t"] == 5.0
    assert shifted["waypoints"][0]["x"] == 0


def test_split_into_sorties_at_recharge():
    tour = _tour(0, [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0)])
    tour["recharge_stops"] = [2]
    sorties = split_into_sorties([tour])
    assert len(sorties) == 2
    assert sorties[0]["waypoints"][-1]["index"] == len(sorties[0]["waypoints"]) - 1


def test_sensor_footprint_and_inspection():
    sensor = SensorSpec(altitude_m=40.0, fov_h_deg=60.0, cell_size_m=5.0)
    assert sensor.footprint_width_m > 0
    assert sensor.radius_cells >= 1
    covered = cells_under_footprint((5, 5), 1.5)
    assert (5, 5) in covered
    assert (6, 5) in covered
    tour = _tour(0, [(5, 5), (6, 5), (7, 5)])
    expected = {(x, 5) for x in range(4, 9)}
    report = inspection_coverage([tour], expected, sensor)
    assert report["imaged_cells"] >= 3
    assert report["percentage"] > 0
