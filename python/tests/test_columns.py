"""Column pool + set-partitioning schedule mode."""

from __future__ import annotations

from fleet.columns import generate_columns, solve_set_partition
from fleet.optimize import TourOptimizer, TourTask, tour_depot


def make_tour(drone_id: int, duration: float, depot=(0, 0)) -> dict:
    x, y = depot
    return {
        "drone_id": drone_id,
        "waypoints": [
            {"index": 0, "x": x, "y": y, "t": 0.0},
            {"index": 1, "x": x + 1, "y": y, "t": duration / 2},
            {"index": 2, "x": x, "y": y, "t": duration},
        ],
        "segments": [
            {"from": 0, "to": 1, "length": 1, "turn_cost": 0, "battery_used": 1.0},
            {"from": 1, "to": 2, "length": 1, "turn_cost": 1, "battery_used": 2.0},
        ],
        "recharge_stops": [],
        "cost": {"time": duration, "turns": 1, "length": 2.0},
    }


def test_columns_mode_respects_drone_limit():
    tours = [make_tour(i, d) for i, d in enumerate([10.0, 8.0, 6.0])]
    result = TourOptimizer(
        tours, max_drones=2, time_limit=5, mode="columns", deconflict=False
    ).solve()
    assert result.drone_count <= 2
    assert result.mode == "columns"
    assert result.columns_generated and result.columns_generated >= 3
    assert result.columns_selected == result.drone_count


def test_columns_mode_minimises_makespan_with_enough_drones():
    tours = [make_tour(i, 10.0) for i in range(3)]
    result = TourOptimizer(
        tours, max_drones=3, time_limit=5, mode="columns", deconflict=False
    ).solve()
    assert result.drone_count == 3
    assert abs(result.makespan - 10.0) < 1e-6


def test_columns_mode_packs_under_high_drone_cost():
    tours = [make_tour(i, 10.0) for i in range(3)]
    result = TourOptimizer(
        tours,
        max_drones=3,
        drone_cost=100.0,
        time_limit=5,
        mode="columns",
        deconflict=False,
    ).solve()
    assert result.drone_count == 1
    assert abs(result.makespan - 30.0) < 1e-6


def test_columns_mode_keeps_depots_apart():
    tours = [make_tour(0, 10.0, depot=(0, 0)), make_tour(1, 10.0, depot=(5, 5))]
    result = TourOptimizer(
        tours,
        max_drones=2,
        drone_cost=1e6,
        time_limit=5,
        mode="columns",
        deconflict=False,
    ).solve()
    assert result.drone_count == 2


def test_generate_columns_covers_every_sortie_as_singleton():
    tours = [make_tour(i, float(5 + i)) for i in range(4)]
    tasks = [
        TourTask(i, float(t["cost"]["time"]), 2.0, 1, tour_depot(t), t)
        for i, t in enumerate(tours)
    ]
    columns = generate_columns(tasks, max_drones=2)
    covered = {i for col in columns for i in col.sortie_indices}
    assert covered == {0, 1, 2, 3}
    singletons = [c for c in columns if len(c.sortie_indices) == 1]
    assert len(singletons) == 4


def test_set_partition_covers_exactly_once():
    tours = [make_tour(i, 10.0) for i in range(3)]
    tasks = [
        TourTask(i, 10.0, 2.0, 1, tour_depot(t), t) for i, t in enumerate(tours)
    ]
    columns = generate_columns(tasks, max_drones=3)
    selected, _, drones, makespan = solve_set_partition(
        tasks, columns, max_drones=3, time_limit=5
    )
    seen: list[int] = []
    for col in selected:
        seen.extend(col.sortie_indices)
    assert sorted(seen) == [0, 1, 2]
    assert drones == 3
    assert abs(makespan - 10.0) < 1e-6
