"""Column pool + set-partitioning schedule mode with dual pricing."""

from __future__ import annotations

from fleet.columns import (
    Column,
    generate_columns,
    price_columns,
    solve_column_generation,
    solve_rmp_lp_duals,
    solve_set_partition,
)
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


def test_generate_columns_seeds_planner_regions():
    """Same-depot sorties from one planner region become a seed column."""

    tours = [make_tour(i, 10.0) for i in range(4)]
    tasks = [
        TourTask(
            i,
            10.0,
            2.0,
            1,
            tour_depot(t),
            t,
            region_id=0 if i < 3 else 1,
        )
        for i, t in enumerate(tours)
    ]
    columns = generate_columns(tasks, max_drones=2)
    region_pack = {0, 1, 2}
    assert any(set(c.sortie_indices) == region_pack for c in columns)


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


def test_lp_duals_and_pricing_from_singletons():
    tours = [make_tour(i, float(4 + i)) for i in range(4)]
    tasks = [
        TourTask(i, float(t["cost"]["time"]), 2.0, 1, tour_depot(t), t)
        for i, t in enumerate(tours)
    ]
    singletons = [
        Column((t.index,), t.duration, t.depot) for t in tasks
    ]
    duals = solve_rmp_lp_duals(
        tasks, singletons, max_drones=2, drone_cost=1.0
    )
    assert duals is not None
    assert len(duals) == 4
    priced = price_columns(
        tasks, duals, drone_weight=1.0, existing={c.key: c for c in singletons}
    )
    # With only singletons in the RMP, multi-sortie packs should price in.
    assert any(len(c.sortie_indices) > 1 for c in priced)


def test_column_generation_runs_pricing_loop():
    tours = [make_tour(i, 10.0) for i in range(3)]
    tasks = [
        TourTask(i, 10.0, 2.0, 1, tour_depot(t), t) for i, t in enumerate(tours)
    ]
    result = solve_column_generation(tasks, max_drones=3, time_limit=5)
    assert result.pricing_rounds >= 1
    assert result.columns_generated >= 3
    assert result.drone_count == 3
    assert abs(result.makespan - 10.0) < 1e-6


def test_columns_mode_reports_pricing_stats():
    tours = [make_tour(i, 10.0) for i in range(3)]
    result = TourOptimizer(
        tours, max_drones=3, time_limit=5, mode="columns", deconflict=False
    ).solve()
    assert result.pricing_rounds is not None and result.pricing_rounds >= 1
    assert result.columns_added_by_pricing is not None
