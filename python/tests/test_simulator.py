from pathlib import Path

from fleet import io
from fleet.simulate import check_battery, simulate

PARAMS = {
    "v": 1.0,
    "B": 100.0,
    "takeoff_cost": 5.0,
    "landing_cost": 5.0,
    "max_drones": 2,
}


def write_case(tmp_path: Path, map_payload: dict, tours: list[dict]) -> tuple:
    paths = (
        tmp_path / "map.json",
        tmp_path / "params.json",
        tmp_path / "tours.json",
        tmp_path / "metrics.json",
    )
    io.write_json(paths[0], map_payload)
    io.write_json(paths[1], PARAMS)
    io.write_json(paths[2], tours)
    return paths


def square_tour() -> dict:
    return {
        "drone_id": 0,
        "waypoints": [
            {"index": 0, "x": 0, "y": 0, "t": 0.0},
            {"index": 1, "x": 0, "y": 1, "t": 1.0},
            {"index": 2, "x": 1, "y": 1, "t": 2.0},
            {"index": 3, "x": 1, "y": 0, "t": 3.0},
            {"index": 4, "x": 0, "y": 0, "t": 4.0},
        ],
        "segments": [
            {"from": 0, "to": 1, "length": 1.0, "turn_cost": 0, "battery_used": 1.0},
            {"from": 1, "to": 2, "length": 1.0, "turn_cost": 1, "battery_used": 2.0},
            {"from": 2, "to": 3, "length": 1.0, "turn_cost": 1, "battery_used": 2.0},
            {"from": 3, "to": 4, "length": 1.0, "turn_cost": 1, "battery_used": 2.0},
        ],
        "recharge_stops": [],
        "cost": {"time": 4.0, "turns": 3, "length": 4.0},
    }


def test_simulator_metrics(tmp_path: Path) -> None:
    map_payload = {
        "width": 2,
        "height": 2,
        "seed": 1,
        "obstacles": [],
        "no_fly": [],
        "depots": [{"x": 0, "y": 0}],
        "params": PARAMS,
    }
    result = simulate(*write_case(tmp_path, map_payload, [square_tour()]))
    assert result.ok
    assert result.metrics["coverage"]["percentage"] == 100.0
    assert result.metrics["drones"]["used"] == 1
    assert result.metrics["checks"]["problems"] == []


def test_unreachable_pockets_do_not_count_as_gaps(tmp_path: Path) -> None:
    # A wall at x=2 isolates the column x=3; it must not count as uncovered.
    map_payload = {
        "width": 4,
        "height": 2,
        "seed": 1,
        "obstacles": [{"x": 2, "y": 0}, {"x": 2, "y": 1}],
        "no_fly": [],
        "depots": [{"x": 0, "y": 0}],
        "params": PARAMS,
    }
    result = simulate(*write_case(tmp_path, map_payload, [square_tour()]))
    assert result.coverage_ok
    assert result.metrics["coverage"]["unreachable_cells"] == 2
    assert result.metrics["coverage"]["reachable_cells"] == 4


def test_simulator_flags_jumps_and_gaps(tmp_path: Path) -> None:
    map_payload = {
        "width": 3,
        "height": 1,
        "seed": 1,
        "obstacles": [],
        "no_fly": [],
        "depots": [{"x": 0, "y": 0}],
        "params": PARAMS,
    }
    tour = square_tour()
    tour["waypoints"] = [
        {"index": 0, "x": 0, "y": 0, "t": 0.0},
        {"index": 1, "x": 2, "y": 0, "t": 1.0},
    ]
    tour["segments"] = tour["segments"][:1]
    result = simulate(*write_case(tmp_path, map_payload, [tour]))
    assert not result.continuous
    assert not result.coverage_ok
    assert result.metrics["coverage"]["uncovered_cells"] == [{"x": 1, "y": 0}]


def test_check_battery_respects_recharge_stops() -> None:
    params = {"B": 10.0, "takeoff_cost": 1.0, "landing_cost": 1.0}
    segments = [
        {"from": i, "to": i + 1, "length": 1.0, "turn_cost": 0, "battery_used": 4.0}
        for i in range(4)
    ]
    tour = {"segments": segments, "recharge_stops": []}
    assert check_battery(tour, params) is not None
    tour["recharge_stops"] = [2]
    assert check_battery(tour, params) is None
