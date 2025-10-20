from pathlib import Path

from fleet.simulate import simulate
from fleet import io


def test_simulator_metrics(tmp_path: Path) -> None:
    map_path = tmp_path / "map.json"
    params_path = tmp_path / "params.json"
    tours_path = tmp_path / "tours.json"
    metrics_path = tmp_path / "metrics.json"

    map_payload = {
        "width": 2,
        "height": 2,
        "seed": 1,
        "obstacles": [],
        "no_fly": [],
        "depots": [{"x": 0, "y": 0}],
        "params": {
            "v": 1.0,
            "B": 100.0,
            "takeoff_cost": 5.0,
            "landing_cost": 5.0,
            "max_drones": 2,
        },
    }
    params_payload = map_payload["params"]
    tour = {
        "drone_id": 0,
        "waypoints": [
            {"index": 0, "x": 0, "y": 0, "t": 0.0},
            {"index": 1, "x": 0, "y": 1, "t": 1.0},
            {"index": 2, "x": 1, "y": 1, "t": 2.0},
            {"index": 3, "x": 1, "y": 0, "t": 3.0},
        ],
        "segments": [
            {"from": 0, "to": 1, "length": 1.0, "turn_cost": 0, "battery_used": 1.0},
            {"from": 1, "to": 2, "length": 1.0, "turn_cost": 1, "battery_used": 1.0},
            {"from": 2, "to": 3, "length": 1.0, "turn_cost": 1, "battery_used": 1.0},
        ],
        "recharge_stops": [],
        "cost": {"time": 3.0, "turns": 2, "length": 3.0},
    }

    io.write_json(map_path, map_payload)
    io.write_json(params_path, params_payload)
    io.write_json(tours_path, [tour])

    result = simulate(map_path, params_path, tours_path, metrics_path)
    assert result.coverage_ok is True
    assert result.collision_free is True
    assert result.metrics["coverage"]["percentage"] == 100.0
    assert result.metrics["drones"]["used"] == 1
