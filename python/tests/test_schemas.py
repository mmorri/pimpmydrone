"""Every JSON artefact the pipeline produces must validate against schema/."""

import json
from pathlib import Path

import jsonschema
import pytest

from fleet import io
from fleet.generate_map import GeneratorConfig, generate_workspace
from fleet.optimize import TourOptimizer
from fleet.simulate import simulate

SCHEMA_DIR = Path(__file__).resolve().parents[2] / "schema"


def schema(name: str) -> dict:
    return json.loads((SCHEMA_DIR / f"{name}.schema.json").read_text())


@pytest.fixture
def workspace():
    return generate_workspace(
        GeneratorConfig(seed=5, width=8, height=8, no_fly_count=1, obstacle_density=0.1)
    )


def test_generated_workspace_matches_schema(workspace) -> None:
    map_payload, params_payload = workspace
    jsonschema.validate(map_payload, schema("map"))
    jsonschema.validate(params_payload, schema("params"))


def test_metrics_match_schema(tmp_path: Path) -> None:
    map_payload = {
        "width": 2,
        "height": 1,
        "seed": 1,
        "obstacles": [],
        "no_fly": [],
        "depots": [{"x": 0, "y": 0}],
    }
    params = {
        "v": 1.0,
        "B": 50.0,
        "takeoff_cost": 1.0,
        "landing_cost": 1.0,
        "max_drones": 1,
    }
    tour = {
        "drone_id": 0,
        "waypoints": [
            {"index": 0, "x": 0, "y": 0, "t": 1.0},
            {"index": 1, "x": 1, "y": 0, "t": 2.0},
            {"index": 2, "x": 0, "y": 0, "t": 4.0},
        ],
        "segments": [
            {"from": 0, "to": 1, "length": 1, "turn_cost": 0, "battery_used": 1.0},
            {"from": 1, "to": 2, "length": 1, "turn_cost": 1, "battery_used": 2.0},
        ],
        "recharge_stops": [],
        "cost": {"time": 4.0, "turns": 1, "length": 2.0},
    }
    jsonschema.validate([tour], schema("tours"))
    optimized = TourOptimizer([tour], max_drones=1, time_limit=5).solve().tours
    jsonschema.validate(optimized, schema("tours"))

    paths = [tmp_path / n for n in ("map.json", "params.json", "tours.json")]
    for path, payload in zip(paths, (map_payload, params, optimized), strict=True):
        io.write_json(path, payload)
    metrics_path = tmp_path / "metrics.json"
    result = simulate(*paths, metrics_path)
    assert result.ok
    jsonschema.validate(io.read_json(metrics_path), schema("metrics"))
