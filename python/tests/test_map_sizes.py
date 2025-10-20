from pathlib import Path

from fleet.generate_map import GeneratorConfig, generate_workspace, write_workspace
from fleet.map_utils import MapSpec


def test_generate_various_sizes_structure() -> None:
    cases = [
        {"w": 10, "h": 10, "density": 0.1},
        {"w": 100, "h": 100, "density": 0.1},
        {"w": 1000, "h": 1000, "density": 0.05},
    ]
    for i, case in enumerate(cases):
        config = GeneratorConfig(
            seed=1234 + i,
            width=case["w"],
            height=case["h"],
            obstacle_density=case["density"],
            no_fly_count=0,  # keep fast for large grids
            depot_count=1,
        )
        map_payload, params_payload = generate_workspace(config)

        assert map_payload["width"] == case["w"]
        assert map_payload["height"] == case["h"]
        assert "depots" in map_payload and len(map_payload["depots"]) == 1
        assert map_payload["params"]["max_drones"] >= 1

        spec = MapSpec.from_json(map_payload)
        blocked = spec.blocked_cells()
        free = spec.free_cells()
        # Sanity: at least one free cell and not fully blocked
        assert len(free) > 0
        assert len(free) + len(blocked) == case["w"] * case["h"]


def test_write_workspace_outputs(tmp_path: Path) -> None:
    config = GeneratorConfig(
        seed=999,
        width=100,
        height=100,
        obstacle_density=0.1,
        no_fly_count=0,
        depot_count=1,
    )
    map_path = tmp_path / "map.json"
    params_path = tmp_path / "params.json"
    m, p = write_workspace(map_path, params_path, config)
    assert map_path.exists()
    assert params_path.exists()
    assert m["width"] == 100 and m["height"] == 100
    assert p["max_drones"] == m["params"]["max_drones"]

