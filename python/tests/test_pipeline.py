"""End-to-end run through the Haskell binaries (skipped when they are absent)."""

from pathlib import Path

import pytest

from fleet import cli, io
from fleet.generate_map import GeneratorConfig, write_workspace
from fleet.optimize import optimize_file
from fleet.simulate import simulate

BINARIES = [
    cli.HASKELL_BIN / "coverage-partitioner",
    cli.HASKELL_BIN / "coverage-planner",
]

pytestmark = pytest.mark.skipif(
    not all(b.exists() for b in BINARIES), reason="Haskell binaries not built"
)


@pytest.mark.parametrize("algorithm", ["stc", "boustro"])
@pytest.mark.parametrize("partition", ["region", "strip"])
def test_pipeline_end_to_end(tmp_path: Path, algorithm: str, partition: str) -> None:
    config = GeneratorConfig(
        seed=10,
        width=12,
        height=12,
        obstacle_density=0.15,
        no_fly_count=1,
        depot_count=1,
        battery=60.0,
        takeoff_cost=2.0,
        landing_cost=2.0,
        max_drones=3,
    )
    map_path, params_path = tmp_path / "map.json", tmp_path / "params.json"
    write_workspace(map_path, params_path, config)
    tours_path = cli.plan(
        map_path, params_path, tmp_path / "tours.json", algorithm, partition
    )
    optimized = tmp_path / "tours_optimized.json"
    optimize_file(tours_path, optimized, max_drones=config.max_drones, time_limit=5)
    metrics_path = tmp_path / "metrics.json"
    result = simulate(map_path, params_path, optimized, metrics_path)
    assert result.problems == []
    assert result.ok
    metrics = io.read_json(metrics_path)
    assert metrics["coverage"]["percentage"] == 100.0
    assert 1 <= metrics["drones"]["used"] <= config.max_drones


def test_pipeline_with_two_depots(tmp_path: Path) -> None:
    config = GeneratorConfig(
        seed=21,
        width=15,
        height=10,
        obstacle_density=0.1,
        no_fly_count=0,
        depot_count=2,
    )
    map_path, params_path = tmp_path / "map.json", tmp_path / "params.json"
    write_workspace(map_path, params_path, config)
    tours_path = cli.plan(map_path, params_path, tmp_path / "tours.json")
    optimized = tmp_path / "tours_optimized.json"
    optimize_file(tours_path, optimized, max_drones=config.max_drones, time_limit=5)
    result = simulate(map_path, params_path, optimized, tmp_path / "metrics.json")
    assert result.ok, result.problems
