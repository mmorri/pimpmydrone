"""Golden tests over the planner's output.

The planner is deterministic, so the flight it produces for a fixed workspace
is a stable fingerprint of planning quality. These tests pin that fingerprint:
a change that makes plans worse (or better) fails here and has to be
acknowledged by refreshing the fixture, which shows the delta in the diff.

Refresh after an intentional change with::

    FLEET_UPDATE_GOLDEN=1 uv run pytest tests/test_regression.py
"""

from __future__ import annotations

import json
import os
from pathlib import Path

import pytest

from fleet import cli, io
from fleet.analyze import analyze_tours
from fleet.generate_map import GeneratorConfig, write_workspace

GOLDEN = Path(__file__).parent / "golden" / "planner.json"
BINARIES = [
    cli.HASKELL_BIN / "coverage-partitioner",
    cli.HASKELL_BIN / "coverage-planner",
]

pytestmark = pytest.mark.skipif(
    not all(binary.exists() for binary in BINARIES),
    reason="Haskell binaries not built",
)

# (name, config overrides, algorithm, partition)
CASES = [
    ("open", dict(width=20, height=20, obstacle_density=0.0, no_fly_count=0), "stc"),
    (
        "open",
        dict(width=20, height=20, obstacle_density=0.0, no_fly_count=0),
        "boustro",
    ),
    (
        "cluttered",
        dict(width=20, height=20, obstacle_density=0.2, no_fly_count=1),
        "stc",
    ),
    (
        "cluttered",
        dict(width=20, height=20, obstacle_density=0.2, no_fly_count=1),
        "boustro",
    ),
    ("tight", dict(width=20, height=20, obstacle_density=0.1, battery=60.0), "boustro"),
]

TRACKED = ("sorties", "steps", "commute_steps", "in_region_steps", "covered_cells")


def fingerprint(tmp_path: Path, overrides: dict, algorithm: str) -> dict:
    config = GeneratorConfig(seed=99, depot_count=1, max_drones=3, **overrides)
    map_path, params_path = tmp_path / "map.json", tmp_path / "params.json"
    write_workspace(map_path, params_path, config)
    tours_path = cli.plan(map_path, params_path, tmp_path / "tours.json", algorithm)
    stats = analyze_tours(io.read_json(tours_path)).as_dict()
    skipped = io.read_json(tmp_path / "skipped.json")
    return {key: stats[key] for key in TRACKED} | {
        "skipped": sum(len(region["cells"]) for region in skipped["out_of_range"])
    }


@pytest.mark.parametrize("name,overrides,algorithm", CASES)
def test_planner_output_is_stable(
    tmp_path: Path, name: str, overrides: dict, algorithm: str
) -> None:
    key = f"{name}/{algorithm}"
    actual = fingerprint(tmp_path, overrides, algorithm)

    if os.environ.get("FLEET_UPDATE_GOLDEN"):
        golden = json.loads(GOLDEN.read_text()) if GOLDEN.exists() else {}
        golden[key] = actual
        GOLDEN.parent.mkdir(parents=True, exist_ok=True)
        GOLDEN.write_text(json.dumps(golden, indent=2, sort_keys=True) + "\n")
        pytest.skip(f"refreshed golden for {key}")

    assert GOLDEN.exists(), "run with FLEET_UPDATE_GOLDEN=1 to create the fixture"
    expected = json.loads(GOLDEN.read_text()).get(key)
    assert expected is not None, f"no golden entry for {key}"
    assert actual == expected, (
        f"planner output changed for {key}: {actual} != {expected}. "
        "If this is an intentional improvement, refresh with FLEET_UPDATE_GOLDEN=1."
    )
