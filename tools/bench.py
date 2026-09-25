#!/usr/bin/env python3
"""Run a matrix of missions and report plan quality.

The point of this harness is to make algorithm work falsifiable: run it
before and after a change and compare. Every row is one mission; the columns
that matter are ``length`` (total flown steps), ``commute`` (share of those
steps spent travelling to and from a depot) and ``steps/cell`` (how many
times the average covered cell is flown over).

    python3 tools/bench.py                      # default matrix, prints a table
    python3 tools/bench.py --json out.json      # also write raw results
    python3 tools/bench.py --compare base.json  # diff against an earlier run
    python3 tools/bench.py --quick              # small matrix, for a smoke test

Missions run in a temporary directory; nothing is left behind.
"""

from __future__ import annotations

import argparse
import json
import platform
import subprocess
import sys
import tempfile
import time
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import List, Optional

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "python" / "src"))

# Fixed matrix: changing it invalidates comparisons with stored baselines.
MATRIX = [
    # name, width, height, density, no_fly, depots, battery, max_drones
    ("open-40", 40, 40, 0.00, 0, 1, 150.0, 3),
    ("sparse-40", 40, 40, 0.10, 1, 1, 150.0, 3),
    ("cluttered-40", 40, 40, 0.25, 2, 1, 150.0, 3),
    ("twodepot-60", 60, 40, 0.10, 1, 2, 200.0, 4),
    ("tight-battery-40", 40, 40, 0.10, 1, 1, 90.0, 3),
    ("large-100", 100, 100, 0.20, 3, 1, 500.0, 5),
]
QUICK_MATRIX = [MATRIX[0], MATRIX[2]]
ALGORITHMS = ["stc", "boustro", "auto"]
PARTITIONS = ["region"]
SEEDS = [1, 2]


@dataclass
class Row:
    case: str
    algorithm: str
    partition: str
    seed: int
    reachable: int
    out_of_range: int
    coverage: float
    drones: int
    makespan: float
    length: float
    sorties: int
    commute_share: float
    steps_per_cell: float
    plan_seconds: float
    total_seconds: float

    @property
    def key(self) -> str:
        return f"{self.case}/{self.algorithm}/{self.partition}/{self.seed}"


def run_case(spec, algorithm: str, partition: str, seed: int, out_dir: Path) -> Row:
    from fleet import io
    from fleet.generate_map import GeneratorConfig, write_workspace
    from fleet.analyze import analyze_tours
    from fleet.optimize import optimize_file
    from fleet.simulate import simulate

    name, width, height, density, no_fly, depots, battery, max_drones = spec
    config = GeneratorConfig(
        seed=seed,
        width=width,
        height=height,
        obstacle_density=density,
        no_fly_count=no_fly,
        depot_count=depots,
        battery=battery,
        max_drones=max_drones,
    )
    map_path, params_path = out_dir / "map.json", out_dir / "params.json"
    tours_path = out_dir / "tours_initial.json"
    optimized_path = out_dir / "tours_optimized.json"
    metrics_path = out_dir / "metrics.json"

    started = time.perf_counter()
    write_workspace(map_path, params_path, config)
    plan_started = time.perf_counter()
    from fleet import cli

    cli.plan(map_path, params_path, tours_path, algorithm, partition)
    plan_seconds = time.perf_counter() - plan_started
    optimize_file(tours_path, optimized_path, max_drones=max_drones, time_limit=10)
    result = simulate(
        map_path,
        params_path,
        optimized_path,
        metrics_path,
        skipped_path=out_dir / "skipped.json",
    )
    total_seconds = time.perf_counter() - started

    metrics = io.read_json(metrics_path)
    if not result.ok:
        raise RuntimeError(f"{name}/{algorithm}/{seed}: {result.problems}")
    # Measure plan quality on the planner's own output: the solver assigns
    # equal-cost tours arbitrarily across runs, which would make the flight
    # breakdown wobble for reasons that have nothing to do with planning.
    efficiency = analyze_tours(io.read_json(tours_path)).as_dict()
    return Row(
        case=name,
        algorithm=algorithm,
        partition=partition,
        seed=seed,
        reachable=metrics["coverage"]["reachable_cells"],
        out_of_range=metrics["coverage"]["out_of_range_cells"],
        coverage=metrics["coverage"]["percentage"],
        drones=metrics["drones"]["used"],
        makespan=metrics["makespan"],
        length=metrics["total_length"],
        sorties=efficiency["sorties"],
        commute_share=efficiency["commute_share"],
        steps_per_cell=efficiency["steps_per_cell"],
        plan_seconds=plan_seconds,
        total_seconds=total_seconds,
    )


def format_table(rows: List[Row], baseline: Optional[dict]) -> str:
    header = [
        "case",
        "algo",
        "seed",
        "cells",
        "cov%",
        "drones",
        "makespan",
        "length",
        "sorties",
        "commute",
        "steps/cell",
        "skipped",
        "plan s",
    ]
    lines = ["| " + " | ".join(header) + " |", "|" + "---|" * len(header)]
    for row in rows:
        previous = (baseline or {}).get(row.key)
        lines.append(
            "| "
            + " | ".join(
                [
                    row.case,
                    row.algorithm,
                    str(row.seed),
                    str(row.reachable),
                    f"{row.coverage:.1f}",
                    str(row.drones),
                    f"{row.makespan:.0f}{delta(row.makespan, previous, 'makespan')}",
                    f"{row.length:.0f}{delta(row.length, previous, 'length')}",
                    str(row.sorties),
                    f"{100 * row.commute_share:.1f}%",
                    f"{row.steps_per_cell:.2f}",
                    str(row.out_of_range),
                    f"{row.plan_seconds:.2f}",
                ]
            )
            + " |"
        )
    lines.append("")
    lines.append(summary(rows, baseline))
    return "\n".join(lines)


def delta(value: float, previous: Optional[dict], field: str) -> str:
    if not previous or not previous.get(field):
        return ""
    change = 100 * (value - previous[field]) / previous[field]
    if abs(change) < 0.05:
        return " (=)"
    return f" ({change:+.1f}%)"


def summary(rows: List[Row], baseline: Optional[dict]) -> str:
    length = sum(row.length for row in rows)
    makespan = sum(row.makespan for row in rows)
    commute = sum(row.commute_share * row.length for row in rows) / max(length, 1)
    text = (
        f"totals: length {length:.0f}, makespan {makespan:.0f}, "
        f"commute {100 * commute:.1f}%"
    )
    if baseline:
        base_rows = [baseline[row.key] for row in rows if row.key in baseline]
        if len(base_rows) == len(rows):
            base_length = sum(entry["length"] for entry in base_rows)
            base_makespan = sum(entry["makespan"] for entry in base_rows)
            text += (
                f" | vs baseline: length {100 * (length - base_length) / base_length:+.1f}%, "
                f"makespan {100 * (makespan - base_makespan) / base_makespan:+.1f}%"
            )
    return text


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--json", type=Path, help="write raw results here")
    parser.add_argument("--compare", type=Path, help="baseline JSON to diff against")
    parser.add_argument("--quick", action="store_true", help="run a reduced matrix")
    parser.add_argument("--markdown", type=Path, help="write the table to a file")
    args = parser.parse_args()

    from fleet import cli

    # The pipeline logs every subprocess it starts; that is noise in a report.
    cli.console.quiet = True

    missing = [
        binary
        for binary in (cli.HASKELL_BIN / "coverage-partitioner", cli.HASKELL_BIN / "coverage-planner")
        if not binary.exists()
    ]
    if missing:
        print(f"Haskell binaries missing: {missing}. Run `make build` first.", file=sys.stderr)
        return 1

    baseline = None
    if args.compare:
        baseline = {entry["key"]: entry for entry in json.loads(args.compare.read_text())["rows"]}

    matrix = QUICK_MATRIX if args.quick else MATRIX
    seeds = SEEDS[:1] if args.quick else SEEDS
    rows: List[Row] = []
    with tempfile.TemporaryDirectory(prefix="fleet-bench-") as tmp:
        for spec in matrix:
            for algorithm in ALGORITHMS:
                for partition in PARTITIONS:
                    for seed in seeds:
                        out_dir = Path(tmp) / f"{spec[0]}-{algorithm}-{partition}-{seed}"
                        row = run_case(spec, algorithm, partition, seed, out_dir)
                        rows.append(row)
                        print(f"  {row.key}: length {row.length:.0f}", file=sys.stderr)

    table = format_table(rows, baseline)
    print(table)
    if args.markdown:
        args.markdown.write_text(table + "\n")
    if args.json:
        payload = {
            "generated": time.strftime("%Y-%m-%d"),
            "commit": git_commit(),
            "platform": platform.platform(),
            "rows": [{"key": row.key, **asdict(row)} for row in rows],
        }
        args.json.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n")
    return 0


def git_commit() -> str:
    try:
        return subprocess.run(
            ["git", "rev-parse", "--short", "HEAD"],
            cwd=ROOT,
            capture_output=True,
            text=True,
            check=True,
        ).stdout.strip()
    except (subprocess.CalledProcessError, FileNotFoundError):
        return "unknown"


if __name__ == "__main__":
    raise SystemExit(main())
