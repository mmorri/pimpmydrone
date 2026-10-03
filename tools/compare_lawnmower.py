#!/usr/bin/env python3
"""Compare PimpMyDrone auto plan vs a naive boustrophedon lawnmower baseline.

Usage:
  cd python && uv run python ../tools/compare_lawnmower.py \\
      --geojson ../examples/sites/solar_industrial.geojson
"""

from __future__ import annotations

import argparse
import json
import shutil
import tempfile
from pathlib import Path

from fleet.site import plan_site


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--geojson", type=Path, required=True)
    parser.add_argument("--vertical", default="solar")
    parser.add_argument("--out", type=Path, default=None)
    args = parser.parse_args()

    root = args.out or Path(tempfile.mkdtemp(prefix="pmd-compare-"))
    auto_dir = root / "auto"
    boustro_dir = root / "boustro"
    for d in (auto_dir, boustro_dir):
        if d.exists():
            shutil.rmtree(d)

    auto = plan_site(
        args.geojson, auto_dir, vertical=args.vertical, coverage_algorithm="auto"
    )
    lawn = plan_site(
        args.geojson, boustro_dir, vertical=args.vertical, coverage_algorithm="boustro"
    )

    def pack(label: str, result) -> dict:
        m = result.metrics
        return {
            "label": label,
            "coverage_pct": m["coverage"]["percentage"],
            "inspection_pct": (m.get("inspection") or {}).get("percentage"),
            "makespan_s": m["makespan"],
            "total_length": m["total_length"],
            "drones": m["drones"]["used"],
            "commute_share": (m.get("efficiency") or {}).get("commute_share"),
            "deconflicted": (m.get("deconfliction") or {}).get("ok"),
            "ok": result.simulation.ok,
        }

    report = {
        "site": str(args.geojson),
        "vertical": args.vertical,
        "auto": pack("auto (mega-STC/boustro mix)", auto),
        "lawnmower": pack("boustro only", lawn),
    }
    a, b = report["auto"], report["lawnmower"]
    if b["total_length"]:
        report["length_improvement_pct"] = round(
            100.0 * (1.0 - a["total_length"] / b["total_length"]), 2
        )
    if b["makespan_s"]:
        report["makespan_improvement_pct"] = round(
            100.0 * (1.0 - a["makespan_s"] / b["makespan_s"]), 2
        )

    out_path = root / "comparison.json"
    out_path.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    print(json.dumps(report, indent=2))
    print(f"\nWrote {out_path}")


if __name__ == "__main__":
    main()
