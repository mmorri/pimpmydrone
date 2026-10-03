"""Disclaimer gate + safety artefacts."""

from __future__ import annotations

from pathlib import Path

from fastapi.testclient import TestClient

from fleet.export_mission import export_all
from fleet.geo import GeoTransform
from fleet.safety import DISCLAIMER_FULL, build_safety_report, collect_warnings
from fleet.webapp import create_app
from fleet import io


def test_safety_report_contains_no_responsibility_language() -> None:
    text = build_safety_report(
        metrics={
            "coverage": {"percentage": 100.0, "reachable_cells": 10, "unreachable_cells": 0},
            "drones": {"used": 2},
            "makespan": 12,
            "total_length": 40,
            "checks": {"problems": []},
        },
        geo={"origin_lon": 0, "origin_lat": 0, "cell_size_m": 25, "width": 4, "height": 4, "altitude_m": 40},
        manifest={"geojson": "missions.geojson", "csv": "waypoints.csv", "qgc_plans": [], "simplified": True},
        vertical="solar",
    )
    assert "no liability" in text.lower() or "no responsibility" in text.lower()
    assert "deconfliction" in text.lower()
    assert DISCLAIMER_FULL.splitlines()[0] in text


def test_warnings_flag_multi_drone_and_partial_coverage() -> None:
    warnings = collect_warnings(
        metrics={
            "coverage": {"percentage": 88.0},
            "drones": {"used": 3},
            "checks": {"problems": ["gap"]},
            "efficiency": {"commute_share": 0.8},
        },
        geo={"cell_size_m": 25},
    )
    assert any("88" in w for w in warnings)
    assert any("deconfliction" in w.lower() for w in warnings)


def test_export_writes_safety_files(tmp_path: Path) -> None:
    geo = GeoTransform(-122.25, 37.77, 10.0, 10, 10, vertical="solar", altitude_m=35)
    geo_path = tmp_path / "geo.json"
    tours_path = tmp_path / "tours.json"
    io.write_json(geo_path, geo.to_json())
    io.write_json(
        tours_path,
        [
            {
                "drone_id": 0,
                "waypoints": [
                    {"index": 0, "x": 0, "y": 0, "t": 1.0},
                    {"index": 1, "x": 1, "y": 0, "t": 2.0},
                    {"index": 2, "x": 0, "y": 0, "t": 4.0},
                ],
                "segments": [],
                "recharge_stops": [],
                "cost": {"time": 4.0, "turns": 1, "length": 2.0},
            }
        ],
    )
    export_all(
        tours_path,
        geo_path,
        tmp_path / "missions",
        metrics={
            "coverage": {"percentage": 100.0, "reachable_cells": 3, "unreachable_cells": 0},
            "drones": {"used": 1},
            "makespan": 4,
            "total_length": 2,
            "checks": {"problems": []},
        },
    )
    assert (tmp_path / "missions" / "SAFETY_REPORT.md").exists()
    assert (tmp_path / "missions" / "DISCLAIMER.txt").exists()
    assert "AS IS" in (tmp_path / "missions" / "DISCLAIMER.txt").read_text(encoding="utf-8")


def test_api_rejects_plan_without_disclaimer() -> None:
    client = TestClient(create_app())
    res = client.post(
        "/api/plan",
        json={
            "geojson": {
                "type": "Polygon",
                "coordinates": [[
                    [0, 0], [0.001, 0], [0.001, 0.001], [0, 0.001], [0, 0]
                ]],
            },
            "accepted_disclaimer": False,
        },
    )
    assert res.status_code == 400
    assert "disclaimer" in res.json()["detail"].lower()


def test_api_config_is_donation_funded() -> None:
    client = TestClient(create_app())
    cfg = client.get("/api/config").json()
    assert cfg["monetization"] == "donations"
    assert "donate_url" in cfg
    home = client.get("/")
    assert home.status_code == 200
    assert "do not take responsibility" in home.text.lower() or "not</em> take responsibility" in home.text.lower()
    assert "not spatially deconflicted" not in home.text.lower()
    assert "discrete same-cell" in home.text.lower()
    health = client.get("/api/health").json()
    assert health["partition_families"] is True
    assert health["hangar_partition"] == "auto"
    assert health["version"].startswith("0.5.")
