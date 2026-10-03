"""Perception schema, ontology, compile, and demo backend."""

from __future__ import annotations

import json
from pathlib import Path

import pytest
from fastapi.testclient import TestClient

from fleet.perception.backends import available_backends, run_detect
from fleet.perception.backends.base import PerceptionInput
from fleet.perception.compile import compile_detections
from fleet.perception.ontology import resolve_role
from fleet.perception.schema import validate_perception
from fleet.webapp import create_app

ROOT = Path(__file__).resolve().parents[2]
EXAMPLES = ROOT / "examples" / "perception"


def test_validate_solar_example():
    payload = json.loads((EXAMPLES / "solar_detections.geojson").read_text())
    assert validate_perception(payload) == []


def test_resolve_role_ontology_and_overrides():
    assert resolve_role("solar_panel", ontology="solar")["role"] == "cover"
    assert resolve_role("person", ontology="security")["role"] == "inspect"
    assert resolve_role("smoke", ontology="thermal")["role"] == "inspect"
    assert resolve_role("whatever", explicit_role="avoid")["role"] == "avoid"
    assert resolve_role("custom", role_map={"custom": "cover"})["role"] == "cover"


def test_compile_cover_avoid_inspect():
    payload = json.loads((EXAMPLES / "solar_detections.geojson").read_text())
    compiled = compile_detections(payload, ontology="solar")
    roles = {
        (f.get("properties") or {}).get("role")
        for f in compiled.site_geojson["features"]
    }
    assert "cover" in roles
    assert "avoid" in roles
    assert "inspect" in roles
    assert compiled.report["counts"]["cover"] >= 1
    assert compiled.report["counts"]["avoid"] >= 1


def test_demo_backend_detect():
    fc = run_detect(
        "demo",
        PerceptionInput(ontology="solar", center_lonlat=(-122.25, 37.77)),
    )
    assert fc["type"] == "FeatureCollection"
    assert fc["features"]
    assert validate_perception(fc) == []


def test_backends_list_includes_demo():
    names = {row["name"] for row in available_backends()}
    assert {"demo", "yolo", "onnx", "vlm", "domain"} <= names
    demo = next(r for r in available_backends() if r["name"] == "demo")
    assert demo["installed"] and demo["enabled"]


def test_api_perception_ingest_and_plan():
    app = create_app()
    client = TestClient(app)
    payload = json.loads((EXAMPLES / "solar_detections.geojson").read_text())

    denied = client.post(
        "/api/perception/ingest",
        json={"perception": payload, "ontology": "solar"},
    )
    assert denied.status_code == 400

    ingest = client.post(
        "/api/perception/ingest",
        json={
            "perception": payload,
            "ontology": "solar",
            "accepted_disclaimer": True,
        },
    )
    assert ingest.status_code == 200, ingest.text
    body = ingest.json()
    assert body["site_geojson"]["type"] == "FeatureCollection"
    assert body["report"]["counts"]["cover"] >= 1

    detect = client.post(
        "/api/perception/detect",
        json={
            "backend": "demo",
            "ontology": "security",
            "accepted_disclaimer": True,
            "center_lon": -122.25,
            "center_lat": 37.77,
        },
    )
    assert detect.status_code == 200
    assert detect.json()["perception"]["features"]

    # Full plan can be slow (Haskell); skip if binaries missing.
    from fleet.cli import HASKELL_BIN

    if not (HASKELL_BIN / "coverage-planner").exists():
        pytest.skip("Haskell binaries not built")

    planned = client.post(
        "/api/plan/from-perception",
        json={
            "perception": payload,
            "ontology": "solar",
            "vertical": "solar",
            "accepted_disclaimer": True,
            "cell_size_m": 8,
        },
    )
    assert planned.status_code == 200, planned.text
    data = planned.json()
    assert data["job_id"]
    assert data["perception_report"]["counts"]["avoid"] >= 1
    assert any("perception" in w.lower() for w in data["warnings"])
