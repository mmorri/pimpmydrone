"""Public planner API — donation-funded, disclaimer-gated, no subscriptions."""

from __future__ import annotations

import json
import os
import re
import shutil
import tempfile
import time
import uuid
from pathlib import Path
from typing import Any, Optional

from fastapi import FastAPI, File, Form, HTTPException, UploadFile
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import FileResponse, HTMLResponse, PlainTextResponse
from fastapi.staticfiles import StaticFiles
from pydantic import BaseModel, Field

from . import io
from .safety import DISCLAIMER_FULL, DISCLAIMER_SHORT, collect_warnings
from .site import VERTICALS, plan_site

ROOT = Path(__file__).resolve().parents[3]
WEBSITE_DIR = Path(os.environ.get("FLEET_WEBSITE", ROOT / "website"))
STATIC_DIR = Path(__file__).resolve().parent / "static"
JOBS_ROOT = Path(os.environ.get("FLEET_JOBS", Path(tempfile.gettempdir()) / "fleet-jobs"))
# Fair-use cap so a public demo cannot melt a laptop — not a paywall.
MAX_CELLS = int(os.environ.get("FLEET_MAX_CELLS", "8000"))
MAX_UPLOAD_BYTES = int(os.environ.get("FLEET_MAX_UPLOAD_BYTES", str(2_000_000)))
DONATE_URL = os.environ.get(
    "FLEET_DONATE_URL", "https://github.com/sponsors/mmorri"
)
GITHUB_URL = os.environ.get(
    "FLEET_GITHUB_URL", "https://github.com/mmorri/pimpmydrone"
)
JOB_TTL_SECONDS = int(os.environ.get("FLEET_JOB_TTL_SECONDS", str(24 * 3600)))


class PlanRequest(BaseModel):
    geojson: dict[str, Any]
    vertical: str = Field(default="solar")
    cell_size_m: Optional[float] = None
    max_drones: Optional[int] = Field(default=None, ge=1, le=12)
    battery: Optional[float] = Field(default=None, gt=0)
    altitude_m: Optional[float] = Field(default=None, gt=0, le=120)
    accepted_disclaimer: bool = False


def create_app() -> FastAPI:
    app = FastAPI(
        title="PimpMyDrone",
        description=(
            "Experimental multi-drone coverage planner. Free forever. "
            "Donations welcome. Authors accept no responsibility for flights."
        ),
        version="0.3.3",
    )
    app.add_middleware(
        CORSMiddleware,
        allow_origins=os.environ.get("FLEET_CORS_ORIGINS", "*").split(","),
        allow_methods=["GET", "POST"],
        allow_headers=["*"],
    )

    site_dir = WEBSITE_DIR if WEBSITE_DIR.exists() else STATIC_DIR
    if site_dir.exists():
        app.mount("/assets", StaticFiles(directory=site_dir), name="assets")

    @app.get("/", response_class=HTMLResponse)
    def index() -> HTMLResponse:
        path = site_dir / "index.html"
        if not path.exists():
            raise HTTPException(500, "Website missing")
        return HTMLResponse(_inject_config(path.read_text(encoding="utf-8")))

    @app.get("/legal", response_class=HTMLResponse)
    def legal_page() -> HTMLResponse:
        path = site_dir / "legal.html"
        if path.exists():
            return HTMLResponse(_inject_config(path.read_text(encoding="utf-8")))
        legal = ROOT / "LEGAL.md"
        return HTMLResponse(
            f"<pre style='white-space:pre-wrap;padding:2rem;font:14px/1.45 ui-monospace,monospace'>"
            f"{legal.read_text(encoding='utf-8') if legal.exists() else DISCLAIMER_FULL}"
            f"</pre>"
        )

    @app.get("/donate", response_class=HTMLResponse)
    def donate_page() -> HTMLResponse:
        path = site_dir / "donate.html"
        if path.exists():
            return HTMLResponse(_inject_config(path.read_text(encoding="utf-8")))
        raise HTTPException(404, "Donate page missing")

    @app.get("/api/health")
    def health() -> dict[str, Any]:
        return {
            "status": "ok",
            "product": "pimpmydrone",
            "version": "0.3.3",
            "monetization": "donations",
            "max_cells": MAX_CELLS,
        }

    @app.get("/api/config")
    def config() -> dict[str, Any]:
        return {
            "donate_url": DONATE_URL,
            "github_url": GITHUB_URL,
            "max_cells": MAX_CELLS,
            "disclaimer_short": DISCLAIMER_SHORT,
            "monetization": "donations",
        }

    @app.get("/api/disclaimer", response_class=PlainTextResponse)
    def disclaimer_text() -> str:
        return DISCLAIMER_FULL

    @app.get("/api/verticals")
    def verticals() -> dict[str, Any]:
        return {
            name: {
                "name": preset.name,
                "description": preset.description,
                "cell_size_m": preset.cell_size_m,
                "altitude_m": preset.altitude_m,
                "max_drones": preset.max_drones,
                "battery": preset.battery,
            }
            for name, preset in VERTICALS.items()
        }

    @app.post("/api/plan")
    def plan_json(body: PlanRequest) -> dict[str, Any]:
        _require_disclaimer(body.accepted_disclaimer)
        return _run_job(
            body.geojson,
            vertical=body.vertical,
            cell_size_m=body.cell_size_m,
            max_drones=body.max_drones,
            battery=body.battery,
            altitude_m=body.altitude_m,
        )

    @app.post("/api/plan/upload")
    async def plan_upload(
        file: UploadFile = File(...),
        vertical: str = Form("solar"),
        cell_size_m: Optional[float] = Form(None),
        max_drones: Optional[int] = Form(None),
        battery: Optional[float] = Form(None),
        altitude_m: Optional[float] = Form(None),
        accepted_disclaimer: bool = Form(False),
    ) -> dict[str, Any]:
        _require_disclaimer(accepted_disclaimer)
        raw = await file.read()
        if len(raw) > MAX_UPLOAD_BYTES:
            raise HTTPException(413, f"GeoJSON larger than {MAX_UPLOAD_BYTES} bytes")
        try:
            geojson = json.loads(raw.decode("utf-8"))
        except Exception as exc:  # noqa: BLE001
            raise HTTPException(400, f"Invalid GeoJSON: {exc}") from exc
        return _run_job(
            geojson,
            vertical=vertical,
            cell_size_m=cell_size_m,
            max_drones=max_drones,
            battery=battery,
            altitude_m=altitude_m,
        )

    @app.get("/api/jobs/{job_id}/png")
    def job_png(job_id: str) -> FileResponse:
        return _file_response(_job_dir(job_id) / "path.png", "image/png")

    @app.get("/api/jobs/{job_id}/missions.geojson")
    def job_geojson(job_id: str) -> FileResponse:
        return _file_response(
            _job_dir(job_id) / "missions" / "missions.geojson",
            "application/geo+json",
        )

    @app.get("/api/jobs/{job_id}/waypoints.csv")
    def job_csv(job_id: str) -> FileResponse:
        return _file_response(
            _job_dir(job_id) / "missions" / "waypoints.csv", "text/csv"
        )

    @app.get("/api/jobs/{job_id}/safety")
    def job_safety(job_id: str) -> FileResponse:
        return _file_response(
            _job_dir(job_id) / "missions" / "SAFETY_REPORT.md",
            "text/markdown",
        )

    @app.get("/api/jobs/{job_id}/qgc/{drone_id}.plan")
    def job_qgc(job_id: str, drone_id: int) -> FileResponse:
        return _file_response(
            _job_dir(job_id) / "missions" / "qgc" / f"drone_{drone_id}.plan",
            "application/json",
        )

    @app.get("/api/jobs/{job_id}/bundle.zip")
    def job_bundle(job_id: str) -> FileResponse:
        job = _job_dir(job_id)
        if not job.exists():
            raise HTTPException(404, "Job not found")
        archive = job / "bundle"
        zip_path = Path(shutil.make_archive(str(archive), "zip", root_dir=job))
        return FileResponse(
            zip_path, media_type="application/zip", filename=f"{job_id}.zip"
        )

    return app


def _inject_config(html: str) -> str:
    snippet = (
        "<script>window.FLEET_CONFIG="
        + json.dumps(
            {
                "donate_url": DONATE_URL,
                "github_url": GITHUB_URL,
                "max_cells": MAX_CELLS,
                "disclaimer_short": DISCLAIMER_SHORT,
            }
        )
        + ";</script>"
    )
    if re.search(r"</head>", html, flags=re.I):
        return re.sub(r"</head>", snippet + "</head>", html, count=1, flags=re.I)
    return snippet + html


def _require_disclaimer(accepted: bool) -> None:
    if not accepted:
        raise HTTPException(
            400,
            "You must accept the liability disclaimer before planning. "
            "See /legal — authors accept no responsibility for any flight or damage.",
        )


def _job_dir(job_id: str) -> Path:
    if not job_id or not re.fullmatch(r"[A-Za-z0-9._-]{6,80}", job_id):
        raise HTTPException(400, "Invalid job id")
    return JOBS_ROOT / job_id


def _file_response(path: Path, media_type: str) -> FileResponse:
    if not path.exists():
        raise HTTPException(404, f"{path.name} not found")
    return FileResponse(path, media_type=media_type)


def _cleanup_jobs() -> None:
    if not JOBS_ROOT.exists():
        return
    now = time.time()
    for child in JOBS_ROOT.iterdir():
        try:
            if child.is_dir() and now - child.stat().st_mtime > JOB_TTL_SECONDS:
                shutil.rmtree(child, ignore_errors=True)
        except OSError:
            continue


def _run_job(
    geojson: dict[str, Any],
    *,
    vertical: str,
    cell_size_m: Optional[float],
    max_drones: Optional[int],
    battery: Optional[float],
    altitude_m: Optional[float],
) -> dict[str, Any]:
    if vertical not in VERTICALS:
        raise HTTPException(400, f"Unknown vertical {vertical!r}")
    if not isinstance(geojson, dict) or "type" not in geojson:
        raise HTTPException(400, "Body must be a GeoJSON object")

    job_id = f"{int(time.time())}-{uuid.uuid4().hex[:8]}"
    _cleanup_jobs()
    out_dir = _job_dir(job_id)
    io.ensure_directory(out_dir)
    io.write_json(out_dir / "input.geojson", geojson)
    (out_dir / "DISCLAIMER.txt").write_text(DISCLAIMER_FULL, encoding="utf-8")

    try:
        result = plan_site(
            out_dir / "input.geojson",
            out_dir,
            vertical=vertical,
            cell_size_m=cell_size_m,
            max_cells=MAX_CELLS,
            max_drones=max_drones,
            battery=battery,
            altitude_m=altitude_m,
        )
    except Exception as exc:  # noqa: BLE001
        raise HTTPException(400, str(exc)) from exc

    metrics = result.metrics
    geo_payload = io.read_json(out_dir / "geo.json")
    warnings = collect_warnings(metrics=metrics, geo=geo_payload)
    return {
        "job_id": job_id,
        "vertical": vertical,
        "coverage_pct": metrics["coverage"]["percentage"],
        "makespan": metrics["makespan"],
        "drones": metrics["drones"]["used"],
        "total_length": metrics["total_length"],
        "ok": result.simulation.ok,
        "warnings": warnings,
        "disclaimer": DISCLAIMER_SHORT,
        "donate_url": DONATE_URL,
        "png_url": f"/api/jobs/{job_id}/png",
        "missions_geojson_url": f"/api/jobs/{job_id}/missions.geojson",
        "waypoints_csv_url": f"/api/jobs/{job_id}/waypoints.csv",
        "safety_url": f"/api/jobs/{job_id}/safety",
        "bundle_url": f"/api/jobs/{job_id}/bundle.zip",
        "qgc_urls": [
            f"/api/jobs/{job_id}/qgc/{i}.plan" for i in range(metrics["drones"]["used"])
        ],
        "manifest": result.manifest,
    }


app = create_app()
