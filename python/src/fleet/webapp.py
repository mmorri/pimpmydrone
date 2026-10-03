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
from fastapi.responses import FileResponse, HTMLResponse, JSONResponse, PlainTextResponse
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
DONATE_ALT_URL = os.environ.get(
    "FLEET_DONATE_ALT_URL", "https://ko-fi.com/pimpmydrone"
)
GITHUB_URL = os.environ.get(
    "FLEET_GITHUB_URL", "https://github.com/mmorri/pimpmydrone"
)
JOB_TTL_SECONDS = int(os.environ.get("FLEET_JOB_TTL_SECONDS", str(24 * 3600)))
RATE_LIMIT_PER_MIN = int(os.environ.get("FLEET_RATE_LIMIT_PER_MIN", "12"))
_CORS = [o.strip() for o in os.environ.get("FLEET_CORS_ORIGINS", "*").split(",") if o.strip()]


class PlanRequest(BaseModel):
    geojson: dict[str, Any]
    vertical: str = Field(default="solar")
    cell_size_m: Optional[float] = None
    max_drones: Optional[int] = Field(default=None, ge=1, le=12)
    battery: Optional[float] = Field(default=None, gt=0)
    altitude_m: Optional[float] = Field(default=None, gt=0, le=120)
    accepted_disclaimer: bool = False


class PerceptionIngestRequest(BaseModel):
    perception: dict[str, Any]
    ontology: str = Field(default="solar")
    role_map: Optional[dict[str, str]] = None
    min_confidence: float = Field(default=0.25, ge=0, le=1)
    accepted_disclaimer: bool = False


class PerceptionDetectRequest(BaseModel):
    backend: str = Field(default="demo")
    ontology: str = Field(default="solar")
    text: Optional[str] = None
    image_b64: Optional[str] = None
    image_url: Optional[str] = None
    center_lon: Optional[float] = None
    center_lat: Optional[float] = None
    frame_georef: Optional[dict[str, float]] = None
    accepted_disclaimer: bool = False


class PerceptionCommandRequest(BaseModel):
    command: str
    ontology: str = Field(default="solar")
    backend: str = Field(default="vlm")
    image_b64: Optional[str] = None
    center_lon: Optional[float] = None
    center_lat: Optional[float] = None
    accepted_disclaimer: bool = False


class PlanFromPerceptionRequest(BaseModel):
    perception: dict[str, Any]
    ontology: str = Field(default="solar")
    vertical: str = Field(default="solar")
    role_map: Optional[dict[str, str]] = None
    min_confidence: float = Field(default=0.25, ge=0, le=1)
    cell_size_m: Optional[float] = None
    max_drones: Optional[int] = Field(default=None, ge=1, le=12)
    battery: Optional[float] = Field(default=None, gt=0)
    altitude_m: Optional[float] = Field(default=None, gt=0, le=120)
    accepted_disclaimer: bool = False


def create_app() -> FastAPI:
    app = FastAPI(
        title="PimpMyDrone",
        description=(
            "Experimental multi-drone coverage planner with perception adapters "
            "(YOLO / CNN / VLM / domain). Free forever. Donations welcome. "
            "Authors accept no responsibility for flights."
        ),
        version="0.5.6",
    )
    app.add_middleware(
        CORSMiddleware,
        allow_origins=_CORS or ["*"],
        allow_methods=["GET", "POST"],
        allow_headers=["*"],
    )
    # Simple in-process rate limit for plan endpoints (fair-use, not auth).
    hit_log: dict[str, list[float]] = {}

    @app.middleware("http")
    async def rate_limit_plans(request, call_next):
        path = request.url.path
        if request.method == "POST" and path.startswith("/api/plan"):
            client = (request.client.host if request.client else "unknown")
            now = time.time()
            window = hit_log.setdefault(client, [])
            hit_log[client] = [t for t in window if now - t < 60.0]
            if len(hit_log[client]) >= RATE_LIMIT_PER_MIN:
                return JSONResponse(
                    {
                        "detail": (
                            f"Fair-use limit: {RATE_LIMIT_PER_MIN} plan requests / "
                            "minute / IP. Run locally or donate to keep the hangar online."
                        )
                    },
                    status_code=429,
                )
            hit_log[client].append(now)
        return await call_next(request)

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

    @app.get("/guide", response_class=HTMLResponse)
    def guide_page() -> HTMLResponse:
        path = site_dir / "guide.html"
        if path.exists():
            return HTMLResponse(_inject_config(path.read_text(encoding="utf-8")))
        raise HTTPException(404, "Guide page missing")

    @app.get("/readme", response_class=HTMLResponse)
    def readme_page() -> HTMLResponse:
        """Alias for the operator case-study guide."""
        return guide_page()

    @app.get("/api/health")
    def health() -> dict[str, Any]:
        return {
            "status": "ok",
            "product": "pimpmydrone",
            "version": "0.5.6",
            "monetization": "donations",
            "max_cells": MAX_CELLS,
            "perception": True,
            "deconfliction": True,
            "inspection_metrics": True,
            "schedule_mode": "sortie",
            "schedule_modes": ["sortie", "tour", "columns"],
            "partitions": ["region", "strip", "auto"],
            "osm_buildings": True,
            "coverage_age": True,
            "compact_tours": True,
            "flat_depot_field": True,
            "sortie_gantt": True,
            "column_generation": True,
            "dual_pricing": True,
            "region_columns": True,
            "partition_families": True,
        }

    @app.get("/api/config")
    def config() -> dict[str, Any]:
        return {
            "donate_url": DONATE_URL,
            "donate_alt_url": DONATE_ALT_URL,
            "github_url": GITHUB_URL,
            "max_cells": MAX_CELLS,
            "disclaimer_short": DISCLAIMER_SHORT,
            "monetization": "donations",
            "rate_limit_per_min": RATE_LIMIT_PER_MIN,
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

    @app.get("/api/perception/backends")
    def perception_backends() -> dict[str, Any]:
        from .perception.backends import available_backends

        return {"backends": available_backends()}

    @app.get("/api/perception/ontology")
    def perception_ontology() -> dict[str, Any]:
        from .perception.ontology import ontology_catalog

        return ontology_catalog()

    @app.post("/api/perception/ingest")
    def perception_ingest(body: PerceptionIngestRequest) -> dict[str, Any]:
        _require_disclaimer(body.accepted_disclaimer)
        from .perception.compile import compile_detections
        from .perception.schema import validate_perception

        errors = validate_perception(body.perception)
        if errors:
            raise HTTPException(400, "; ".join(errors[:8]))
        try:
            compiled = compile_detections(
                body.perception,
                ontology=body.ontology,
                role_map=body.role_map,
                min_confidence=body.min_confidence,
            )
        except ValueError as exc:
            raise HTTPException(400, str(exc)) from exc
        return {
            "site_geojson": compiled.site_geojson,
            "report": compiled.report,
            "normalized": compiled.normalized,
            "disclaimer": DISCLAIMER_SHORT,
        }

    @app.post("/api/perception/detect")
    def perception_detect(body: PerceptionDetectRequest) -> dict[str, Any]:
        _require_disclaimer(body.accepted_disclaimer)
        from .perception.backends import run_detect
        from .perception.backends.base import PerceptionInput

        center = None
        if body.center_lon is not None and body.center_lat is not None:
            center = (body.center_lon, body.center_lat)
        try:
            fc = run_detect(
                body.backend,
                PerceptionInput(
                    ontology=body.ontology,
                    text=body.text,
                    image_b64=body.image_b64,
                    image_url=body.image_url,
                    center_lonlat=center,
                    frame_georef=body.frame_georef,
                ),
            )
        except (KeyError, RuntimeError, ValueError) as exc:
            raise HTTPException(400, str(exc)) from exc
        return {"perception": fc, "backend": body.backend}

    @app.post("/api/perception/command")
    def perception_command(body: PerceptionCommandRequest) -> dict[str, Any]:
        _require_disclaimer(body.accepted_disclaimer)
        from .perception.backends import run_detect
        from .perception.backends.base import PerceptionInput

        center = None
        if body.center_lon is not None and body.center_lat is not None:
            center = (body.center_lon, body.center_lat)
        backend = body.backend
        try:
            fc = run_detect(
                backend,
                PerceptionInput(
                    ontology=body.ontology,
                    text=body.command,
                    image_b64=body.image_b64,
                    center_lonlat=center,
                ),
            )
        except (KeyError, RuntimeError, ValueError):
            # Always usable hangar path.
            fc = run_detect(
                "demo",
                PerceptionInput(
                    ontology=body.ontology,
                    text=body.command,
                    center_lonlat=center,
                ),
            )
            backend = "demo"
        return {"perception": fc, "backend": backend, "command": body.command}

    @app.post("/api/plan/from-perception")
    def plan_from_perception(body: PlanFromPerceptionRequest) -> dict[str, Any]:
        _require_disclaimer(body.accepted_disclaimer)
        from .perception.compile import compile_detections

        try:
            compiled = compile_detections(
                body.perception,
                ontology=body.ontology,
                role_map=body.role_map,
                min_confidence=body.min_confidence,
            )
        except ValueError as exc:
            raise HTTPException(400, str(exc)) from exc
        result = _run_job(
            compiled.site_geojson,
            vertical=body.vertical,
            cell_size_m=body.cell_size_m,
            max_drones=body.max_drones,
            battery=body.battery,
            altitude_m=body.altitude_m,
            perception_report=compiled.report,
        )
        # Persist perception artefacts alongside the job.
        job = _job_dir(result["job_id"])
        io.write_json(job / "perception.json", body.perception)
        io.write_json(job / "perception_report.json", compiled.report)
        io.write_json(job / "site_from_perception.geojson", compiled.site_geojson)
        result["perception_report"] = compiled.report
        return result

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

    @app.get("/api/jobs/{job_id}/coverage-age.png")
    def job_coverage_age(job_id: str) -> FileResponse:
        return _file_response(_job_dir(job_id) / "coverage_age.png", "image/png")

    @app.get("/api/jobs/{job_id}/sortie-gantt.png")
    def job_sortie_gantt(job_id: str) -> FileResponse:
        return _file_response(_job_dir(job_id) / "sortie_gantt.png", "image/png")

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
                "donate_alt_url": DONATE_ALT_URL,
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
    perception_report: Optional[dict[str, Any]] = None,
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
    if perception_report:
        from .safety import write_safety_bundle

        io.write_json(out_dir / "perception_report.json", perception_report)
        write_safety_bundle(
            out_dir / "missions",
            metrics=metrics,
            geo=geo_payload,
            manifest=result.manifest,
            vertical=vertical,
            perception=perception_report,
        )
    warnings = collect_warnings(
        metrics=metrics, geo=geo_payload, perception=perception_report
    )
    return {
        "job_id": job_id,
        "vertical": vertical,
        "coverage_pct": metrics["coverage"]["percentage"],
        "inspection_pct": (metrics.get("inspection") or {}).get("percentage"),
        "deconflicted": (metrics.get("deconfliction") or {}).get("ok"),
        "makespan": metrics["makespan"],
        "drones": metrics["drones"]["used"],
        "total_length": metrics["total_length"],
        "ok": result.simulation.ok,
        "warnings": warnings,
        "disclaimer": DISCLAIMER_SHORT,
        "donate_url": DONATE_URL,
        "donate_alt_url": DONATE_ALT_URL,
        "png_url": f"/api/jobs/{job_id}/png",
        "coverage_age_url": f"/api/jobs/{job_id}/coverage-age.png",
        "sortie_gantt_url": f"/api/jobs/{job_id}/sortie-gantt.png",
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
