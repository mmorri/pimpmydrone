"""Vertical site presets and the site → plan → export pipeline."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Any, Optional

from . import io
from .export_mission import export_all
from .geo import load_geojson, write_site_workspace
from .optimize import optimize_file
from .simulate import SimulationResult, simulate
from .visualize import visualize


@dataclass(frozen=True)
class VerticalPreset:
    name: str
    cell_size_m: float
    altitude_m: float
    speed: float
    battery: float
    takeoff_cost: float
    landing_cost: float
    max_drones: int
    max_cells: int
    description: str


VERTICALS: dict[str, VerticalPreset] = {
    "solar": VerticalPreset(
        name="solar",
        cell_size_m=5.0,
        altitude_m=35.0,
        speed=8.0,
        battery=1800.0,
        takeoff_cost=12.0,
        landing_cost=12.0,
        max_drones=4,
        max_cells=12_000,
        description="Solar-farm inspection: ~5 m cells, mid altitude photo passes.",
    ),
    "security": VerticalPreset(
        name="security",
        cell_size_m=10.0,
        altitude_m=50.0,
        speed=10.0,
        battery=1400.0,
        takeoff_cost=10.0,
        landing_cost=10.0,
        max_drones=3,
        max_cells=8_000,
        description="Perimeter security patrol: coarser cells, dual entry depots.",
    ),
    "generic": VerticalPreset(
        name="generic",
        cell_size_m=8.0,
        altitude_m=40.0,
        speed=8.0,
        battery=1500.0,
        takeoff_cost=10.0,
        landing_cost=10.0,
        max_drones=4,
        max_cells=10_000,
        description="Generic area coverage over a GeoJSON polygon.",
    ),
}


@dataclass
class SiteMissionResult:
    out_dir: Path
    metrics: dict[str, Any]
    manifest: dict[str, Any]
    simulation: SimulationResult
    png_path: Path


def plan_site(
    geojson_path: Path,
    out_dir: Path,
    *,
    vertical: str = "solar",
    cell_size_m: Optional[float] = None,
    max_cells: Optional[int] = None,
    max_drones: Optional[int] = None,
    battery: Optional[float] = None,
    altitude_m: Optional[float] = None,
    coverage_algorithm: str = "auto",
    partition: str = "region",
    drone_cost: float = 1.0,
    deadline: Optional[float] = None,
    solver_time: float = 30.0,
    schedule_mode: str = "sortie",
    deconflict: bool = True,
    osm_buildings: bool = False,
    simplify_export: bool = True,
    plan_fn=None,
) -> SiteMissionResult:
    """Rasterise a site, run the coverage pipeline, and export flyable missions.

    ``plan_fn`` defaults to ``fleet.cli.plan`` and is injectable for tests.
    """

    if vertical not in VERTICALS:
        raise ValueError(f"Unknown vertical {vertical!r}; choose from {sorted(VERTICALS)}")
    preset = VERTICALS[vertical]
    if plan_fn is None:
        from .cli import plan as plan_fn  # local import avoids a cli↔site cycle

    geojson = load_geojson(geojson_path)
    io.ensure_directory(out_dir)
    if osm_buildings:
        from .osm import enrich_geojson_with_osm_buildings

        geojson, osm_report = enrich_geojson_with_osm_buildings(geojson)
        io.write_json(out_dir / "osm_buildings.json", osm_report)
        io.write_json(out_dir / "input_enriched.geojson", geojson)
        # Prefer the enriched geojson as the site input path for reproducibility.
        geojson_path = out_dir / "input_enriched.geojson"
        io.write_json(geojson_path, geojson)
    map_path, params_path, geo_path, _geo = write_site_workspace(
        out_dir,
        geojson,
        cell_size_m=cell_size_m if cell_size_m is not None else preset.cell_size_m,
        max_cells=max_cells if max_cells is not None else preset.max_cells,
        vertical=vertical,
        altitude_m=altitude_m if altitude_m is not None else preset.altitude_m,
        speed=preset.speed,
        battery=battery if battery is not None else preset.battery,
        takeoff_cost=preset.takeoff_cost,
        landing_cost=preset.landing_cost,
        max_drones=max_drones if max_drones is not None else preset.max_drones,
    )

    params = io.read_json(params_path)
    fleet_size = int(params["max_drones"])
    initial = out_dir / "tours_initial.json"
    optimized = out_dir / "tours_optimized.json"
    metrics_path = out_dir / "metrics.json"
    png_path = out_dir / "path.png"
    export_dir = out_dir / "missions"

    plan_fn(map_path, params_path, initial, coverage_algorithm, partition)
    optimize_file(
        initial,
        optimized,
        max_drones=fleet_size,
        drone_cost=drone_cost,
        deadline=deadline,
        time_limit=solver_time,
        mode=schedule_mode,
        deconflict=deconflict,
    )
    simulation = simulate(
        map_path,
        params_path,
        optimized,
        metrics_path,
        skipped_path=out_dir / "skipped.json",
    )
    visualize(map_path, optimized, png_path, None)
    manifest = export_all(
        optimized,
        geo_path,
        export_dir,
        simplify=simplify_export,
        metrics=simulation.metrics,
    )
    return SiteMissionResult(
        out_dir=out_dir,
        metrics=simulation.metrics,
        manifest=manifest,
        simulation=simulation,
        png_path=png_path,
    )


__all__ = ["VERTICALS", "VerticalPreset", "SiteMissionResult", "plan_site"]
