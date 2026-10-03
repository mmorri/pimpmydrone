"""Coverage-age / Gantt visuals and OSM enrichment helpers."""

from __future__ import annotations

from pathlib import Path

from fleet.generate_map import GeneratorConfig, write_workspace
from fleet.map_utils import MapSpec
from fleet.osm import site_bbox
from fleet.visualize import render_coverage_age, render_sortie_gantt, visualize


def test_site_bbox_from_solar_example() -> None:
    from fleet import io

    root = Path(__file__).resolve().parents[2]
    geo = io.read_json(root / "examples" / "sites" / "solar_farm.geojson")
    south, west, north, east = site_bbox(geo)
    assert south < north and west < east


def test_heatmap_and_gantt_written(tmp_path: Path) -> None:
    map_path = tmp_path / "map.json"
    params_path = tmp_path / "params.json"
    write_workspace(
        map_path,
        params_path,
        GeneratorConfig(width=6, height=6, seed=3, obstacle_density=0.0, no_fly_count=0),
    )
    tours = [
        {
            "drone_id": 0,
            "waypoints": [
                {"index": 0, "x": 0, "y": 0, "t": 0.0},
                {"index": 1, "x": 1, "y": 0, "t": 1.0},
                {"index": 2, "x": 2, "y": 0, "t": 2.0},
                {"index": 3, "x": 2, "y": 1, "t": 3.0},
            ],
            "segments": [],
            "recharge_stops": [2],
            "cost": {"time": 3.0, "turns": 1, "length": 3.0},
        },
        {
            "drone_id": 1,
            "waypoints": [
                {"index": 0, "x": 0, "y": 2, "t": 0.5},
                {"index": 1, "x": 1, "y": 2, "t": 1.5},
                {"index": 2, "x": 1, "y": 3, "t": 2.5},
            ],
            "segments": [],
            "recharge_stops": [],
            "cost": {"time": 2.5, "turns": 1, "length": 2.0},
        },
    ]
    tours_path = tmp_path / "tours.json"
    from fleet import io

    io.write_json(tours_path, tours)
    png = tmp_path / "path.png"
    visualize(map_path, tours_path, png)
    assert png.exists()
    assert (tmp_path / "coverage_age.png").exists()
    assert (tmp_path / "sortie_gantt.png").exists()

    spec = MapSpec.from_json(io.read_json(map_path))
    heat = render_coverage_age(spec, tours, tmp_path / "heat2.png")
    gantt = render_sortie_gantt(tours, tmp_path / "gantt2.png")
    assert heat.exists() and gantt.exists()
