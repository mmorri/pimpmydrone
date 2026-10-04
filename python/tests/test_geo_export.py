"""Unit tests for GeoJSON → grid import and mission export."""

from __future__ import annotations

from pathlib import Path

from fleet.export_mission import export_all, simplify_grid_path
from fleet.geo import GeoTransform, write_site_workspace


SOLAR = (
    Path(__file__).resolve().parents[2] / "examples" / "sites" / "solar_farm.geojson"
)


def test_write_site_workspace_solar(tmp_path: Path) -> None:
    import json

    geojson = json.loads(SOLAR.read_text(encoding="utf-8"))
    map_path, params_path, geo_path, geo = write_site_workspace(
        tmp_path,
        geojson,
        cell_size_m=20.0,
        vertical="solar",
        max_drones=2,
        battery=800.0,
    )
    assert map_path.exists() and params_path.exists() and geo_path.exists()
    assert geo.width >= 2 and geo.height >= 2
    assert geo.vertical == "solar"
    payload = json.loads(map_path.read_text(encoding="utf-8"))
    assert payload["depots"]
    free = payload["width"] * payload["height"] - len(payload["obstacles"])
    assert free > 10


def test_cell_lonlat_roundtrip() -> None:
    geo = GeoTransform(-122.25, 37.77, 10.0, 20, 20)
    lon, lat = geo.cell_to_lonlat(3, 5)
    x, y = geo.lonlat_to_cell(lon, lat)
    assert (x, y) == (3, 5)


def test_long_axis_points_along_a_north_south_block(tmp_path: Path) -> None:
    import json

    # ~180 m east, ~1.1 km north. The long edge is nearly due north.
    ring = [
        [-120.0, 36.0],
        [-119.998, 36.0],
        [-119.998, 36.01],
        [-120.0, 36.01],
        [-120.0, 36.0],
    ]
    geojson = {
        "type": "FeatureCollection",
        "features": [
            {
                "type": "Feature",
                "properties": {"role": "cover"},
                "geometry": {"type": "Polygon", "coordinates": [ring]},
            }
        ],
    }
    _map, _params, geo_path, geo = write_site_workspace(
        tmp_path, geojson, cell_size_m=20.0, align="long-axis", vertical="generic"
    )
    assert abs(geo.axis_x_north) > 0.9
    assert abs(geo.axis_x_east) < 0.2
    lon, lat = geo.cell_to_lonlat(2, 3)
    assert geo.lonlat_to_cell(lon, lat) == (2, 3)
    stored = json.loads(geo_path.read_text(encoding="utf-8"))
    restored = GeoTransform.from_json(stored)
    assert abs(restored.axis_x_north) > 0.9
    assert restored.cell_to_lonlat(2, 3) == geo.cell_to_lonlat(2, 3)


def test_simplify_keeps_turns_and_ends() -> None:
    waypoints = [
        {"index": 0, "x": 0, "y": 0, "t": 0},
        {"index": 1, "x": 1, "y": 0, "t": 1},
        {"index": 2, "x": 2, "y": 0, "t": 2},
        {"index": 3, "x": 2, "y": 1, "t": 3},
        {"index": 4, "x": 2, "y": 2, "t": 4},
    ]
    kept = simplify_grid_path(waypoints, recharge_stops=[])
    indices = [wp["index"] for wp in kept]
    assert indices[0] == 0 and indices[-1] == 4
    assert 2 in indices  # heading change


def test_export_all_writes_bundle(tmp_path: Path) -> None:
    geo = GeoTransform(-122.25, 37.77, 10.0, 10, 10, vertical="solar", altitude_m=35)
    geo_path = tmp_path / "geo.json"
    tours_path = tmp_path / "tours.json"
    from fleet import io

    io.write_json(geo_path, geo.to_json())
    tours = [
        {
            "drone_id": 0,
            "waypoints": [
                {"index": 0, "x": 0, "y": 0, "t": 1.0},
                {"index": 1, "x": 1, "y": 0, "t": 2.0},
                {"index": 2, "x": 1, "y": 1, "t": 3.0},
                {"index": 3, "x": 0, "y": 0, "t": 5.0},
            ],
            "segments": [],
            "recharge_stops": [],
            "cost": {"time": 5.0, "turns": 2, "length": 4.0},
        }
    ]
    io.write_json(tours_path, tours)
    manifest = export_all(tours_path, geo_path, tmp_path / "missions")
    assert Path(manifest["geojson"]).exists()
    assert Path(manifest["csv"]).exists()
    assert manifest["qgc_plans"]
    assert Path(manifest["qgc_plans"][0]).exists()
