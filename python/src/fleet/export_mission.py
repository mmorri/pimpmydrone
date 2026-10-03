"""Export planned tours to georeferenced mission formats operators can fly."""

from __future__ import annotations

import csv
from pathlib import Path
from typing import Any, Sequence

from . import io
from .geo import GeoTransform


def simplify_grid_path(
    waypoints: Sequence[dict[str, Any]],
    recharge_stops: Sequence[int],
) -> list[dict[str, Any]]:
    """Keep endpoints, recharge landings, and heading changes only."""

    if not waypoints:
        return []
    keep = {0, len(waypoints) - 1, *recharge_stops}
    for i in range(1, len(waypoints) - 1):
        prev = waypoints[i - 1]
        cur = waypoints[i]
        nxt = waypoints[i + 1]
        dx1, dy1 = cur["x"] - prev["x"], cur["y"] - prev["y"]
        dx2, dy2 = nxt["x"] - cur["x"], nxt["y"] - cur["y"]
        if (dx1, dy1) != (dx2, dy2):
            keep.add(i)
    return [waypoints[i] for i in sorted(keep)]


def tour_to_geo_waypoints(
    tour: dict[str, Any],
    geo: GeoTransform,
    *,
    simplify: bool = True,
) -> list[dict[str, Any]]:
    waypoints = tour["waypoints"]
    recharge = set(tour.get("recharge_stops") or [])
    selected = (
        simplify_grid_path(waypoints, sorted(recharge)) if simplify else list(waypoints)
    )
    out: list[dict[str, Any]] = []
    for wp in selected:
        lon, lat = geo.cell_to_lonlat(int(wp["x"]), int(wp["y"]))
        idx = int(wp["index"])
        out.append(
            {
                "index": idx,
                "x": int(wp["x"]),
                "y": int(wp["y"]),
                "t": float(wp["t"]),
                "lon": lon,
                "lat": lat,
                "alt_m": geo.altitude_m,
                "recharge": idx in recharge,
            }
        )
    return out


def export_geojson_missions(
    tours: Sequence[dict[str, Any]],
    geo: GeoTransform,
    path: Path,
    *,
    simplify: bool = True,
) -> Path:
    features = []
    for tour in tours:
        pts = tour_to_geo_waypoints(tour, geo, simplify=simplify)
        if len(pts) < 2:
            continue
        coords = [[p["lon"], p["lat"], p["alt_m"]] for p in pts]
        features.append(
            {
                "type": "Feature",
                "properties": {
                    "drone_id": tour["drone_id"],
                    "waypoints": len(pts),
                    "recharge_stops": tour.get("recharge_stops") or [],
                    "cost": tour.get("cost"),
                    "vertical": geo.vertical,
                },
                "geometry": {"type": "LineString", "coordinates": coords},
            }
        )
    payload = {"type": "FeatureCollection", "features": features}
    io.write_json(path, payload)
    return path


def export_qgc_plans(
    tours: Sequence[dict[str, Any]],
    geo: GeoTransform,
    out_dir: Path,
    *,
    simplify: bool = True,
    geo_payload: dict[str, Any] | None = None,
) -> list[Path]:
    """Write one QGroundControl ``.plan`` file per drone (altitude = AGL draft)."""

    io.ensure_directory(out_dir)
    paths: list[Path] = []
    fence = _geofence_from_geo(geo_payload)
    # AltitudeMode 1 = RelHome / AGL-style relative altitude in QGC plan items.
    alt_mode = 1
    for tour in tours:
        pts = tour_to_geo_waypoints(tour, geo, simplify=simplify)
        if not pts:
            continue
        home = pts[0]
        items = []
        seq = 0
        items.append(
            _qgc_item(
                seq,
                22,
                home["lat"],
                home["lon"],
                geo.altitude_m,
                [0, 0, 0, None, home["lat"], home["lon"], geo.altitude_m],
                altitude_mode=alt_mode,
            )
        )
        seq += 1
        for pt in pts[1:]:
            if pt["recharge"]:
                items.append(
                    _qgc_item(
                        seq,
                        21,
                        pt["lat"],
                        pt["lon"],
                        0.0,
                        [0, 0, 0, None, pt["lat"], pt["lon"], 0],
                        altitude_mode=alt_mode,
                    )
                )
                seq += 1
                items.append(
                    _qgc_item(
                        seq,
                        22,
                        pt["lat"],
                        pt["lon"],
                        geo.altitude_m,
                        [0, 0, 0, None, pt["lat"], pt["lon"], geo.altitude_m],
                        altitude_mode=alt_mode,
                    )
                )
                seq += 1
            else:
                items.append(
                    _qgc_item(
                        seq,
                        16,
                        pt["lat"],
                        pt["lon"],
                        geo.altitude_m,
                        [0, 0, 0, None, pt["lat"], pt["lon"], geo.altitude_m],
                        altitude_mode=alt_mode,
                    )
                )
                seq += 1
        last = pts[-1]
        items.append(
            _qgc_item(
                seq,
                21,
                last["lat"],
                last["lon"],
                0.0,
                [0, 0, 0, None, last["lat"], last["lon"], 0],
                altitude_mode=alt_mode,
            )
        )
        plan = {
            "fileType": "Plan",
            "groundStation": "QGroundControl",
            "version": 1,
            "geoFence": fence,
            "rallyPoints": {"points": [], "version": 2},
            "mission": {
                "cruiseSpeed": max(1.0, float(geo.cell_size_m)),
                "firmwareType": 12,
                "globalPlanAltitudeMode": alt_mode,
                "hoverSpeed": 5,
                "items": items,
                "plannedHomePosition": [home["lat"], home["lon"], 0.0],
                "vehicleType": 2,
                "version": 2,
            },
        }
        path = out_dir / f"drone_{tour['drone_id']}.plan"
        io.write_json(path, plan)
        paths.append(path)
    return paths


def _geofence_from_geo(geo_payload: dict[str, Any] | None) -> dict[str, Any]:
    """Build QGC geofence polygons from avoid rings stored on geo.json."""

    polygons: list[dict[str, Any]] = []
    if geo_payload:
        for geom in geo_payload.get("avoid") or []:
            ring = _exterior_ring(geom)
            if ring and len(ring) >= 3:
                polygons.append(
                    {
                        "version": 1,
                        "polygon": [[lat, lon] for lon, lat in ring],
                        "inclusion": False,
                    }
                )
        site = geo_payload.get("site")
        site_ring = _exterior_ring(site) if site else None
        if site_ring and len(site_ring) >= 3:
            polygons.insert(
                0,
                {
                    "version": 1,
                    "polygon": [[lat, lon] for lon, lat in site_ring],
                    "inclusion": True,
                },
            )
    return {"circles": [], "polygons": polygons, "version": 2}


def _exterior_ring(geom: dict[str, Any] | None) -> list[list[float]] | None:
    if not geom:
        return None
    gtype = geom.get("type")
    coords = geom.get("coordinates")
    if gtype == "Polygon" and coords:
        return [[float(lon), float(lat)] for lon, lat, *_ in coords[0]]
    if gtype == "MultiPolygon" and coords:
        # Largest ring by vertex count as a cheap stand-in for area.
        rings = coords
        best = max(rings, key=lambda poly: len(poly[0]) if poly else 0)
        return [[float(lon), float(lat)] for lon, lat, *_ in best[0]]
    return None


def export_waypoints_csv(
    tours: Sequence[dict[str, Any]],
    geo: GeoTransform,
    path: Path,
    *,
    simplify: bool = True,
) -> Path:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(
            handle,
            fieldnames=[
                "drone_id",
                "seq",
                "lon",
                "lat",
                "alt_m",
                "t",
                "recharge",
                "grid_x",
                "grid_y",
            ],
        )
        writer.writeheader()
        for tour in tours:
            pts = tour_to_geo_waypoints(tour, geo, simplify=simplify)
            for seq, pt in enumerate(pts):
                writer.writerow(
                    {
                        "drone_id": tour["drone_id"],
                        "seq": seq,
                        "lon": f"{pt['lon']:.8f}",
                        "lat": f"{pt['lat']:.8f}",
                        "alt_m": pt["alt_m"],
                        "t": pt["t"],
                        "recharge": int(pt["recharge"]),
                        "grid_x": pt["x"],
                        "grid_y": pt["y"],
                    }
                )
    return path


def export_all(
    tours_path: Path,
    geo_path: Path,
    out_dir: Path,
    *,
    simplify: bool = True,
    metrics: dict[str, Any] | None = None,
) -> dict[str, Any]:
    tours = io.read_json(tours_path)
    geo_payload = io.read_json(geo_path)
    geo = GeoTransform.from_json(geo_payload)
    io.ensure_directory(out_dir)
    geojson_path = out_dir / "missions.geojson"
    csv_path = out_dir / "waypoints.csv"
    plans_dir = out_dir / "qgc"
    export_geojson_missions(tours, geo, geojson_path, simplify=simplify)
    export_waypoints_csv(tours, geo, csv_path, simplify=simplify)
    plans = export_qgc_plans(
        tours, geo, plans_dir, simplify=simplify, geo_payload=geo_payload
    )
    from .compact import compression_ratio, write_compact_tours

    compact_path = out_dir / "tours_compact.json"
    write_compact_tours(compact_path, tours)
    compact_ratio = round(compression_ratio(tours), 3)
    deconflict_ok = ((metrics or {}).get("deconfliction") or {}).get("ok")
    readme = out_dir / "README_FLYING.md"
    readme.write_text(
        "\n".join(
            [
                "# How to use these files",
                "",
                "1. Read `SAFETY_REPORT.md` and `DISCLAIMER.txt` completely.",
                "2. Open each `qgc/drone_*.plan` in QGroundControl (or import CSV).",
                "3. Altitudes are **AGL / RelHome drafts** — confirm frame matches your vehicle.",
                "4. Inclusion site + avoid keep-outs are embedded as QGC geofence polygons when present.",
                "5. Multi-drone clocks were time-staggered when possible; still verify separation yourself.",
                f"6. Simulator deconfliction flag: **{deconflict_ok}**.",
                "7. `tours_compact.json` is a lossless `runs-v1` encoding of the grid tours (smaller archive).",
                "",
                "These paths are drafts from an experimental planner. You fly them at your own risk.",
                "",
            ]
        ),
        encoding="utf-8",
    )
    manifest = {
        "geojson": str(geojson_path),
        "csv": str(csv_path),
        "qgc_plans": [str(p) for p in plans],
        "tours_compact": str(compact_path),
        "compact_ratio_expanded_over_compact": compact_ratio,
        "drones": len(tours),
        "vertical": geo.vertical,
        "altitude_m": geo.altitude_m,
        "altitude_mode": geo_payload.get("altitude_mode", "AGL"),
        "geofence_polygons": len(
            ((_geofence_from_geo(geo_payload) or {}).get("polygons") or [])
        ),
        "simplified": simplify,
        "disclaimer": "See SAFETY_REPORT.md — authors accept no responsibility for flights.",
    }
    io.write_json(out_dir / "manifest.json", manifest)
    from .safety import write_safety_bundle

    write_safety_bundle(
        out_dir,
        metrics=metrics,
        geo=geo_payload,
        manifest=manifest,
        vertical=geo.vertical,
    )
    return manifest


def _qgc_item(
    seq: int,
    command: int,
    lat: float,
    lon: float,
    alt: float,
    params: list[Any],
    *,
    altitude_mode: int = 1,
) -> dict[str, Any]:
    return {
        "AMSLAltAboveTerrain": None,
        "Altitude": alt,
        "AltitudeMode": altitude_mode,
        "autoContinue": True,
        "command": command,
        "doJumpId": seq + 1,
        "frame": 3,  # MAV_FRAME_GLOBAL_RELATIVE_ALT
        "params": params,
        "type": "SimpleItem",
    }


__all__ = [
    "export_all",
    "export_geojson_missions",
    "export_qgc_plans",
    "export_waypoints_csv",
    "simplify_grid_path",
    "tour_to_geo_waypoints",
]
