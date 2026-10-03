"""Fetch OpenStreetMap building footprints as avoid / keep-out polygons.

Uses the public Overpass API. Results are tagged ``role=avoid`` so the site
rasteriser treats them as no-fly. Network failures are soft — callers get an
empty list and a warning rather than a hard crash.
"""

from __future__ import annotations

import json
import urllib.error
import urllib.parse
import urllib.request
from typing import Any

from shapely.geometry import Polygon, mapping
from shapely.ops import unary_union

OVERPASS_URL = "https://overpass-api.de/api/interpreter"
USER_AGENT = "PimpMyDrone/0.5 (experimental coverage planner; contact via GitHub mmorri/pimpmydrone)"


def site_bbox(
    geojson: dict[str, Any], *, pad_deg: float = 0.0003
) -> tuple[float, float, float, float]:
    """Return (south, west, north, east) for Overpass from coverage polygons."""

    from .geo import extract_polygons

    site = unary_union(extract_polygons(geojson))
    minx, miny, maxx, maxy = site.bounds
    return (
        miny - pad_deg,
        minx - pad_deg,
        maxy + pad_deg,
        maxx + pad_deg,
    )


def fetch_building_polygons(
    south: float,
    west: float,
    north: float,
    east: float,
    *,
    timeout_s: float = 45.0,
    endpoint: str = OVERPASS_URL,
) -> list[Polygon]:
    """Query Overpass for building ways/relations inside the bbox."""

    query = f"""
    [out:json][timeout:{int(timeout_s)}];
    (
      way["building"]({south},{west},{north},{east});
      relation["building"]({south},{west},{north},{east});
    );
    out geom;
    """
    data = urllib.parse.urlencode({"data": query}).encode("utf-8")
    req = urllib.request.Request(
        endpoint,
        data=data,
        headers={
            "User-Agent": USER_AGENT,
            "Content-Type": "application/x-www-form-urlencoded",
        },
        method="POST",
    )
    try:
        with urllib.request.urlopen(req, timeout=timeout_s) as resp:
            payload = json.loads(resp.read().decode("utf-8"))
    except (urllib.error.URLError, TimeoutError, json.JSONDecodeError) as exc:
        raise RuntimeError(f"Overpass request failed: {exc}") from exc

    polygons: list[Polygon] = []
    for element in payload.get("elements") or []:
        poly = _element_polygon(element)
        if poly is not None and not poly.is_empty and poly.is_valid:
            polygons.append(poly)
    return polygons


def enrich_geojson_with_osm_buildings(
    geojson: dict[str, Any],
    *,
    timeout_s: float = 45.0,
    endpoint: str = OVERPASS_URL,
) -> tuple[dict[str, Any], dict[str, Any]]:
    """Return (enriched FeatureCollection, report)."""

    south, west, north, east = site_bbox(geojson)
    buildings = fetch_building_polygons(
        south, west, north, east, timeout_s=timeout_s, endpoint=endpoint
    )
    features = list(geojson.get("features") or [])
    if geojson.get("type") != "FeatureCollection":
        features = [
            {
                "type": "Feature",
                "properties": {"role": "cover"},
                "geometry": (
                    geojson
                    if geojson.get("type") in {"Polygon", "MultiPolygon"}
                    else None
                ),
            }
        ]
        features = [f for f in features if f.get("geometry")]

    added = 0
    for poly in buildings:
        # Skip tiny scraps
        if poly.area < 1e-12:
            continue
        features.append(
            {
                "type": "Feature",
                "properties": {
                    "role": "avoid",
                    "source": "osm",
                    "name": "OSM building",
                },
                "geometry": mapping(poly),
            }
        )
        added += 1

    enriched = {"type": "FeatureCollection", "features": features}
    report = {
        "bbox": {"south": south, "west": west, "north": north, "east": east},
        "buildings_added": added,
        "endpoint": endpoint,
        "note": (
            "OSM buildings were imported as avoid keep-outs. Verify every polygon "
            "before flight — Overpass data can be stale, incomplete, or wrong."
        ),
    }
    return enriched, report


def _element_polygon(element: dict[str, Any]) -> Polygon | None:
    etype = element.get("type")
    if etype == "way":
        geom = element.get("geometry") or []
        if len(geom) < 3:
            return None
        coords = [(float(p["lon"]), float(p["lat"])) for p in geom]
        if coords[0] != coords[-1]:
            coords.append(coords[0])
        try:
            return Polygon(coords)
        except Exception:  # noqa: BLE001
            return None
    if etype == "relation":
        # Prefer outer members with inline geometry when present.
        outers = []
        for member in element.get("members") or []:
            if member.get("role") not in {"outer", ""}:
                continue
            g = member.get("geometry") or []
            if len(g) >= 3:
                coords = [(float(p["lon"]), float(p["lat"])) for p in g]
                if coords[0] != coords[-1]:
                    coords.append(coords[0])
                try:
                    outers.append(Polygon(coords))
                except Exception:  # noqa: BLE001
                    continue
        if not outers:
            return None
        try:
            merged = unary_union(outers)
            if merged.geom_type == "Polygon":
                return merged
            if merged.geom_type == "MultiPolygon":
                return max(merged.geoms, key=lambda g: g.area)
        except Exception:  # noqa: BLE001
            return None
    return None


__all__ = [
    "enrich_geojson_with_osm_buildings",
    "fetch_building_polygons",
    "site_bbox",
]
