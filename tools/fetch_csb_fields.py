#!/usr/bin/env python3
"""Pull a hangar-sized US sample from USDA NASS Crop Sequence Boundaries 2024.

CSB polygons cover the lower 48. This script keeps two fields per state/crop
(about 18 and 27 acres) under examples/sites/csb/2024/<state>/<crop>/.
California stays on the surveyed DWR catalog. Do not vendor the national file.
"""

from __future__ import annotations

import json
import math
import urllib.parse
import urllib.request
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "examples" / "sites" / "csb" / "2024"
SERVICE = (
    "https://services3.arcgis.com/g6eV2CrSSwCZj8Mc/arcgis/rest/services/"
    "NASS_Crop_Sequence_Boundaries_2024/FeatureServer/0/query"
)

# CDL class codes used by the 2024 crop-sequence layer.
CROPS = {
    1: ("corn", "Corn"),
    5: ("soybeans", "Soybeans"),
    23: ("spring-wheat", "Spring wheat"),
    41: ("sugar-beets", "Sugar beets"),
}
# The public feature service currently answers for these states only.
# Minnesota is the full layer; Iowa, North Dakota, and South Dakota are thin.
# The other 44 states live in the ~3.5 GB national file geodatabase, which
# this script does not download.
STATES = {
    "27": "Minnesota",
    "19": "Iowa",
    "38": "North Dakota",
    "46": "South Dakota",
}
REQUESTS = [
    ("27", 1),
    ("27", 5),
    ("27", 23),
    ("27", 41),
    ("19", 1),
    ("19", 5),
    ("38", 23),
    ("46", 1),
    ("46", 5),
]
BANDS = ((18, 22, "CSBACRES ASC"), (27, 32, "CSBACRES ASC"))


def _query(fips: str, code: int, lo: float, hi: float, order: str) -> dict:
    where = (
        f"STATEFIPS='{fips}' AND CDL2024={code} "
        f"AND CSBACRES>={lo} AND CSBACRES<={hi}"
    )
    params = {
        "where": where,
        "outFields": "CSBID,CSBACRES,CDL2024,STATEFIPS,CNTY",
        "returnGeometry": "true",
        "outSR": "4326",
        "orderByFields": order,
        "resultRecordCount": "1",
        "geometryPrecision": "5",
        "f": "geojson",
    }
    url = SERVICE + "?" + urllib.parse.urlencode(params)
    req = urllib.request.Request(url, headers={"User-Agent": "PimpMyDrone/0.5.14"})
    with urllib.request.urlopen(req, timeout=60) as resp:
        return json.load(resp)


def _ring(geometry: dict) -> list[list[float]]:
    gtype = geometry.get("type")
    coords = geometry.get("coordinates") or []
    if gtype == "Polygon":
        return coords[0] if coords else []
    if gtype == "MultiPolygon" and coords:
        rings = [poly[0] for poly in coords if poly and poly[0]]
        return max(rings, key=len) if rings else []
    return []


def _rdp(points: list[list[float]], epsilon: float) -> list[list[float]]:
    if len(points) < 3:
        return points
    start, end = points[0], points[-1]
    dx, dy = end[0] - start[0], end[1] - start[1]
    norm = math.hypot(dx, dy) or 1.0
    far_i, far_d = 0, 0.0
    for i, (x, y) in enumerate(points[1:-1], start=1):
        dist = abs(dy * x - dx * y + end[0] * start[1] - end[1] * start[0]) / norm
        if dist > far_d:
            far_i, far_d = i, dist
    if far_d > epsilon:
        left = _rdp(points[: far_i + 1], epsilon)
        right = _rdp(points[far_i:], epsilon)
        return left[:-1] + right
    return [start, end]


def _close(ring: list[list[float]]) -> list[list[float]]:
    if ring and ring[0] != ring[-1]:
        ring = [*ring, ring[0]]
    return ring


def _simplify(ring: list[list[float]]) -> list[list[float]]:
    if len(ring) >= 2 and ring[0] == ring[-1]:
        open_ring = ring[:-1]
    else:
        open_ring = ring
    return _close(_rdp(open_ring, 0.00008))


def _depot(ring: list[list[float]]) -> list[float]:
    return min(ring, key=lambda p: p[0] + p[1])


def _slug(name: str) -> str:
    return name.lower().replace(" ", "-")


def main() -> None:
    states: dict[str, dict] = {}
    written = 0
    for fips, code in REQUESTS:
        state = STATES[fips]
        slug, label = CROPS[code]
        features = []
        seen: set[str] = set()
        for lo, hi, order in BANDS:
            payload = _query(fips, code, lo, hi, order)
            for feature in payload.get("features") or []:
                unique = str((feature.get("properties") or {}).get("CSBID") or "")
                if unique and unique not in seen:
                    seen.add(unique)
                    features.append(feature)
        if not features:
            print(f"skip {state} {label}: no fields in the sample bands")
            continue
        for feature in features:
            props = feature.get("properties") or {}
            unique = str(props.get("CSBID") or "").strip()
            acres = float(props.get("CSBACRES") or 0)
            county = str(props.get("CNTY") or "").strip()
            ring = _simplify(_ring(feature.get("geometry") or {}))
            if len(ring) < 4 or not unique.isdigit():
                print(f"skip {unique or '?'}: empty ring")
                continue
            field_id = f"csb_2024_{unique}"
            rel = f"{_slug(state)}/{slug}/{field_id}.geojson"
            path = OUT / rel
            path.parent.mkdir(parents=True, exist_ok=True)
            geo = {
                "type": "FeatureCollection",
                "features": [
                    {
                        "type": "Feature",
                        "properties": {
                            "name": f"{county} {state} {label.lower()} · {acres:.1f} ac",
                            "role": "cover",
                            "source": "USDA NASS Crop Sequence Boundaries 2024",
                            "year": 2024,
                            "state": state,
                            "county": county,
                            "crop": label,
                            "crop_code": code,
                            "unique_id": unique,
                            "acres": round(acres, 2),
                            "vertical": "generic",
                        },
                        "geometry": {"type": "Polygon", "coordinates": [ring]},
                    },
                    {
                        "type": "Feature",
                        "properties": {"depot": True, "role": "depot"},
                        "geometry": {"type": "Point", "coordinates": _depot(ring)},
                    },
                ],
            }
            path.write_text(json.dumps(geo, indent=2) + "\n")
            bucket = states.setdefault(
                fips, {"state": state, "fips": fips, "crops": {}}
            )
            crop_bucket = bucket["crops"].setdefault(
                code, {"crop": label, "code": code, "slug": slug, "fields": []}
            )
            crop_bucket["fields"].append(
                {
                    "id": field_id,
                    "unique_id": unique,
                    "acres": round(acres, 2),
                    "state": state,
                    "county": county,
                    "crop": label,
                    "code": code,
                    "vertical": "generic",
                    "path": rel,
                }
            )
            written += 1
            print(f"wrote {rel} ({county}, {acres:.1f} ac, {len(ring)} verts)")

    catalog = {
        "source": "USDA NASS Crop Sequence Boundaries 2024",
        "year": 2024,
        "service": (
            "https://services3.arcgis.com/g6eV2CrSSwCZj8Mc/arcgis/rest/services/"
            "NASS_Crop_Sequence_Boundaries_2024/FeatureServer"
        ),
        "attribution": (
            "USDA National Agricultural Statistics Service, Crop Sequence "
            "Boundaries 2024. Synthetic field polygons from the Cropland Data "
            "Layer for the lower 48. This is a public sample, not the national file."
        ),
        "organization": (
            "examples/sites/csb/2024/<state>/<crop>/<field>.geojson. "
            "California orchards stay in the DWR 2023 catalog."
        ),
        "states": [
            {
                "state": states[fips]["state"],
                "fips": fips,
                "source": "USDA NASS Crop Sequence Boundaries 2024",
                "crops": [
                    states[fips]["crops"][code]
                    for _fips, code in REQUESTS
                    if _fips == fips and code in states[fips]["crops"]
                ],
            }
            for fips in STATES
            if fips in states
        ],
    }
    OUT.mkdir(parents=True, exist_ok=True)
    (OUT / "catalog.json").write_text(json.dumps(catalog, indent=2) + "\n")
    print(f"catalog {written} fields")


if __name__ == "__main__":
    main()
