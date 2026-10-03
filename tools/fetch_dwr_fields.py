#!/usr/bin/env python3
"""Pull a hangar-sized sample of DWR 2023 crop fields and file them by county/crop.

The statewide layer is hundreds of megabytes. This script queries the public
2023 map service and writes one GeoJSON per field plus catalog.json. Re-run to
refresh the committed sample; do not vendor the full shapefile.
"""

from __future__ import annotations

import json
import math
import urllib.parse
import urllib.request
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "examples" / "sites" / "dwr" / "2023"
SERVICE = (
    "https://utility.arcgis.com/usrsvcs/servers/"
    "d94e891e00364e49a2ed9e9e2e27837d/rest/services/Planning/"
    "i15_Crop_Mapping_2023/MapServer/0/query"
)

# Remote-sensing codes from the DWR land-use legend (MAIN_CROP).
CROPS = {
    "D12": ("almonds", "Almonds"),
    "D13": ("walnuts", "Walnuts"),
    "D14": ("pistachios", "Pistachios"),
    "V": ("grapes", "Grapes"),
}

# Two fields per bucket, about 18–32 acres so an 8 m grid stays under the hangar cap.
REQUESTS = [
    ("Fresno", "D12"),
    ("Fresno", "D14"),
    ("Fresno", "V"),
    ("Kern", "D12"),
    ("Kern", "D14"),
    ("Tulare", "D13"),
]
# One smaller block and one closer to 30 acres, so a crop folder is not two copies.
BANDS = ((18, 22, "ACRES ASC"), (27, 32, "ACRES ASC"))


def _query(county: str, code: str, lo: float, hi: float, order: str) -> dict:
    where = (
        f"COUNTY='{county}' AND MAIN_CROP='{code}' AND ACRES>={lo} AND ACRES<={hi}"
    )
    params = {
        "where": where,
        "outFields": "UniqueID,COUNTY,MAIN_CROP,ACRES",
        "returnGeometry": "true",
        "outSR": "4326",
        "orderByFields": order,
        "resultRecordCount": "1",
        "geometryPrecision": "6",
        "f": "geojson",
    }
    url = SERVICE + "?" + urllib.parse.urlencode(params)
    req = urllib.request.Request(url, headers={"User-Agent": "PimpMyDrone/0.5.12"})
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
    """Simplify an open or closed ring. A closed ring shares its first point, which breaks RDP."""
    if len(ring) >= 2 and ring[0] == ring[-1]:
        open_ring = ring[:-1]
    else:
        open_ring = ring
    return _close(_rdp(open_ring, 0.00005))


def _depot(ring: list[list[float]]) -> list[float]:
    return min(ring, key=lambda p: p[0] + p[1])


def main() -> None:
    counties: dict[str, dict[str, dict]] = {}
    written = 0
    for county, code in REQUESTS:
        slug, label = CROPS[code]
        features = []
        seen: set[str] = set()
        for lo, hi, order in BANDS:
            payload = _query(county, code, lo, hi, order)
            for feature in payload.get("features") or []:
                unique = str((feature.get("properties") or {}).get("UniqueID") or "")
                if unique and unique not in seen:
                    seen.add(unique)
                    features.append(feature)
        if not features:
            print(f"skip {county} {code}: no fields in the sample bands")
            continue
        for feature in features:
            props = feature.get("properties") or {}
            unique = str(props.get("UniqueID") or "").strip()
            acres = float(props.get("ACRES") or 0)
            ring = _simplify(_ring(feature.get("geometry") or {}))
            if len(ring) < 4 or not unique:
                print(f"skip {unique or '?'}: empty ring")
                continue
            field_id = f"dwr_2023_{unique}"
            rel = f"{county.lower()}/{slug}/{field_id}.geojson"
            path = OUT / rel
            path.parent.mkdir(parents=True, exist_ok=True)
            geo = {
                "type": "FeatureCollection",
                "features": [
                    {
                        "type": "Feature",
                        "properties": {
                            "name": f"{county} {label.lower()} · {acres:.1f} ac",
                            "role": "cover",
                            "source": "DWR Statewide Crop Mapping 2023",
                            "year": 2023,
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
            bucket = counties.setdefault(county, {})
            crop_bucket = bucket.setdefault(
                code, {"crop": label, "code": code, "slug": slug, "fields": []}
            )
            crop_bucket["fields"].append(
                {
                    "id": field_id,
                    "unique_id": unique,
                    "acres": round(acres, 2),
                    "county": county,
                    "crop": label,
                    "code": code,
                    "vertical": "generic",
                    "path": rel,
                }
            )
            written += 1
            print(f"wrote {rel} ({acres:.1f} ac, {len(ring)} verts)")

    catalog = {
        "source": "California DWR Statewide Crop Mapping 2023 (final)",
        "year": 2023,
        "service": (
            "https://gis.water.ca.gov/arcgis/rest/services/Planning/"
            "i15_Crop_Mapping_2023/MapServer"
        ),
        "attribution": (
            "California Department of Water Resources, Statewide Crop Mapping. "
            "Field boundaries are a public sample, not the full state layer."
        ),
        "organization": (
            "examples/sites/dwr/2023/<county>/<crop>/<field>.geojson. "
            "Flight lines are not stored here — plan a field and the hangar "
            "writes that field's tours."
        ),
        "counties": [
            {
                "county": county,
                "crops": [bucket[code] for code in CROPS if code in bucket],
            }
            for county, bucket in counties.items()
        ],
    }
    OUT.mkdir(parents=True, exist_ok=True)
    (OUT / "catalog.json").write_text(json.dumps(catalog, indent=2) + "\n")
    print(f"catalog {written} fields")


if __name__ == "__main__":
    main()
