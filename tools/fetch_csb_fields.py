#!/usr/bin/env python3
"""Pull a hangar-sized US sample from USDA NASS Crop Sequence Boundaries 2024.

CSB polygons cover the lower 48. This script keeps two fields per state/crop
(about 18 and 27 acres) under examples/sites/csb/2024/<state>/<crop>/.
California stays on the surveyed DWR catalog.

The default run refreshes the states the public feature service actually
serves. ``--national`` reads Arkansas, Georgia, Illinois, Kansas, Nebraska,
and Texas from a cache of the national geodatabase at
~/.cache/pimpmydrone/csb/. That file is not written into the repo.
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
    2: ("cotton", "Cotton"),
    3: ("rice", "Rice"),
    5: ("soybeans", "Soybeans"),
    10: ("peanuts", "Peanuts"),
    23: ("spring-wheat", "Spring wheat"),
    24: ("winter-wheat", "Winter wheat"),
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
# Hangar sample pulled from the national geodatabase (cached outside the repo).
# The public feature service does not serve these states.
NATIONAL = [
    ("17", "Illinois", 1),
    ("17", "Illinois", 5),
    ("20", "Kansas", 24),
    ("31", "Nebraska", 1),
    ("48", "Texas", 2),
    ("05", "Arkansas", 3),
    ("13", "Georgia", 10),
]
CACHE = Path.home() / ".cache" / "pimpmydrone" / "csb"
ZIP_NAME = "NationalCSB_2017-2024_rev23.zip"
ZIP_URL = (
    "https://www.nass.usda.gov/Research_and_Science/Crop-Sequence-Boundaries/"
    f"datasets/{ZIP_NAME}"
)
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


def _write_field(
    states: dict[str, dict],
    *,
    fips: str,
    state: str,
    code: int,
    unique: str,
    acres: float,
    county: str,
    ring: list[list[float]],
    sample: str,
) -> bool:
    slug, label = CROPS[code]
    if len(ring) < 4 or not unique.isdigit():
        print(f"skip {unique or '?'}: empty ring")
        return False
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
                    "sample": sample,
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
        fips,
        {"state": state, "fips": fips, "sample": sample, "crops": {}},
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
    print(f"wrote {rel} ({county}, {acres:.1f} ac, {len(ring)} verts)")
    return True


def _state_record(bucket: dict, order: list[tuple[str, int]]) -> dict:
    fips = bucket["fips"]
    return {
        "state": bucket["state"],
        "fips": fips,
        "sample": bucket.get("sample") or "feature-service",
        "source": "USDA NASS Crop Sequence Boundaries 2024",
        "crops": [
            bucket["crops"][code]
            for pair_fips, code in order
            if pair_fips == fips and code in bucket["crops"]
        ],
    }


def _merge_catalog(updates: list[dict]) -> None:
    path = OUT / "catalog.json"
    existing = json.loads(path.read_text()) if path.exists() else {}
    by_fips = {row["fips"]: row for row in existing.get("states") or []}
    for row in updates:
        by_fips[row["fips"]] = row
    states = sorted(by_fips.values(), key=lambda row: row["state"])
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
            "California orchards stay in the DWR 2023 catalog. "
            "States the feature service does not serve are read from a local "
            "cache of the national geodatabase, which is not in git."
        ),
        "states": states,
    }
    OUT.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(catalog, indent=2) + "\n")
    fields = sum(len(crop["fields"]) for state in states for crop in state["crops"])
    print(f"catalog {fields} fields across {len(states)} states")


def _ensure_gdb() -> Path:
    found = sorted(CACHE.rglob("*.gdb"))
    if found:
        return found[0]
    zip_path = CACHE / ZIP_NAME
    if not zip_path.exists():
        CACHE.mkdir(parents=True, exist_ok=True)
        print(f"downloading {ZIP_URL} to {zip_path}")
        urllib.request.urlretrieve(ZIP_URL, zip_path)
    print(f"unzipping {zip_path}")
    import zipfile

    with zipfile.ZipFile(zip_path) as archive:
        archive.extractall(CACHE)
    found = sorted(CACHE.rglob("*.gdb"))
    if not found:
        raise SystemExit(f"no file geodatabase under {CACHE}")
    return found[0]


def _to_lonlat(transformer, xs: list[float], ys: list[float]) -> list[list[float]]:
    lon, lat = transformer.transform(xs, ys)
    return [[round(float(x), 5), round(float(y), 5)] for x, y in zip(lon, lat, strict=False)]


def _national_ring(geom, transformer) -> list[list[float]]:
    if geom is None or geom.is_empty:
        return []
    if geom.geom_type == "MultiPolygon":
        geom = max(geom.geoms, key=lambda part: part.area)
    if geom.geom_type != "Polygon":
        return []
    coords = list(geom.exterior.coords)
    xs = [point[0] for point in coords]
    ys = [point[1] for point in coords]
    return _simplify(_to_lonlat(transformer, xs, ys))


def fetch_national(names: list[str] | None) -> None:
    """Read a hangar sample for named states from the cached national gdb."""
    try:
        import pyogrio
        import pyogrio.raw
        from pyproj import Transformer
        from shapely import from_wkb
    except ImportError as exc:
        raise SystemExit(
            "national samples need pyogrio, pyproj, and shapely "
            "(uv venv && uv pip install pyogrio pyproj shapely)"
        ) from exc

    wanted = {name.lower() for name in names} if names else None
    jobs = [
        (fips, state, code)
        for fips, state, code in NATIONAL
        if wanted is None or state.lower() in wanted or fips in (wanted or set())
    ]
    if not jobs:
        known = ", ".join(sorted({state for _fips, state, _code in NATIONAL}))
        raise SystemExit(f"unknown state. Choose from: {known}")

    gdb = _ensure_gdb()
    layers = pyogrio.list_layers(gdb)
    layer = str(layers[0][0])
    info = pyogrio.read_info(gdb, layer=layer)
    transformer = Transformer.from_crs(info["crs"], "EPSG:4326", always_xy=True)
    dtypes = list(info["dtypes"])
    fips_type = str(dtypes[list(info["fields"]).index("STATEFIPS")])
    text_fips = "str" in fips_type or "object" in fips_type

    states: dict[str, dict] = {}
    written = 0
    for fips, state, code in jobs:
        slug, label = CROPS[code]
        state_value = f"'{fips}'" if text_fips else str(int(fips))
        seen: set[str] = set()
        for lo, hi, _order in BANDS:
            where = (
                f"STATEFIPS = {state_value} AND CDL2024 = {code} "
                f"AND CSBACRES >= {lo} AND CSBACRES <= {hi}"
            )
            _meta, _fids, geoms, data = pyogrio.raw.read(
                gdb,
                layer=layer,
                where=where,
                max_features=1,
                columns=["CSBID", "CSBACRES", "CDL2024", "STATEFIPS", "CNTY"],
            )
            if geoms is None or len(geoms) == 0:
                print(f"skip {state} {label} {lo}-{hi} ac")
                continue
            names = list(_meta["fields"])
            values = {name: data[index][0] for index, name in enumerate(names)}
            unique = str(values["CSBID"]).split(".")[0]
            if unique in seen:
                continue
            seen.add(unique)
            geom = geoms[0]
            if isinstance(geom, (bytes, bytearray)):
                geom = from_wkb(geom)
            ring = _national_ring(geom, transformer)
            if _write_field(
                states,
                fips=fips,
                state=state,
                code=code,
                unique=unique,
                acres=float(values["CSBACRES"]),
                county=str(values["CNTY"]).strip(),
                ring=ring,
                sample="national-gdb",
            ):
                written += 1
        if not seen:
            print(f"skip {state} {label}: no fields in the sample bands")
    if written == 0:
        raise SystemExit("national sample wrote no fields")
    order = [(fips, code) for fips, _state, code in NATIONAL]
    _merge_catalog([_state_record(states[fips], order) for fips in states])


def main() -> None:
    states: dict[str, dict] = {}
    written = 0
    for fips, code in REQUESTS:
        state = STATES[fips]
        _slug_name, label = CROPS[code]
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
            ring = _simplify(_ring(feature.get("geometry") or {}))
            if _write_field(
                states,
                fips=fips,
                state=state,
                code=code,
                unique=str(props.get("CSBID") or "").strip(),
                acres=float(props.get("CSBACRES") or 0),
                county=str(props.get("CNTY") or "").strip(),
                ring=ring,
                sample="feature-service",
            ):
                written += 1
    if written == 0:
        raise SystemExit("feature service wrote no fields")
    _merge_catalog(
        [_state_record(states[fips], REQUESTS) for fips in STATES if fips in states]
    )


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--state",
        action="append",
        help="Pull this state from the national geodatabase cache (repeatable).",
    )
    parser.add_argument(
        "--national",
        action="store_true",
        help="Pull the hangar sample for every state listed in NATIONAL.",
    )
    args = parser.parse_args()
    if args.national or args.state:
        fetch_national(None if args.national else args.state)
    else:
        main()
