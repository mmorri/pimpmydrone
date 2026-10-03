"""Georeferencing helpers: GeoJSON site → grid map, and cell ↔ lon/lat."""

from __future__ import annotations

import math
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Any, Iterable, Sequence

from shapely.geometry import Point, Polygon, mapping, shape
from shapely.ops import unary_union

from . import io
from .generate_map import cell_to_json

# WGS84 metres per degree at the equator; lon is scaled by cos(lat).
_METRES_PER_DEG_LAT = 111_320.0


@dataclass(frozen=True)
class GeoTransform:
    """Affine map from grid cell centres to WGS84 degrees."""

    origin_lon: float
    origin_lat: float
    cell_size_m: float
    width: int
    height: int
    vertical: str = "generic"
    altitude_m: float = 40.0

    @property
    def metres_per_deg_lon(self) -> float:
        return _METRES_PER_DEG_LAT * max(0.2, math.cos(math.radians(self.origin_lat)))

    def cell_to_lonlat(self, x: int, y: int) -> tuple[float, float]:
        east_m = (x + 0.5) * self.cell_size_m
        north_m = (y + 0.5) * self.cell_size_m
        lon = self.origin_lon + east_m / self.metres_per_deg_lon
        lat = self.origin_lat + north_m / _METRES_PER_DEG_LAT
        return lon, lat

    def lonlat_to_cell(self, lon: float, lat: float) -> tuple[int, int]:
        east_m = (lon - self.origin_lon) * self.metres_per_deg_lon
        north_m = (lat - self.origin_lat) * _METRES_PER_DEG_LAT
        x = int(east_m // self.cell_size_m)
        y = int(north_m // self.cell_size_m)
        return x, y

    def to_json(self) -> dict[str, Any]:
        return asdict(self)

    @classmethod
    def from_json(cls, payload: dict[str, Any]) -> "GeoTransform":
        return cls(
            origin_lon=float(payload["origin_lon"]),
            origin_lat=float(payload["origin_lat"]),
            cell_size_m=float(payload["cell_size_m"]),
            width=int(payload["width"]),
            height=int(payload["height"]),
            vertical=str(payload.get("vertical", "generic")),
            altitude_m=float(payload.get("altitude_m", 40.0)),
        )


def load_geojson(path: Path) -> dict[str, Any]:
    return io.read_json(path)


def extract_polygons(geojson: dict[str, Any]) -> list[Polygon]:
    """Pull coverage polygons out of a Feature, FeatureCollection or Geometry.

    When features carry ``properties.role``, only ``cover`` (and untagged)
    polygons are treated as the coverage area; ``avoid`` polygons are ignored
    here and handled by :func:`extract_avoid_polygons`.
    """

    polygons = _polygons_with_roles(geojson, include_roles={"cover", None, ""})
    if not polygons:
        # Legacy GeoJSON with no roles: take every polygon.
        polygons = _polygons_with_roles(geojson, include_roles=None)
    if not polygons:
        raise ValueError("GeoJSON contains no polygonal coverage area")
    return polygons


def extract_avoid_polygons(geojson: dict[str, Any]) -> list[Polygon]:
    """Polygons tagged ``properties.role == 'avoid'`` (keep-out / no-fly)."""

    return _polygons_with_roles(geojson, include_roles={"avoid"})


def extract_depot_lonlats(geojson: dict[str, Any]) -> list[tuple[float, float]]:
    """Optional Point features tagged as depots (properties.depot / role=depot)."""

    if geojson.get("type") != "FeatureCollection":
        return []
    depots: list[tuple[float, float]] = []
    for feature in geojson.get("features", []):
        props = feature.get("properties") or {}
        geom = feature.get("geometry") or {}
        is_depot = bool(props.get("depot")) or props.get("role") == "depot"
        if not is_depot or geom.get("type") != "Point":
            continue
        lon, lat = geom["coordinates"][:2]
        depots.append((float(lon), float(lat)))
    return depots


def _polygons_with_roles(
    geojson: dict[str, Any],
    *,
    include_roles: set[str | None] | None,
) -> list[Polygon]:
    items: list[tuple[Any, str | None]] = []
    kind = geojson.get("type")
    if kind == "FeatureCollection":
        for feature in geojson.get("features", []):
            props = feature.get("properties") or {}
            role = props.get("role")
            if role is not None:
                role = str(role)
            items.append((feature.get("geometry"), role))
    elif kind == "Feature":
        props = geojson.get("properties") or {}
        role = props.get("role")
        if role is not None:
            role = str(role)
        items.append((geojson.get("geometry"), role))
    elif kind in {"Polygon", "MultiPolygon", "GeometryCollection"}:
        items.append((geojson, None))
    else:
        raise ValueError(f"Unsupported GeoJSON type: {kind!r}")

    polygons: list[Polygon] = []
    for geom, role in items:
        if include_roles is not None and role not in include_roles:
            continue
        if geom is None:
            continue
        shaped = shape(geom)
        if shaped.is_empty:
            continue
        if shaped.geom_type == "Polygon":
            polygons.append(shaped)
        elif shaped.geom_type == "MultiPolygon":
            polygons.extend(shaped.geoms)
        elif shaped.geom_type == "GeometryCollection":
            for part in shaped.geoms:
                if part.geom_type == "Polygon":
                    polygons.append(part)
                elif part.geom_type == "MultiPolygon":
                    polygons.extend(part.geoms)
    return polygons


def choose_cell_size(
    site: Polygon,
    *,
    cell_size_m: float | None,
    max_cells: int,
) -> float:
    minx, miny, maxx, maxy = site.bounds
    mid_lat = (miny + maxy) / 2.0
    width_m = (maxx - minx) * _METRES_PER_DEG_LAT * max(
        0.2, math.cos(math.radians(mid_lat))
    )
    height_m = (maxy - miny) * _METRES_PER_DEG_LAT
    side_w = max(width_m, 1.0)
    side_h = max(height_m, 1.0)
    # Fit roughly into max_cells (plus padding margin).
    auto = max(1.0, math.sqrt((side_w * side_h) / max(max_cells * 0.75, 4)))
    if cell_size_m is None or cell_size_m <= 0:
        return auto
    # Honour the request unless it would explode the free/hosting cap.
    estimated = math.ceil(side_w / cell_size_m) * math.ceil(side_h / cell_size_m)
    if estimated > max_cells:
        return max(cell_size_m, auto)
    return float(cell_size_m)

def rasterize_site(
    site: Polygon,
    *,
    cell_size_m: float,
    depot_lonlats: Sequence[tuple[float, float]] | None = None,
    avoid_polygons: Sequence[Polygon] | None = None,
    vertical: str = "generic",
    altitude_m: float = 40.0,
    seed: int = 0,
    speed: float = 8.0,
    battery: float = 1200.0,
    takeoff_cost: float = 10.0,
    landing_cost: float = 10.0,
    max_drones: int = 4,
) -> tuple[dict[str, Any], dict[str, Any], GeoTransform]:
    """Convert a WGS84 polygon into ``map.json`` / ``params.json`` + geo metadata."""

    minx, miny, maxx, maxy = site.bounds
    # Pad one cell so depots on the boundary stay in-bounds.
    pad_m = cell_size_m
    mid_lat = (miny + maxy) / 2.0
    m_per_lon = _METRES_PER_DEG_LAT * max(0.2, math.cos(math.radians(mid_lat)))
    origin_lon = minx - pad_m / m_per_lon
    origin_lat = miny - pad_m / _METRES_PER_DEG_LAT
    width_m = (maxx - minx) * m_per_lon + 2 * pad_m
    height_m = (maxy - miny) * _METRES_PER_DEG_LAT + 2 * pad_m
    width = max(2, int(math.ceil(width_m / cell_size_m)))
    height = max(2, int(math.ceil(height_m / cell_size_m)))

    geo = GeoTransform(
        origin_lon=origin_lon,
        origin_lat=origin_lat,
        cell_size_m=cell_size_m,
        width=width,
        height=height,
        vertical=vertical,
        altitude_m=altitude_m,
    )

    avoid = list(avoid_polygons or [])
    free: list[tuple[int, int]] = []
    obstacles: list[tuple[int, int]] = []
    for y in range(height):
        for x in range(width):
            lon, lat = geo.cell_to_lonlat(x, y)
            cell_poly = _cell_polygon(geo, x, y)
            in_site = site.covers(Point(lon, lat)) or site.intersects(cell_poly)
            in_avoid = any(
                a.covers(Point(lon, lat)) or a.intersects(cell_poly) for a in avoid
            )
            if in_site and not in_avoid:
                free.append((x, y))
            else:
                obstacles.append((x, y))

    if not free:
        raise ValueError("Rasterisation produced no free cells; try a finer cell size")

    free_set = set(free)
    depots = _place_depots(geo, free_set, site, depot_lonlats, vertical)
    no_fly = [_lonlat_poly_to_grid(geo, poly) for poly in avoid]
    no_fly = [p for p in no_fly if p is not None]

    map_payload = {
        "width": width,
        "height": height,
        "seed": seed,
        "obstacles": [cell_to_json(cell) for cell in obstacles],
        "no_fly": no_fly,
        "depots": [cell_to_json(cell) for cell in depots],
        # Drone kinematics live only in params.json (no duplicated block).
    }
    params_payload = {
        "v": speed,
        "B": battery,
        "takeoff_cost": takeoff_cost,
        "landing_cost": landing_cost,
        "max_drones": max_drones,
    }
    return map_payload, params_payload, geo


def write_site_workspace(
    out_dir: Path,
    geojson: dict[str, Any],
    *,
    cell_size_m: float | None = None,
    max_cells: int = 8_000,
    vertical: str = "generic",
    altitude_m: float = 40.0,
    seed: int = 0,
    speed: float = 8.0,
    battery: float = 1200.0,
    takeoff_cost: float = 10.0,
    landing_cost: float = 10.0,
    max_drones: int = 4,
) -> tuple[Path, Path, Path, GeoTransform]:
    """Rasterise a site GeoJSON into ``out_dir`` and return artefact paths."""

    polygons = extract_polygons(geojson)
    avoid_polygons = extract_avoid_polygons(geojson)
    site = unary_union(polygons)
    if site.geom_type == "MultiPolygon":
        # Coverage planner expects one connected workspace; take the largest piece.
        site = max(site.geoms, key=lambda g: g.area)
    if avoid_polygons:
        avoid_union = unary_union(avoid_polygons)
        site = site.difference(avoid_union)
        if site.is_empty:
            raise ValueError("Avoid zones removed the entire coverage area")
        if site.geom_type == "MultiPolygon":
            site = max(site.geoms, key=lambda g: g.area)
    depot_lonlats = extract_depot_lonlats(geojson)
    size = choose_cell_size(site, cell_size_m=cell_size_m, max_cells=max_cells)
    map_payload, params_payload, geo = rasterize_site(
        site,
        cell_size_m=size,
        depot_lonlats=depot_lonlats,
        avoid_polygons=avoid_polygons,
        vertical=vertical,
        altitude_m=altitude_m,
        seed=seed,
        speed=speed,
        battery=battery,
        takeoff_cost=takeoff_cost,
        landing_cost=landing_cost,
        max_drones=max_drones,
    )
    io.ensure_directory(out_dir)
    map_path = out_dir / "map.json"
    params_path = out_dir / "params.json"
    geo_path = out_dir / "geo.json"
    geo_payload = geo.to_json()
    geo_payload["site"] = mapping(site)
    geo_payload["altitude_mode"] = "AGL"
    if avoid_polygons:
        geo_payload["avoid"] = [mapping(poly) for poly in avoid_polygons]
    # Default nadir inspection sensor for the vertical (overridable in params).
    geo_payload["sensor"] = {
        "fov_h_deg": 60.0 if vertical != "security" else 80.0,
        "fov_v_deg": 45.0 if vertical != "security" else 60.0,
        "forward_overlap": 0.2,
        "side_overlap": 0.2,
    }
    params_payload["sensor"] = {
        **geo_payload["sensor"],
        "altitude_m": altitude_m,
        "cell_size_m": size,
    }
    params_payload["altitude_m"] = altitude_m
    params_payload["cell_size_m"] = size
    io.write_json(map_path, map_payload)
    io.write_json(params_path, params_payload)
    io.write_json(geo_path, geo_payload)
    return map_path, params_path, geo_path, geo


def _lonlat_poly_to_grid(geo: GeoTransform, poly: Polygon) -> dict[str, Any] | None:
    """Project a WGS84 polygon into grid-coordinate vertices for ``map.json`` no_fly."""

    if poly.is_empty or poly.geom_type != "Polygon":
        return None
    vertices = []
    for lon, lat in poly.exterior.coords:
        x, y = geo.lonlat_to_cell(float(lon), float(lat))
        # Clamp into the grid so the rasteriser always sees something usable.
        x = max(0, min(geo.width - 1, x))
        y = max(0, min(geo.height - 1, y))
        vertices.append({"x": float(x), "y": float(y)})
    if len(vertices) < 3:
        return None
    return {"vertices": vertices}


def _cell_polygon(geo: GeoTransform, x: int, y: int) -> Polygon:
    lon0 = geo.origin_lon + x * geo.cell_size_m / geo.metres_per_deg_lon
    lat0 = geo.origin_lat + y * geo.cell_size_m / _METRES_PER_DEG_LAT
    lon1 = geo.origin_lon + (x + 1) * geo.cell_size_m / geo.metres_per_deg_lon
    lat1 = geo.origin_lat + (y + 1) * geo.cell_size_m / _METRES_PER_DEG_LAT
    return Polygon([(lon0, lat0), (lon1, lat0), (lon1, lat1), (lon0, lat1)])


def _place_depots(
    geo: GeoTransform,
    free: set[tuple[int, int]],
    site: Polygon,
    depot_lonlats: Sequence[tuple[float, float]] | None,
    vertical: str,
) -> list[tuple[int, int]]:
    if depot_lonlats:
        placed: list[tuple[int, int]] = []
        for lon, lat in depot_lonlats:
            cell = geo.lonlat_to_cell(lon, lat)
            placed.append(_nearest_free(cell, free))
        return _unique(placed)

    if vertical == "security":
        # Entry-style depots: south-west and north-east extremes of the free set.
        sw = min(free, key=lambda c: c[0] + c[1])
        ne = max(free, key=lambda c: c[0] + c[1])
        return _unique([sw, ne])

    # Solar / generic: one depot near the polygon representative point.
    lon, lat = site.representative_point().coords[0]
    return [_nearest_free(geo.lonlat_to_cell(lon, lat), free)]


def _nearest_free(cell: tuple[int, int], free: set[tuple[int, int]]) -> tuple[int, int]:
    if cell in free:
        return cell
    cx, cy = cell
    return min(free, key=lambda c: (c[0] - cx) ** 2 + (c[1] - cy) ** 2)


def _unique(cells: Iterable[tuple[int, int]]) -> list[tuple[int, int]]:
    seen: set[tuple[int, int]] = set()
    out: list[tuple[int, int]] = []
    for cell in cells:
        if cell not in seen:
            seen.add(cell)
            out.append(cell)
    return out


__all__ = [
    "GeoTransform",
    "choose_cell_size",
    "extract_avoid_polygons",
    "extract_depot_lonlats",
    "extract_polygons",
    "load_geojson",
    "rasterize_site",
    "write_site_workspace",
]
