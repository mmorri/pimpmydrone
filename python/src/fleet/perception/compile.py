"""Compile perception detections into a site mission GeoJSON + report."""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import Any, Mapping, Optional

from shapely.geometry import Point, Polygon, box, mapping, shape
from shapely.ops import unary_union

from .ontology import DEFAULT_ONTOLOGY, resolve_role
from .schema import validate_perception

# ~ metres → degrees at mid-latitudes (good enough for inspect buffers).
_M_PER_DEG = 111_320.0
_DEFAULT_POINT_BUFFER_M = 8.0
_AVOID_POINT_BUFFER_M = 12.0


@dataclass
class CompileResult:
    site_geojson: dict[str, Any]
    report: dict[str, Any]
    normalized: dict[str, Any] = field(default_factory=dict)


def compile_detections(
    perception: dict[str, Any],
    *,
    ontology: str = DEFAULT_ONTOLOGY,
    role_map: Optional[Mapping[str, str]] = None,
    min_confidence: float = 0.25,
    point_buffer_m: float = _DEFAULT_POINT_BUFFER_M,
) -> CompileResult:
    """Turn a perception FeatureCollection into planning site GeoJSON.

    Roles:
    - cover → coverage polygons (union)
    - avoid → keep-out polygons (also punched from cover when overlapping)
    - inspect → Point/Polygon features retained for operator review
    - depot → Point features tagged for depot placement
    - track → treated like inspect unless remapped
    """

    errors = validate_perception(perception)
    if errors:
        raise ValueError("Invalid perception payload: " + "; ".join(errors[:8]))

    cover_polys: list[Polygon] = []
    avoid_polys: list[Polygon] = []
    out_features: list[dict[str, Any]] = []
    counts: dict[str, int] = {
        "cover": 0,
        "avoid": 0,
        "inspect": 0,
        "depot": 0,
        "track": 0,
        "dropped_low_confidence": 0,
    }
    by_class: dict[str, int] = {}
    sources: set[str] = set()
    normalized_features: list[dict[str, Any]] = []

    for feature in perception.get("features", []):
        props = dict(feature.get("properties") or {})
        conf = float(props.get("confidence") or 0.0)
        if conf < min_confidence:
            counts["dropped_low_confidence"] += 1
            continue
        class_name = str(props.get("class") or "unknown")
        meta = resolve_role(
            class_name,
            ontology=ontology,
            explicit_role=props.get("role"),
            role_map=role_map,
        )
        role = str(meta.get("role") or props.get("role") or "inspect")
        if role == "track":
            role = "inspect"
        props = {
            **props,
            **{k: v for k, v in meta.items() if k != "role" and k not in props},
            "class": class_name,
            "role": role,
            "confidence": conf,
            "source": props.get("source") or "manual",
        }
        sources.add(str(props["source"]))
        by_class[class_name] = by_class.get(class_name, 0) + 1
        geom = feature.get("geometry")
        if geom is None:
            continue
        shaped = shape(geom)
        if shaped.is_empty:
            continue

        mid_lat = _mid_lat(shaped)
        buffer_m = point_buffer_m
        if role == "avoid":
            buffer_m = max(point_buffer_m, _AVOID_POINT_BUFFER_M)

        poly = _as_polygon(shaped, mid_lat, buffer_m)
        counts[role] = counts.get(role, 0) + 1

        if role == "cover" and poly is not None:
            cover_polys.append(poly)
            out_features.append(_feature(mapping(poly), props))
        elif role == "avoid" and poly is not None:
            avoid_polys.append(poly)
            out_features.append(_feature(mapping(poly), props))
        elif role == "depot":
            pt = shaped if shaped.geom_type == "Point" else shaped.representative_point()
            depot_props = {**props, "depot": True, "role": "depot"}
            out_features.append(
                _feature(mapping(pt), depot_props)
            )
        else:  # inspect
            if shaped.geom_type == "Point":
                out_features.append(_feature(mapping(shaped), props))
            elif poly is not None:
                out_features.append(_feature(mapping(poly), props))
                # Also drop a point at the centroid for easy GCS markers.
                out_features.append(
                    _feature(
                        mapping(poly.representative_point()),
                        {**props, "marker": True},
                    )
                )
            else:
                out_features.append(_feature(mapping(shaped), props))

        normalized_features.append(
            {"type": "Feature", "geometry": geom, "properties": props}
        )

    if cover_polys:
        cover = unary_union(cover_polys)
    else:
        # Fallback: bbox of everything we kept, expanded slightly.
        all_geoms = []
        for f in out_features:
            g = f.get("geometry")
            if g:
                all_geoms.append(shape(g))
        if not all_geoms:
            raise ValueError("No usable detections after confidence filter")
        cover = box(*unary_union(all_geoms).bounds).buffer(
            _deg_buffer(5.0, _mid_lat(unary_union(all_geoms)))
        )

    avoid = unary_union(avoid_polys) if avoid_polys else None
    if avoid is not None and not avoid.is_empty:
        cover = cover.difference(avoid)
        if cover.is_empty:
            raise ValueError("Avoid zones removed the entire coverage area")

    if cover.geom_type == "MultiPolygon":
        cover = max(cover.geoms, key=lambda g: g.area)

    # Coverage polygon first so extract_polygons / planners see the site.
    site_features = [
        _feature(
            mapping(cover),
            {"role": "cover", "class": "compiled_coverage", "source": "manual"},
        )
    ]
    # Avoid polygons retained as role=avoid for no_fly rasterisation.
    if avoid is not None and not avoid.is_empty:
        if avoid.geom_type == "Polygon":
            site_features.append(
                _feature(
                    mapping(avoid),
                    {"role": "avoid", "class": "compiled_avoid", "source": "manual"},
                )
            )
        elif avoid.geom_type == "MultiPolygon":
            for part in avoid.geoms:
                site_features.append(
                    _feature(
                        mapping(part),
                        {"role": "avoid", "class": "compiled_avoid", "source": "manual"},
                    )
                )
    site_features.extend(
        f
        for f in out_features
        if (f.get("properties") or {}).get("role") in {"inspect", "depot"}
    )

    site_geojson = {"type": "FeatureCollection", "features": site_features}
    report = {
        "ontology": ontology,
        "min_confidence": min_confidence,
        "counts": counts,
        "by_class": by_class,
        "sources": sorted(sources),
        "cover_area_deg2": float(cover.area),
        "disclaimer": (
            "Perception outputs are experimental and untrusted. False positives and "
            "false negatives are expected. This is NOT a sense-and-avoid system."
        ),
    }
    normalized = {"type": "FeatureCollection", "features": normalized_features}
    return CompileResult(
        site_geojson=site_geojson, report=report, normalized=normalized
    )


def _feature(geometry: dict[str, Any], props: dict[str, Any]) -> dict[str, Any]:
    return {"type": "Feature", "geometry": geometry, "properties": props}


def _mid_lat(geom: Any) -> float:
    try:
        return float(geom.centroid.y)
    except Exception:  # noqa: BLE001
        return 0.0


def _deg_buffer(metres: float, lat: float) -> float:
    return metres / max(_M_PER_DEG * max(0.2, math.cos(math.radians(lat))), 1.0)


def _as_polygon(geom: Any, lat: float, buffer_m: float) -> Optional[Polygon]:
    if geom.geom_type == "Polygon":
        return geom
    if geom.geom_type == "MultiPolygon":
        return max(geom.geoms, key=lambda g: g.area)
    if geom.geom_type == "Point":
        return geom.buffer(_deg_buffer(buffer_m, lat))
    try:
        return geom.convex_hull if geom.geom_type == "LineString" else None
    except Exception:  # noqa: BLE001
        return None


__all__ = ["CompileResult", "compile_detections"]
