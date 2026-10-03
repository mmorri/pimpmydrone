"""Validate perception FeatureCollections."""

from __future__ import annotations

from typing import Any

ROLES = frozenset({"cover", "avoid", "inspect", "depot", "track"})
SOURCES = frozenset({"yolo", "cnn", "vlm", "domain", "manual", "demo"})
GEOM_TYPES = frozenset({"Point", "Polygon", "MultiPolygon"})


def validate_perception(payload: dict[str, Any]) -> list[str]:
    """Return a list of validation errors (empty means OK)."""

    errors: list[str] = []
    if not isinstance(payload, dict):
        return ["Perception payload must be a JSON object"]
    if payload.get("type") != "FeatureCollection":
        errors.append("type must be FeatureCollection")
    features = payload.get("features")
    if not isinstance(features, list):
        errors.append("features must be an array")
        return errors
    if not features:
        errors.append("features must not be empty")
    for i, feature in enumerate(features):
        prefix = f"features[{i}]"
        if not isinstance(feature, dict):
            errors.append(f"{prefix}: must be an object")
            continue
        if feature.get("type") != "Feature":
            errors.append(f"{prefix}: type must be Feature")
        geom = feature.get("geometry")
        if geom is None:
            errors.append(f"{prefix}: geometry is required")
        elif not isinstance(geom, dict):
            errors.append(f"{prefix}: geometry must be an object")
        else:
            gtype = geom.get("type")
            if gtype not in GEOM_TYPES:
                errors.append(
                    f"{prefix}: geometry.type must be Point, Polygon, or MultiPolygon"
                )
            if "coordinates" not in geom:
                errors.append(f"{prefix}: geometry.coordinates is required")
        props = feature.get("properties")
        if not isinstance(props, dict):
            errors.append(f"{prefix}: properties must be an object")
            continue
        if not str(props.get("class") or "").strip():
            errors.append(f"{prefix}: properties.class is required")
        conf = props.get("confidence")
        if not isinstance(conf, (int, float)) or conf < 0 or conf > 1:
            errors.append(f"{prefix}: properties.confidence must be in [0, 1]")
        role = props.get("role")
        if role not in ROLES:
            errors.append(f"{prefix}: properties.role must be one of {sorted(ROLES)}")
        source = props.get("source")
        if source not in SOURCES:
            errors.append(
                f"{prefix}: properties.source must be one of {sorted(SOURCES)}"
            )
        pri = props.get("priority")
        if pri is not None and (
            not isinstance(pri, (int, float)) or pri < 0 or pri > 1
        ):
            errors.append(f"{prefix}: properties.priority must be in [0, 1] when set")
    return errors


__all__ = ["GEOM_TYPES", "ROLES", "SOURCES", "validate_perception"]
