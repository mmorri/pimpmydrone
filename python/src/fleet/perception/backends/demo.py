"""Deterministic synthetic detections for CI and the hangar demo."""

from __future__ import annotations

from typing import Any

from ..ontology import resolve_role
from .base import PerceptionInput


class DemoBackend:
    name = "demo"

    def status(self) -> dict[str, Any]:
        return {
            "installed": True,
            "auto": True,
            "detail": "Built-in synthetic detections (no model weights)",
        }

    def detect(self, inputs: PerceptionInput) -> dict[str, Any]:
        ontology = inputs.ontology or "solar"
        # Stable site near Alameda for hangar demos.
        lon0, lat0 = inputs.center_lonlat or (-122.2515, 37.7735)
        specs = _demo_specs(ontology, lon0, lat0)
        features = []
        for class_name, role_hint, ring_or_point, conf in specs:
            meta = resolve_role(class_name, ontology=ontology, explicit_role=role_hint)
            role = meta["role"]
            props = {
                "class": class_name,
                "confidence": conf,
                "role": role,
                "source": "demo",
                "model": "demo-v1",
            }
            if "priority" in meta:
                props["priority"] = meta["priority"]
            if meta.get("thermal"):
                props["thermal"] = True
            if isinstance(ring_or_point[0], (int, float)):
                geom = {"type": "Point", "coordinates": list(ring_or_point)}
            else:
                geom = {"type": "Polygon", "coordinates": [ring_or_point]}
            features.append({"type": "Feature", "geometry": geom, "properties": props})
        # Honour NL-ish text with a tiny keyword parser when no VLM is configured.
        if inputs.text:
            features.extend(_text_intents(inputs.text, lon0, lat0, ontology))
        return {"type": "FeatureCollection", "features": features}


def _demo_specs(
    ontology: str, lon0: float, lat0: float
) -> list[tuple[str, str | None, Any, float]]:
    dlon, dlat = 0.00045, 0.00035
    panel = [
        [lon0 - dlon, lat0 - dlat],
        [lon0 + dlon, lat0 - dlat],
        [lon0 + dlon, lat0 + dlat],
        [lon0 - dlon, lat0 + dlat],
        [lon0 - dlon, lat0 - dlat],
    ]
    truck = [
        [lon0 + 0.0001, lat0 - 0.00005],
        [lon0 + 0.00022, lat0 - 0.00005],
        [lon0 + 0.00022, lat0 + 0.00005],
        [lon0 + 0.0001, lat0 + 0.00005],
        [lon0 + 0.0001, lat0 - 0.00005],
    ]
    if ontology in {"solar", "generic"}:
        return [
            ("solar_panel", "cover", panel, 0.94),
            ("hotspot", "inspect", (lon0 - 0.0001, lat0 + 0.0001), 0.88),
            ("vehicle", "avoid", truck, 0.81),
            ("depot", "depot", (lon0 - dlon, lat0 - dlat), 1.0),
        ]
    if ontology == "security":
        fence = panel
        return [
            ("fence", "cover", fence, 0.9),
            ("person", "inspect", (lon0, lat0), 0.86),
            ("vehicle", "inspect", truck, 0.8),
            ("depot", "depot", (lon0 - dlon, lat0 - dlat), 1.0),
        ]
    if ontology in {"ag", "thermal"}:
        field = panel
        return [
            ("crop_stress" if ontology == "ag" else "hotspot", "inspect", field, 0.84),
            ("smoke", "inspect", (lon0 + 0.00015, lat0), 0.92),
            ("pond", "avoid", truck, 0.77),
            ("depot", "depot", (lon0 - dlon, lat0 - dlat), 1.0),
        ]
    # wildlife
    return [
        ("animal", "inspect", (lon0, lat0), 0.79),
        ("nest", "avoid", truck, 0.7),
        ("depot", "depot", (lon0 - dlon, lat0 - dlat), 1.0),
    ]


def _text_intents(
    text: str, lon0: float, lat0: float, ontology: str
) -> list[dict[str, Any]]:
    lower = text.lower()
    feats: list[dict[str, Any]] = []
    if "avoid" in lower and ("truck" in lower or "vehicle" in lower):
        feats.append(
            {
                "type": "Feature",
                "geometry": {
                    "type": "Point",
                    "coordinates": [lon0 + 0.0003, lat0],
                },
                "properties": {
                    "class": "vehicle",
                    "confidence": 0.7,
                    "role": "avoid",
                    "source": "demo",
                    "model": "demo-nl",
                },
            }
        )
    if "survey" in lower or "cover" in lower or "inspect" in lower:
        feats.append(
            {
                "type": "Feature",
                "geometry": {
                    "type": "Point",
                    "coordinates": [lon0, lat0 + 0.0002],
                },
                "properties": {
                    "class": "solar_panel" if ontology == "solar" else "target",
                    "confidence": 0.7,
                    "role": "inspect" if "inspect" in lower else "cover",
                    "source": "demo",
                    "model": "demo-nl",
                },
            }
        )
    return feats


__all__ = ["DemoBackend"]
