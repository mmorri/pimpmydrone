"""Class → planning role tables for perception verticals."""

from __future__ import annotations

from typing import Any, Mapping, Optional

# Default ontology name used when the caller does not pick a vertical flavour.
DEFAULT_ONTOLOGY = "solar"

ONTOLOGIES: dict[str, dict[str, dict[str, Any]]] = {
    "solar": {
        "solar_panel": {"role": "cover"},
        "panel": {"role": "cover"},
        "pv": {"role": "cover"},
        "hotspot": {"role": "inspect", "priority": 0.9, "thermal": True},
        "defect": {"role": "inspect", "priority": 0.85},
        "crack": {"role": "inspect", "priority": 0.8},
        "vehicle": {"role": "avoid"},
        "truck": {"role": "avoid"},
        "crane": {"role": "avoid"},
        "person": {"role": "avoid"},
        "building": {"role": "avoid"},
    },
    "security": {
        "person": {"role": "inspect", "priority": 0.9},
        "vehicle": {"role": "inspect", "priority": 0.75},
        "car": {"role": "inspect", "priority": 0.75},
        "truck": {"role": "inspect", "priority": 0.8},
        "fence": {"role": "cover"},
        "gate": {"role": "inspect", "priority": 0.6},
        "building": {"role": "avoid"},
    },
    "ag": {
        "crop_stress": {"role": "inspect", "priority": 0.85},
        "ndvi_low": {"role": "inspect", "priority": 0.8},
        "weed": {"role": "inspect", "priority": 0.55},
        "smoke": {"role": "inspect", "priority": 1.0, "thermal": True},
        "flame": {"role": "inspect", "priority": 1.0, "thermal": True},
        "gas_plume": {"role": "inspect", "priority": 0.95, "thermal": True},
        "pond": {"role": "avoid"},
        "tree": {"role": "avoid"},
    },
    "thermal": {
        "hotspot": {"role": "inspect", "priority": 0.95, "thermal": True},
        "smoke": {"role": "inspect", "priority": 1.0, "thermal": True},
        "flame": {"role": "inspect", "priority": 1.0, "thermal": True},
        "gas_plume": {"role": "inspect", "priority": 0.95, "thermal": True},
        "leak": {"role": "inspect", "priority": 0.9, "thermal": True},
    },
    "wildlife": {
        "animal": {"role": "inspect", "priority": 0.7},
        "bird": {"role": "inspect", "priority": 0.65},
        "nest": {"role": "avoid"},
        "herd": {"role": "avoid"},
    },
}


def resolve_role(
    class_name: str,
    *,
    ontology: str = DEFAULT_ONTOLOGY,
    explicit_role: Optional[str] = None,
    role_map: Optional[Mapping[str, str]] = None,
) -> dict[str, Any]:
    """Resolve a detection class to role metadata.

    Precedence: explicit feature role → request role_map → ontology table → inspect.
    """

    if explicit_role:
        return {"role": explicit_role}
    key = class_name.strip().lower().replace(" ", "_").replace("-", "_")
    if role_map and (class_name in role_map or key in role_map):
        mapped = role_map.get(class_name, role_map.get(key))
        if mapped:
            return {"role": mapped}
    table = ONTOLOGIES.get(ontology) or ONTOLOGIES[DEFAULT_ONTOLOGY]
    if key in table:
        return dict(table[key])
    # Soft fallback across all tables for common COCO-ish labels.
    for other in ONTOLOGIES.values():
        if key in other:
            return dict(other[key])
    return {"role": "inspect", "priority": 0.5}


def ontology_catalog() -> dict[str, Any]:
    return {
        "default": DEFAULT_ONTOLOGY,
        "ontologies": ONTOLOGIES,
    }


__all__ = [
    "DEFAULT_ONTOLOGY",
    "ONTOLOGIES",
    "ontology_catalog",
    "resolve_role",
]
