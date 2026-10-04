"""Crop flight styles.

Orchards are lawnmower passes along the long axis of the block, so the
sweep follows the rows. Open fields keep the default auto pattern, which
already picks the shorter of mega-STC, fine STC, and boustrophedon.
"""

from __future__ import annotations

from typing import Any

_ORCHARD = {"almonds", "pistachios", "walnuts", "grapes"}
_OPEN = {"corn", "soybeans", "spring wheat", "winter wheat", "sugar beets"}


def style_for_crop(crop: str | None) -> dict[str, str]:
    key = (crop or "").strip().lower()
    if key in _ORCHARD:
        return {
            "style": "orchard",
            "reason": "Orchard passes follow the long axis of the block.",
            "coverage_algorithm": "boustro",
            "align": "long-axis",
        }
    if key in _OPEN:
        return {
            "style": "open-field",
            "reason": "Open-field sweep. Auto keeps the shorter pattern on this block.",
            "coverage_algorithm": "auto",
            "align": "east-west",
        }
    return {
        "style": "generic",
        "reason": "No crop style. Auto keeps the shorter pattern.",
        "coverage_algorithm": "auto",
        "align": "east-west",
    }


def style_from_geojson(geojson: dict[str, Any]) -> dict[str, str]:
    if geojson.get("type") == "FeatureCollection":
        for feature in geojson.get("features") or []:
            crop = (feature.get("properties") or {}).get("crop")
            if crop:
                return style_for_crop(str(crop))
    return style_for_crop(None)
