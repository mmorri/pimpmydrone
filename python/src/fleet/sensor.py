"""Sensor footprint / GSD model for inspection-oriented coverage reports.

Grid “coverage %” counts visited cells. Operators care whether the *camera*
swept the work. Given altitude and horizontal FOV we approximate a circular
footprint and mark every expected cell whose centre fell under any pass.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from typing import Any, Iterable, Sequence

Cell = tuple[int, int]


@dataclass(frozen=True)
class SensorSpec:
    altitude_m: float
    fov_h_deg: float = 60.0
    fov_v_deg: float = 45.0
    forward_overlap: float = 0.2
    side_overlap: float = 0.2
    cell_size_m: float = 5.0

    @property
    def footprint_width_m(self) -> float:
        return 2.0 * self.altitude_m * math.tan(math.radians(self.fov_h_deg) / 2.0)

    @property
    def footprint_height_m(self) -> float:
        return 2.0 * self.altitude_m * math.tan(math.radians(self.fov_v_deg) / 2.0)

    @property
    def gsd_cm_per_px_estimate(self) -> float | None:
        """Rough GSD if we pretend a 4000 px wide sensor across the FOV."""

        width = self.footprint_width_m
        if width <= 0:
            return None
        return (width * 100.0) / 4000.0

    @property
    def radius_cells(self) -> float:
        # Conservative: use half the smaller footprint axis.
        radius_m = 0.5 * min(self.footprint_width_m, self.footprint_height_m)
        return max(0.0, radius_m / max(self.cell_size_m, 1e-6))

    def to_json(self) -> dict[str, Any]:
        return {
            "altitude_m": self.altitude_m,
            "fov_h_deg": self.fov_h_deg,
            "fov_v_deg": self.fov_v_deg,
            "forward_overlap": self.forward_overlap,
            "side_overlap": self.side_overlap,
            "cell_size_m": self.cell_size_m,
            "footprint_width_m": round(self.footprint_width_m, 3),
            "footprint_height_m": round(self.footprint_height_m, 3),
            "radius_cells": round(self.radius_cells, 3),
            "gsd_cm_per_px_estimate": (
                None
                if self.gsd_cm_per_px_estimate is None
                else round(self.gsd_cm_per_px_estimate, 3)
            ),
        }


def sensor_from_params(params: dict[str, Any], geo: dict[str, Any] | None = None) -> SensorSpec:
    sensor = params.get("sensor") or {}
    altitude = float(
        sensor.get("altitude_m")
        or (geo or {}).get("altitude_m")
        or params.get("altitude_m")
        or 40.0
    )
    cell = float(
        sensor.get("cell_size_m")
        or (geo or {}).get("cell_size_m")
        or params.get("cell_size_m")
        or 5.0
    )
    return SensorSpec(
        altitude_m=altitude,
        fov_h_deg=float(sensor.get("fov_h_deg", 60.0)),
        fov_v_deg=float(sensor.get("fov_v_deg", 45.0)),
        forward_overlap=float(sensor.get("forward_overlap", 0.2)),
        side_overlap=float(sensor.get("side_overlap", 0.2)),
        cell_size_m=cell,
    )


def cells_under_footprint(center: Cell, radius_cells: float) -> set[Cell]:
    if radius_cells <= 0:
        return {center}
    r = int(math.ceil(radius_cells))
    cx, cy = center
    out: set[Cell] = set()
    r2 = radius_cells * radius_cells
    for dy in range(-r, r + 1):
        for dx in range(-r, r + 1):
            if dx * dx + dy * dy <= r2 + 1e-9:
                out.add((cx + dx, cy + dy))
    return out


def inspection_coverage(
    tours: Sequence[dict[str, Any]],
    expected: Iterable[Cell],
    sensor: SensorSpec,
) -> dict[str, Any]:
    """Fraction of expected work cells that fell under any sensor footprint."""

    expected_set = set(expected)
    if not expected_set:
        return {
            "percentage": 100.0,
            "expected_cells": 0,
            "imaged_cells": 0,
            "missed_cells": [],
            "sensor": sensor.to_json(),
            "note": "No expected cells — inspection metric vacuous.",
        }

    imaged: set[Cell] = set()
    radius = sensor.radius_cells
    for tour in tours:
        for wp in tour.get("waypoints") or []:
            cell = (int(wp["x"]), int(wp["y"]))
            for c in cells_under_footprint(cell, radius):
                if c in expected_set:
                    imaged.add(c)

    missed = sorted(expected_set - imaged)
    pct = 100.0 * len(imaged) / len(expected_set)
    # Strip spacing hint: ideal spacing ≈ footprint * (1 - overlap)
    ideal_spacing_m = sensor.footprint_height_m * (1.0 - sensor.forward_overlap)
    return {
        "percentage": pct,
        "expected_cells": len(expected_set),
        "imaged_cells": len(imaged),
        "missed_cells": [{"x": x, "y": y} for x, y in missed[:50]],
        "missed_count": len(missed),
        "sensor": sensor.to_json(),
        "ideal_forward_spacing_m": round(ideal_spacing_m, 3),
        "note": (
            "Approximate nadir camera sweep on a flat world — not a photogrammetry "
            "QA report. Validate GSD and overlap in your mapping tool."
        ),
    }


__all__ = [
    "SensorSpec",
    "sensor_from_params",
    "inspection_coverage",
    "cells_under_footprint",
]
