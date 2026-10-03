"""Perception backend protocol and shared input types."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Optional, Protocol, runtime_checkable


@dataclass
class PerceptionInput:
    """Inputs for a detection backend.

    Georeferencing is caller-supplied: either an affine pixel→lon/lat mapping
    (``frame_georef``) or detections already in WGS84.
    """

    image_path: Optional[str] = None
    image_bytes: Optional[bytes] = None
    image_b64: Optional[str] = None
    image_url: Optional[str] = None
    text: Optional[str] = None
    ontology: str = "solar"
    # Maps pixel coords: lon = a * x + b * y + c, lat = d * x + e * y + f
    frame_georef: Optional[dict[str, float]] = None
    # Fallback anchor when no full affine is given (centre of frame).
    center_lonlat: Optional[tuple[float, float]] = None
    extras: dict[str, Any] = field(default_factory=dict)


@runtime_checkable
class PerceptionBackend(Protocol):
    name: str

    def status(self) -> dict[str, Any]:
        """Return installed/detail flags without running inference."""

    def detect(self, inputs: PerceptionInput) -> dict[str, Any]:
        """Return a perception FeatureCollection."""


def list_backend_status() -> list[dict[str, Any]]:
    from . import available_backends

    return available_backends()


def pixel_to_lonlat(
    x: float,
    y: float,
    *,
    frame_georef: Optional[dict[str, float]],
    center_lonlat: Optional[tuple[float, float]],
    width: float = 640,
    height: float = 480,
) -> tuple[float, float]:
    if frame_georef:
        a = float(frame_georef.get("a", 0))
        b = float(frame_georef.get("b", 0))
        c = float(frame_georef.get("c", 0))
        d = float(frame_georef.get("d", 0))
        e = float(frame_georef.get("e", 0))
        f = float(frame_georef.get("f", 0))
        return a * x + b * y + c, d * x + e * y + f
    if center_lonlat:
        # Rough local metres-per-pixel guess (~0.2 m/px) for demo georef.
        m_per_px = float((frame_georef or {}).get("m_per_px", 0.2))
        lon0, lat0 = center_lonlat
        import math

        dx = (x - width / 2) * m_per_px
        dy = (height / 2 - y) * m_per_px
        m_lon = 111_320.0 * max(0.2, math.cos(math.radians(lat0)))
        return lon0 + dx / m_lon, lat0 + dy / 111_320.0
    raise ValueError(
        "Detections need frame_georef or center_lonlat to project into WGS84"
    )


__all__ = [
    "PerceptionBackend",
    "PerceptionInput",
    "list_backend_status",
    "pixel_to_lonlat",
]
