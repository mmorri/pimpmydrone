"""Compact tour encoding: direction runs instead of per-cell waypoint arrays.

A straight corridor of 500 cells becomes one run ``{"dir":"E","n":500}`` plus
start pose / times, cutting JSON size by roughly an order of magnitude on open
maps while remaining lossless for 4-connected grid paths.
"""

from __future__ import annotations

from pathlib import Path
from typing import Any, Sequence

from . import io

_DIR_CHARS = {
    (1, 0): "E",
    (-1, 0): "W",
    (0, 1): "N",
    (0, -1): "S",
}
_CHAR_DIRS = {v: k for k, v in _DIR_CHARS.items()}


def encode_tour(tour: dict[str, Any]) -> dict[str, Any]:
    """Encode one expanded tour into a compact representation."""

    wps = sorted(tour.get("waypoints") or [], key=lambda wp: wp["index"])
    if not wps:
        return {
            "drone_id": tour.get("drone_id"),
            "encoding": "runs-v1",
            "start": None,
            "runs": [],
            "recharge_stops": [],
            "cost": tour.get("cost"),
            "segments": tour.get("segments") or [],
        }

    start = wps[0]
    runs: list[dict[str, Any]] = []
    i = 1
    while i < len(wps):
        prev = wps[i - 1]
        cur = wps[i]
        dx = int(cur["x"]) - int(prev["x"])
        dy = int(cur["y"]) - int(prev["y"])
        key = (dx, dy)
        if key not in _DIR_CHARS:
            # Non-adjacent jump (should not happen in valid tours): store absolute.
            runs.append(
                {
                    "teleport": {
                        "x": int(cur["x"]),
                        "y": int(cur["y"]),
                        "t": float(cur.get("t", 0.0)),
                    }
                }
            )
            i += 1
            continue
        n = 1
        t_end = float(cur.get("t", 0.0))
        while i + 1 < len(wps):
            nxt = wps[i + 1]
            ndx = int(nxt["x"]) - int(cur["x"])
            ndy = int(nxt["y"]) - int(cur["y"])
            if (ndx, ndy) != key:
                break
            n += 1
            i += 1
            cur = nxt
            t_end = float(cur.get("t", t_end))
        runs.append({"dir": _DIR_CHARS[key], "n": n, "t_end": t_end})
        i += 1

    return {
        "drone_id": tour.get("drone_id"),
        "encoding": "runs-v1",
        "start": {
            "x": int(start["x"]),
            "y": int(start["y"]),
            "t": float(start.get("t", 0.0)),
        },
        "runs": runs,
        "recharge_stops": list(tour.get("recharge_stops") or []),
        "cost": tour.get("cost"),
        "segments": tour.get("segments") or [],
    }


def decode_tour(compact: dict[str, Any]) -> dict[str, Any]:
    """Expand a compact tour back to the waypoint-list form."""

    if compact.get("encoding") != "runs-v1" or not compact.get("start"):
        # Already expanded or unknown — return as-is if it looks like a tour.
        if "waypoints" in compact:
            return compact
        raise ValueError("Not a runs-v1 compact tour")

    start = compact["start"]
    x, y = int(start["x"]), int(start["y"])
    t = float(start.get("t", 0.0))
    waypoints = [{"index": 0, "x": x, "y": y, "t": t}]
    idx = 1
    for run in compact.get("runs") or []:
        if "teleport" in run:
            tele = run["teleport"]
            x, y = int(tele["x"]), int(tele["y"])
            t = float(tele.get("t", t))
            waypoints.append({"index": idx, "x": x, "y": y, "t": t})
            idx += 1
            continue
        dx, dy = _CHAR_DIRS[run["dir"]]
        n = int(run["n"])
        t_end = float(run.get("t_end", t + n))
        dt = (t_end - t) / n if n else 0.0
        for k in range(n):
            x += dx
            y += dy
            t = t + dt
            waypoints.append({"index": idx, "x": x, "y": y, "t": t})
            idx += 1
        t = t_end

    return {
        "drone_id": compact.get("drone_id"),
        "waypoints": waypoints,
        "segments": list(compact.get("segments") or []),
        "recharge_stops": list(compact.get("recharge_stops") or []),
        "cost": compact.get("cost") or {},
    }


def encode_tours(tours: Sequence[dict[str, Any]]) -> list[dict[str, Any]]:
    return [encode_tour(t) for t in tours]


def decode_tours(compacts: Sequence[dict[str, Any]]) -> list[dict[str, Any]]:
    return [decode_tour(t) for t in compacts]


def write_compact_tours(path: Path, tours: Sequence[dict[str, Any]]) -> Path:
    io.write_json(path, encode_tours(tours))
    return path


def compression_ratio(tours: Sequence[dict[str, Any]]) -> float:
    """Rough JSON size ratio expanded/compact (higher = better compression)."""

    import json

    expanded = json.dumps(list(tours))
    compact = json.dumps(encode_tours(tours))
    if not compact:
        return 1.0
    return len(expanded) / len(compact)


__all__ = [
    "encode_tour",
    "decode_tour",
    "encode_tours",
    "decode_tours",
    "write_compact_tours",
    "compression_ratio",
]
