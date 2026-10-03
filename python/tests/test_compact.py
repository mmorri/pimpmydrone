"""Lossless compact tour encoding."""

from __future__ import annotations

from fleet.compact import compression_ratio, decode_tour, encode_tour


def test_encode_decode_roundtrip_straight_and_turns():
    tour = {
        "drone_id": 0,
        "waypoints": [
            {"index": 0, "x": 0, "y": 0, "t": 0.0},
            {"index": 1, "x": 1, "y": 0, "t": 1.0},
            {"index": 2, "x": 2, "y": 0, "t": 2.0},
            {"index": 3, "x": 2, "y": 1, "t": 3.0},
            {"index": 4, "x": 2, "y": 2, "t": 4.0},
            {"index": 5, "x": 1, "y": 2, "t": 5.0},
        ],
        "segments": [
            {"from": 0, "to": 1, "length": 1, "turn_cost": 0, "battery_used": 1}
        ],
        "recharge_stops": [3],
        "cost": {"time": 5.0, "turns": 2, "length": 5.0},
    }
    compact = encode_tour(tour)
    assert compact["encoding"] == "runs-v1"
    assert len(compact["runs"]) == 3  # E*2, N*2, W*1
    restored = decode_tour(compact)
    assert [(wp["x"], wp["y"]) for wp in restored["waypoints"]] == [
        (wp["x"], wp["y"]) for wp in tour["waypoints"]
    ]
    assert restored["recharge_stops"] == [3]


def test_compression_on_long_corridor():
    wps = [{"index": i, "x": i, "y": 0, "t": float(i)} for i in range(200)]
    tour = {
        "drone_id": 1,
        "waypoints": wps,
        "segments": [],
        "recharge_stops": [],
        "cost": {"time": 199.0, "turns": 0, "length": 199.0},
    }
    assert compression_ratio([tour]) > 5.0
