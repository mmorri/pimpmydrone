from fleet.optimize import TourOptimizer


def make_tour(drone_id: int, duration: float, length: float = 10.0, turns: int = 5):
    waypoints = [
        {"index": 0, "x": 0, "y": 0, "t": 0.0},
        {"index": 1, "x": drone_id + 1, "y": 0, "t": duration},
    ]
    segments = [
        {
            "from": 0,
            "to": 1,
            "length": length,
            "turn_cost": turns,
            "battery_used": duration,
        }
    ]
    tour = {
        "drone_id": drone_id,
        "waypoints": waypoints,
        "segments": segments,
        "recharge_stops": [],
        "cost": {"time": duration, "turns": turns, "length": length},
    }
    return tour


def test_optimizer_respects_drone_limit():
    tours = [make_tour(i, duration) for i, duration in enumerate([10.0, 8.0, 6.0])]
    optimizer = TourOptimizer(tours, max_drones=2)
    result = optimizer.solve()
    assert result.drone_count <= 2
    assert len(result.tours) == result.drone_count
    for tour in result.tours:
        indices = [wp["index"] for wp in tour["waypoints"]]
        assert indices == sorted(indices)
        assert tour["cost"]["time"] >= max(wp["t"] for wp in tour["waypoints"]) - min(wp["t"] for wp in tour["waypoints"]) - 1e-6
