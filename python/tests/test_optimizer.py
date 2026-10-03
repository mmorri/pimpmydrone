from fleet.optimize import TourOptimizer, merge_tours


def make_tour(drone_id: int, duration: float, depot=(0, 0), recharges=()) -> dict:
    x, y = depot
    return {
        "drone_id": drone_id,
        "waypoints": [
            {"index": 0, "x": x, "y": y, "t": 0.0},
            {"index": 1, "x": x + 1, "y": y, "t": duration / 2},
            {"index": 2, "x": x, "y": y, "t": duration},
        ],
        "segments": [
            {"from": 0, "to": 1, "length": 1, "turn_cost": 0, "battery_used": 1.0},
            {"from": 1, "to": 2, "length": 1, "turn_cost": 1, "battery_used": 2.0},
        ],
        "recharge_stops": list(recharges),
        "cost": {"time": duration, "turns": 1, "length": 2.0},
    }


def test_optimizer_respects_drone_limit():
    tours = [make_tour(i, d) for i, d in enumerate([10.0, 8.0, 6.0])]
    result = TourOptimizer(tours, max_drones=2, time_limit=5, deconflict=False).solve()
    assert result.drone_count <= 2
    assert len(result.tours) == result.drone_count
    assert [t["drone_id"] for t in result.tours] == list(range(result.drone_count))
    for tour in result.tours:
        indices = [wp["index"] for wp in tour["waypoints"]]
        assert indices == list(range(len(indices)))


def test_optimizer_minimises_makespan_by_default():
    tours = [make_tour(i, 10.0) for i in range(3)]
    result = TourOptimizer(tours, max_drones=3, time_limit=5, deconflict=False).solve()
    assert result.drone_count == 3
    assert abs(result.makespan - 10.0) < 1e-6


def test_optimizer_minimises_drones_under_deadline():
    tours = [make_tour(i, 10.0) for i in range(3)]
    result = TourOptimizer(
        tours, max_drones=3, deadline=25.0, time_limit=5, deconflict=False
    ).solve()
    assert result.drone_count == 2
    assert result.makespan <= 25.0 + 1e-6


def test_optimizer_drone_cost_trades_makespan():
    tours = [make_tour(i, 10.0) for i in range(3)]
    result = TourOptimizer(
        tours, max_drones=3, drone_cost=100.0, time_limit=5, deconflict=False
    ).solve()
    assert result.drone_count == 1
    assert abs(result.makespan - 30.0) < 1e-6


def test_optimizer_keeps_depots_apart():
    tours = [make_tour(0, 10.0, depot=(0, 0)), make_tour(1, 10.0, depot=(5, 5))]
    result = TourOptimizer(
        tours, max_drones=2, drone_cost=1e6, time_limit=5, deconflict=False
    ).solve()
    assert result.drone_count == 2


def test_optimizer_deconflict_clears_shared_cells():
    tours = [make_tour(i, 10.0) for i in range(2)]
    result = TourOptimizer(tours, max_drones=2, time_limit=5, deconflict=True).solve()
    assert result.deconflict is not None
    assert result.deconflict["resolved"] is True


def test_merge_marks_tour_boundary_as_recharge():
    from fleet.optimize import TourTask, tour_depot

    tours = [make_tour(0, 4.0, recharges=[1]), make_tour(1, 4.0)]
    tasks = [TourTask(i, 4.0, 2.0, 1, tour_depot(t), t) for i, t in enumerate(tours)]
    merged = merge_tours(0, [(0, 0.0), (1, 4.0)], tasks)
    assert [wp["index"] for wp in merged["waypoints"]] == list(range(6))
    assert merged["recharge_stops"] == [1, 2]
    assert merged["cost"]["time"] == 8.0
    assert [s["from"] for s in merged["segments"]] == [0, 1, 3, 4]
