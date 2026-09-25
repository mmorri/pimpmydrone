from fleet.generate_map import GeneratorConfig, generate_workspace
from fleet.map_utils import MapSpec


def config(seed: int, **overrides) -> GeneratorConfig:
    base = dict(
        seed=seed,
        width=10,
        height=10,
        obstacle_density=0.1,
        no_fly_count=1,
        depot_count=1,
    )
    base.update(overrides)
    return GeneratorConfig(**base)


def test_generator_reproducible() -> None:
    map1, params1 = generate_workspace(config(42))
    map2, params2 = generate_workspace(config(42))
    assert map1 == map2
    assert params1 == params2


def test_generator_varies_with_seed() -> None:
    map_a, _ = generate_workspace(config(1))
    map_b, _ = generate_workspace(config(2))
    assert map_a != map_b


def test_depots_are_free_cells() -> None:
    map_data, _ = generate_workspace(config(7, obstacle_density=0.4, no_fly_count=3))
    spec = MapSpec.from_json(map_data)
    blocked = spec.blocked_cells()
    assert spec.depots
    assert all(depot not in blocked for depot in spec.depots)


def test_generator_scales_to_large_maps() -> None:
    map_data, _ = generate_workspace(
        config(3, width=400, height=400, obstacle_density=0.2, no_fly_count=3)
    )
    assert len(map_data["obstacles"]) > 0
    assert len(map_data["no_fly"]) == 3
