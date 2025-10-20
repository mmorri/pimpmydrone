from pathlib import Path

from fleet.generate_map import GeneratorConfig, generate_workspace


def test_generator_reproducible(tmp_path: Path) -> None:
    config = GeneratorConfig(seed=42, width=10, height=10, obstacle_density=0.1, no_fly_count=1, depot_count=1)
    map1, params1 = generate_workspace(config)
    map2, params2 = generate_workspace(config)
    assert map1 == map2
    assert params1 == params2


def test_generator_varies_with_seed() -> None:
    config_a = GeneratorConfig(seed=1, width=10, height=10, obstacle_density=0.1, no_fly_count=1, depot_count=1)
    config_b = GeneratorConfig(seed=2, width=10, height=10, obstacle_density=0.1, no_fly_count=1, depot_count=1)
    map_a, _ = generate_workspace(config_a)
    map_b, _ = generate_workspace(config_b)
    assert map_a != map_b
