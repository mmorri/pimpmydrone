"""Command line entry points for the Fleet toolkit."""

from __future__ import annotations

import subprocess
from pathlib import Path
from typing import Optional

import click
from rich.console import Console
from rich.table import Table

from . import io
from .generate_map import GeneratorConfig, write_workspace
from .optimize import optimize_file
from .simulate import simulate
from .visualize import visualize

console = Console()
# Resolve repository root: this file lives at python/src/fleet/cli.py
# parents[0]=.../fleet, [1]=.../src, [2]=.../python, [3]=repo root
ROOT = Path(__file__).resolve().parents[3]
OUTPUTS = ROOT / "outputs"
HASKELL_BIN = ROOT / "haskell" / "bin"


@click.group()
def run() -> None:
    """Fleet CLI entry point."""


@run.command("generate")
@click.option("--seed", type=int, required=True)
@click.option("--width", type=int, default=100)
@click.option("--height", type=int, default=100)
@click.option("--obstacle-density", type=float, default=0.15)
@click.option("--no-fly", "no_fly_count", type=int, default=2)
@click.option("--depots", "depot_count", type=int, default=1)
@click.option("--speed", type=float, default=1.0)
@click.option("--battery", type=float, default=300.0)
@click.option("--takeoff-cost", type=float, default=5.0)
@click.option("--landing-cost", type=float, default=5.0)
@click.option("--max-drones", type=int, default=5)
@click.option("--output-dir", type=click.Path(path_type=Path), default=None)
def generate_command(
    seed: int,
    width: int,
    height: int,
    obstacle_density: float,
    no_fly_count: int,
    depot_count: int,
    speed: float,
    battery: float,
    takeoff_cost: float,
    landing_cost: float,
    max_drones: int,
    output_dir: Optional[Path],
) -> None:
    """Generate random map and parameter files."""

    out_dir = output_dir or (OUTPUTS / str(seed))
    io.ensure_directory(out_dir)
    map_path = out_dir / "map.json"
    params_path = out_dir / "params.json"

    config = GeneratorConfig(
        width=width,
        height=height,
        obstacle_density=obstacle_density,
        no_fly_count=no_fly_count,
        depot_count=depot_count,
        seed=seed,
        speed=speed,
        battery=battery,
        takeoff_cost=takeoff_cost,
        landing_cost=landing_cost,
        max_drones=max_drones,
    )
    write_workspace(map_path, params_path, config)
    console.print(f"Generated workspace at [bold]{out_dir}[/bold]")


@run.command("plan")
@click.option("--map", "map_path", type=click.Path(path_type=Path), required=True)
@click.option("--params", "params_path", type=click.Path(path_type=Path), required=True)
@click.option("--output", "output_path", type=click.Path(path_type=Path), required=True)
@click.option("--algorithm", type=click.Choice(["stc", "boustro"]), default="stc")
def plan_command(map_path: Path, params_path: Path, output_path: Path, algorithm: str) -> None:
    """Plan tours using the Haskell executables."""

    regions_path = output_path.parent / "regions.json"
    partitioner = HASKELL_BIN / "coverage-partitioner"
    planner = HASKELL_BIN / "coverage-planner"
    if not partitioner.exists() or not planner.exists():
        raise click.ClickException("Haskell binaries not found. Run `make build` first.")

    run_subprocess([
        str(partitioner),
        "--map",
        str(map_path),
        "--params",
        str(params_path),
        "--output",
        str(regions_path),
    ])

    run_subprocess([
        str(planner),
        "--map",
        str(map_path),
        "--params",
        str(params_path),
        "--regions",
        str(regions_path),
        "--output",
        str(output_path),
        "--coverage-algorithm",
        algorithm,
    ])
    console.print(f"Planned initial tours at [bold]{output_path}[/bold]")


@run.command("optimize")
@click.option("--input", "input_path", type=click.Path(path_type=Path), required=True)
@click.option("--output", "output_path", type=click.Path(path_type=Path), required=True)
@click.option("--max-drones", type=int, required=True)
def optimize_command(input_path: Path, output_path: Path, max_drones: int) -> None:
    """Optimize tours using the OR-Tools assignment model."""

    result = optimize_file(input_path, output_path, max_drones=max_drones)
    console.print(
        f"Optimized tours written to [bold]{output_path}[/bold] with makespan {result.makespan:.2f}s and {result.drone_count} drones",
    )


@run.command("simulate")
@click.option("--map", "map_path", type=click.Path(path_type=Path), required=True)
@click.option("--params", "params_path", type=click.Path(path_type=Path), required=True)
@click.option("--tours", "tours_path", type=click.Path(path_type=Path), required=True)
@click.option("--metrics", "metrics_path", type=click.Path(path_type=Path), required=True)
def simulate_command(map_path: Path, params_path: Path, tours_path: Path, metrics_path: Path) -> None:
    """Run the discrete simulator and emit mission metrics."""

    result = simulate(map_path, params_path, tours_path, metrics_path)
    table = Table(title="Simulation Metrics")
    table.add_column("Metric")
    table.add_column("Value")
    table.add_row("Coverage", f"{result.metrics['coverage']['percentage']:.2f}%")
    table.add_row("Drones Used", str(result.metrics["drones"]["used"]))
    table.add_row("Makespan", f"{result.metrics['makespan']:.2f}s")
    console.print(table)


@run.command("visualize")
@click.option("--map", "map_path", type=click.Path(path_type=Path), required=True)
@click.option("--tours", "tours_path", type=click.Path(path_type=Path), required=True)
@click.option("--png", "png_path", type=click.Path(path_type=Path), required=True)
@click.option("--mp4", "mp4_path", type=click.Path(path_type=Path), default=None)
def visualize_command(map_path: Path, tours_path: Path, png_path: Path, mp4_path: Optional[Path]) -> None:
    """Render tours to PNG (and optional MP4)."""

    visualize(map_path, tours_path, png_path, mp4_path)
    console.print(f"Saved visualization to [bold]{png_path}[/bold]")
    if mp4_path is not None:
        console.print(f"Saved animation to [bold]{mp4_path}[/bold]")


@run.command("run")
@click.option("--seed", type=int, default=1337)
@click.option("--width", type=int, default=100)
@click.option("--height", type=int, default=100)
@click.option("--obstacle-density", type=float, default=0.15)
@click.option("--no-fly", "no_fly_count", type=int, default=2)
@click.option("--depots", "depot_count", type=int, default=1)
@click.option("--speed", type=float, default=1.0)
@click.option("--battery", type=float, default=300.0)
@click.option("--takeoff-cost", type=float, default=5.0)
@click.option("--landing-cost", type=float, default=5.0)
@click.option("--max-drones", type=int, default=5)
@click.option("--coverage-algorithm", type=click.Choice(["stc", "boustro"]), default="stc")
@click.option("--mp4/--no-mp4", default=False)
def run_command(
    seed: int,
    width: int,
    height: int,
    obstacle_density: float,
    no_fly_count: int,
    depot_count: int,
    speed: float,
    battery: float,
    takeoff_cost: float,
    landing_cost: float,
    max_drones: int,
    coverage_algorithm: str,
    mp4: bool,
) -> None:
    """Run the full pipeline end-to-end."""

    out_dir = OUTPUTS / str(seed)
    io.ensure_directory(out_dir)
    map_path = out_dir / "map.json"
    params_path = out_dir / "params.json"
    initial_tours_path = out_dir / "tours_initial.json"
    optimized_tours_path = out_dir / "tours_optimized.json"
    metrics_path = out_dir / "metrics.json"
    png_path = out_dir / "path.png"
    mp4_path = out_dir / "path.mp4" if mp4 else None

    config = GeneratorConfig(
        width=width,
        height=height,
        obstacle_density=obstacle_density,
        no_fly_count=no_fly_count,
        depot_count=depot_count,
        seed=seed,
        speed=speed,
        battery=battery,
        takeoff_cost=takeoff_cost,
        landing_cost=landing_cost,
        max_drones=max_drones,
    )
    write_workspace(map_path, params_path, config)

    plan_command.callback(map_path, params_path, initial_tours_path, coverage_algorithm)  # type: ignore[arg-type]
    optimize_file(initial_tours_path, optimized_tours_path, max_drones=max_drones)
    result = simulate(map_path, params_path, optimized_tours_path, metrics_path)
    visualize(map_path, optimized_tours_path, png_path, mp4_path)

    console.rule("Mission Summary")
    table = Table()
    table.add_column("Metric")
    table.add_column("Value")
    table.add_row("Coverage", f"{result.metrics['coverage']['percentage']:.2f}%")
    table.add_row("Drones Used", f"{result.metrics['drones']['used']}/{result.metrics['drones']['max']}")
    table.add_row("Makespan", f"{result.metrics['makespan']:.2f}s")
    table.add_row("Outputs", str(out_dir))
    console.print(table)


def run_subprocess(cmd: list[str]) -> None:
    console.log("Executing", " ".join(cmd))
    completed = subprocess.run(cmd, cwd=ROOT)
    if completed.returncode != 0:
        raise click.ClickException(f"Command failed: {' '.join(cmd)}")


if __name__ == "__main__":
    run()
