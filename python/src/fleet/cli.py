"""Command line entry points for the Fleet toolkit."""

from __future__ import annotations

import os
import subprocess
from pathlib import Path
from typing import Optional

import click
from rich.console import Console
from rich.table import Table

from . import io
from .export_mission import export_all
from .generate_map import GeneratorConfig, write_workspace
from .optimize import optimize_file
from .simulate import SimulationResult, simulate
from .site import VERTICALS, plan_site
from .visualize import visualize

console = Console()

# Repository layout: <root>/python/src/fleet/cli.py. Both locations can be
# overridden so the package also works when installed outside the checkout.
ROOT = Path(os.environ.get("FLEET_ROOT", Path(__file__).resolve().parents[3]))
OUTPUTS = Path(os.environ.get("FLEET_OUTPUTS", ROOT / "outputs"))
HASKELL_BIN = Path(os.environ.get("FLEET_HASKELL_BIN", ROOT / "haskell" / "bin"))

PathArg = click.Path(path_type=Path)


def workspace_options(fn):
    """Options shared by ``generate`` and ``run``."""

    options = [
        click.option("--seed", type=int, default=1337, show_default=True),
        click.option("--width", type=int, default=100, show_default=True),
        click.option("--height", type=int, default=100, show_default=True),
        click.option("--obstacle-density", type=float, default=0.15, show_default=True),
        click.option(
            "--no-fly", "no_fly_count", type=int, default=2, show_default=True
        ),
        click.option("--depots", "depot_count", type=int, default=1, show_default=True),
        click.option("--speed", type=float, default=1.0, show_default=True),
        click.option("--battery", type=float, default=300.0, show_default=True),
        click.option("--takeoff-cost", type=float, default=5.0, show_default=True),
        click.option("--landing-cost", type=float, default=5.0, show_default=True),
        click.option("--max-drones", type=int, default=5, show_default=True),
    ]
    for option in reversed(options):
        fn = option(fn)
    return fn


def optimizer_options(fn):
    """Options shared by ``optimize`` and ``run``."""

    options = [
        click.option(
            "--drone-cost",
            type=float,
            default=1.0,
            show_default=True,
            help="Seconds of makespan one extra drone is worth.",
        ),
        click.option(
            "--deadline",
            type=float,
            default=None,
            help="Makespan limit in seconds; minimises the fleet size under it.",
        ),
        click.option(
            "--solver-time",
            type=float,
            default=30.0,
            show_default=True,
            help="CP-SAT time limit in seconds.",
        ),
    ]
    for option in reversed(options):
        fn = option(fn)
    return fn


@click.group()
def run() -> None:
    """Fleet CLI entry point."""


@run.command("generate")
@workspace_options
@click.option("--output-dir", type=PathArg, default=None)
def generate_command(output_dir: Optional[Path], **config_kwargs) -> None:
    """Generate random map and parameter files."""

    config = GeneratorConfig(**config_kwargs)
    out_dir = output_dir or (OUTPUTS / str(config.seed))
    io.ensure_directory(out_dir)
    write_workspace(out_dir / "map.json", out_dir / "params.json", config)
    console.print(f"Generated workspace at [bold]{out_dir}[/bold]")


@run.command("plan")
@click.option("--map", "map_path", type=PathArg, required=True)
@click.option("--params", "params_path", type=PathArg, required=True)
@click.option("--output", "output_path", type=PathArg, required=True)
@click.option(
    "--algorithm", type=click.Choice(["stc", "boustro", "auto"]), default="auto"
)
@click.option("--partition", type=click.Choice(["region", "strip"]), default="region")
def plan_command(
    map_path: Path, params_path: Path, output_path: Path, algorithm: str, partition: str
) -> None:
    """Plan tours using the Haskell executables."""

    plan(map_path, params_path, output_path, algorithm, partition)
    console.print(f"Planned initial tours at [bold]{output_path}[/bold]")


def plan(
    map_path: Path,
    params_path: Path,
    output_path: Path,
    algorithm: str = "auto",
    partition: str = "region",
) -> Path:
    map_path, params_path = map_path.resolve(), params_path.resolve()
    output_path = output_path.resolve()
    regions_path = output_path.parent / "regions.json"
    skipped_path = output_path.parent / "skipped.json"
    partitioner = HASKELL_BIN / "coverage-partitioner"
    planner = HASKELL_BIN / "coverage-planner"
    if not partitioner.exists() or not planner.exists():
        raise click.ClickException(
            f"Haskell binaries not found in {HASKELL_BIN}. Run `make build` first "
            "or set FLEET_HASKELL_BIN."
        )
    run_subprocess(
        [
            str(partitioner),
            "--map",
            str(map_path),
            "--params",
            str(params_path),
            "--output",
            str(regions_path),
            "--method",
            partition,
        ]
    )
    run_subprocess(
        [
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
            "--skipped",
            str(skipped_path),
        ]
    )
    return output_path


@run.command("optimize")
@click.option("--input", "input_path", type=PathArg, required=True)
@click.option("--output", "output_path", type=PathArg, required=True)
@click.option("--max-drones", type=int, required=True)
@optimizer_options
def optimize_command(
    input_path: Path,
    output_path: Path,
    max_drones: int,
    drone_cost: float,
    deadline: Optional[float],
    solver_time: float,
) -> None:
    """Assign tours to drones with the OR-Tools scheduling model."""

    result = optimize_file(
        input_path,
        output_path,
        max_drones=max_drones,
        drone_cost=drone_cost,
        deadline=deadline,
        time_limit=solver_time,
    )
    console.print(
        f"Optimized tours written to [bold]{output_path}[/bold] "
        f"with makespan {result.makespan:.2f}s and {result.drone_count} drones",
    )


@run.command("simulate")
@click.option("--map", "map_path", type=PathArg, required=True)
@click.option("--params", "params_path", type=PathArg, required=True)
@click.option("--tours", "tours_path", type=PathArg, required=True)
@click.option("--metrics", "metrics_path", type=PathArg, required=True)
@click.option(
    "--skipped",
    "skipped_path",
    type=PathArg,
    default=None,
    help="skipped.json from the planner, listing cells out of battery range.",
)
def simulate_command(
    map_path: Path,
    params_path: Path,
    tours_path: Path,
    metrics_path: Path,
    skipped_path: Optional[Path],
) -> None:
    """Run the discrete simulator and emit mission metrics."""

    result = simulate(map_path, params_path, tours_path, metrics_path, skipped_path)
    print_summary(result, metrics_path.parent)
    fail_on_problems(result)


@run.command("visualize")
@click.option("--map", "map_path", type=PathArg, required=True)
@click.option("--tours", "tours_path", type=PathArg, required=True)
@click.option("--png", "png_path", type=PathArg, required=True)
@click.option("--mp4", "mp4_path", type=PathArg, default=None)
def visualize_command(
    map_path: Path, tours_path: Path, png_path: Path, mp4_path: Optional[Path]
) -> None:
    """Render tours to PNG (and optional MP4)."""

    visualize(map_path, tours_path, png_path, mp4_path)
    console.print(f"Saved visualization to [bold]{png_path}[/bold]")
    if mp4_path is not None:
        console.print(f"Saved animation to [bold]{mp4_path}[/bold]")


@run.command("run")
@workspace_options
@optimizer_options
@click.option(
    "--coverage-algorithm",
    type=click.Choice(["stc", "boustro", "auto"]),
    default="auto",
    show_default=True,
)
@click.option("--partition", type=click.Choice(["region", "strip"]), default="region")
@click.option("--mp4/--no-mp4", default=False)
@click.option("--output-dir", type=PathArg, default=None)
def run_command(
    drone_cost: float,
    deadline: Optional[float],
    solver_time: float,
    coverage_algorithm: str,
    partition: str,
    mp4: bool,
    output_dir: Optional[Path],
    **config_kwargs,
) -> None:
    """Run the full pipeline end-to-end."""

    config = GeneratorConfig(**config_kwargs)
    out_dir = output_dir or (OUTPUTS / str(config.seed))
    io.ensure_directory(out_dir)
    map_path = out_dir / "map.json"
    params_path = out_dir / "params.json"
    initial_tours_path = out_dir / "tours_initial.json"
    optimized_tours_path = out_dir / "tours_optimized.json"
    metrics_path = out_dir / "metrics.json"
    png_path = out_dir / "path.png"
    mp4_path = out_dir / "path.mp4" if mp4 else None

    write_workspace(map_path, params_path, config)
    plan(map_path, params_path, initial_tours_path, coverage_algorithm, partition)
    optimize_file(
        initial_tours_path,
        optimized_tours_path,
        max_drones=config.max_drones,
        drone_cost=drone_cost,
        deadline=deadline,
        time_limit=solver_time,
    )
    result = simulate(
        map_path,
        params_path,
        optimized_tours_path,
        metrics_path,
        skipped_path=out_dir / "skipped.json",
    )
    visualize(map_path, optimized_tours_path, png_path, mp4_path)

    console.rule("Mission Summary")
    print_summary(result, out_dir)
    fail_on_problems(result)


@run.command("from-geo")
@click.option("--geojson", "geojson_path", type=PathArg, required=True)
@click.option("--output-dir", type=PathArg, required=True)
@click.option(
    "--vertical",
    type=click.Choice(sorted(VERTICALS)),
    default="solar",
    show_default=True,
)
@click.option("--cell-size", "cell_size_m", type=float, default=None)
@click.option("--max-drones", type=int, default=None)
@click.option("--battery", type=float, default=None)
@click.option("--altitude", "altitude_m", type=float, default=None)
@click.option(
    "--coverage-algorithm",
    type=click.Choice(["stc", "boustro", "auto"]),
    default="auto",
    show_default=True,
)
@click.option("--partition", type=click.Choice(["region", "strip"]), default="region")
@optimizer_options
def from_geo_command(
    geojson_path: Path,
    output_dir: Path,
    vertical: str,
    cell_size_m: Optional[float],
    max_drones: Optional[int],
    battery: Optional[float],
    altitude_m: Optional[float],
    coverage_algorithm: str,
    partition: str,
    drone_cost: float,
    deadline: Optional[float],
    solver_time: float,
) -> None:
    """Plan a real-world site from GeoJSON and export flyable missions."""

    result = plan_site(
        geojson_path,
        output_dir,
        vertical=vertical,
        cell_size_m=cell_size_m,
        max_drones=max_drones,
        battery=battery,
        altitude_m=altitude_m,
        coverage_algorithm=coverage_algorithm,
        partition=partition,
        drone_cost=drone_cost,
        deadline=deadline,
        solver_time=solver_time,
    )
    console.rule(f"Site mission ({vertical})")
    print_summary(result.simulation, output_dir)
    console.print(f"Missions exported under [bold]{output_dir / 'missions'}[/bold]")
    fail_on_problems(result.simulation)


@run.command("export")
@click.option("--tours", "tours_path", type=PathArg, required=True)
@click.option("--geo", "geo_path", type=PathArg, required=True)
@click.option("--output-dir", type=PathArg, required=True)
@click.option("--full/--simplify", default=False, help="Keep every grid cell (large).")
def export_command(
    tours_path: Path, geo_path: Path, output_dir: Path, full: bool
) -> None:
    """Export tours to GeoJSON / CSV / QGroundControl .plan files."""

    manifest = export_all(tours_path, geo_path, output_dir, simplify=not full)
    console.print(f"Wrote mission bundle to [bold]{output_dir}[/bold]")
    for key in ("geojson", "csv"):
        console.print(f"  {key}: {manifest[key]}")
    for plan in manifest["qgc_plans"]:
        console.print(f"  qgc: {plan}")


@run.command("serve")
@click.option("--host", default="127.0.0.1", show_default=True)
@click.option("--port", type=int, default=8000, show_default=True)
def serve_command(host: str, port: int) -> None:
    """Launch the local planner web UI / API."""

    try:
        import uvicorn
    except ImportError as exc:  # pragma: no cover
        raise click.ClickException(
            "uvicorn is required for `fleet serve`. Run `uv sync` in python/."
        ) from exc
    console.print(f"Serving PimpMyDrone UI on http://{host}:{port}")
    uvicorn.run("fleet.webapp:app", host=host, port=port, reload=False)


def print_summary(result: SimulationResult, out_dir: Path) -> None:
    metrics = result.metrics
    table = Table()
    table.add_column("Metric")
    table.add_column("Value")
    table.add_row("Coverage", f"{metrics['coverage']['percentage']:.2f}%")
    table.add_row("Reachable cells", str(metrics["coverage"]["reachable_cells"]))
    table.add_row("Unreachable cells", str(metrics["coverage"]["unreachable_cells"]))
    table.add_row(
        "Drones Used", f"{metrics['drones']['used']}/{metrics['drones']['max']}"
    )
    table.add_row("Makespan", f"{metrics['makespan']:.2f}s")
    table.add_row("Total length", f"{metrics['total_length']:.0f}")
    table.add_row("Outputs", str(out_dir))
    console.print(table)


def fail_on_problems(result: SimulationResult) -> None:
    if result.ok:
        return
    for problem in result.problems:
        console.print(f"[red]✗[/red] {problem}")
    raise click.ClickException("Simulation found problems with the plan")


def run_subprocess(cmd: list[str]) -> None:
    console.log("Executing", " ".join(cmd))
    completed = subprocess.run(cmd)
    if completed.returncode != 0:
        raise click.ClickException(f"Command failed: {' '.join(cmd)}")


if __name__ == "__main__":
    run()
