"""Static and animated visualization of drone tours."""

from __future__ import annotations

from pathlib import Path
from typing import List, Sequence

import matplotlib

matplotlib.use("Agg")

import matplotlib.pyplot as plt  # noqa: E402
import numpy as np  # noqa: E402
from matplotlib import animation  # noqa: E402
from matplotlib.patches import Polygon as MplPolygon  # noqa: E402
from matplotlib.patches import Rectangle  # noqa: E402
from matplotlib.ticker import MaxNLocator  # noqa: E402

from . import io  # noqa: E402
from .map_utils import MapSpec, polygon_rings  # noqa: E402

COLORS = [
    "#1f77b4",
    "#ff7f0e",
    "#2ca02c",
    "#d62728",
    "#9467bd",
    "#8c564b",
]

# Above this many cells per side markers and per-cell grid lines become noise.
DETAIL_LIMIT = 40


def visualize(
    map_path: Path,
    tours_path: Path,
    png_path: Path,
    mp4_path: Path | None = None,
    *,
    heatmap_path: Path | None = None,
    gantt_path: Path | None = None,
) -> None:
    map_data = io.read_json(map_path)
    tours = io.read_json(tours_path)
    spec = MapSpec.from_json(map_data)

    fig, ax = plt.subplots(figsize=(8, 8))
    ax.set_xlim(-1, spec.width)
    ax.set_ylim(-1, spec.height)
    ax.set_aspect("equal")
    ax.set_title("Optimized Drone Paths")
    ax.set_xlabel("x")
    ax.set_ylabel("y")

    draw_workspace(ax, spec)
    draw_tours(ax, spec, tours)

    png_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(png_path, dpi=200)

    if mp4_path is not None:
        try:
            animate(fig, ax, tours, mp4_path)
        except (FileNotFoundError, RuntimeError):
            print("Warning: ffmpeg not available, skipping MP4 export")

    plt.close(fig)

    heat = heatmap_path or png_path.with_name("coverage_age.png")
    gantt = gantt_path or png_path.with_name("sortie_gantt.png")
    render_coverage_age(spec, tours, heat)
    render_sortie_gantt(tours, gantt)


def draw_workspace(ax, spec: MapSpec) -> None:
    detailed = max(spec.width, spec.height) <= DETAIL_LIMIT
    if detailed:
        ax.set_xticks(range(spec.width))
        ax.set_yticks(range(spec.height))
        ax.grid(True, which="both", color="#bbbbbb", linewidth=0.5, linestyle="--")
    else:
        ax.xaxis.set_major_locator(MaxNLocator(integer=True, nbins=10))
        ax.yaxis.set_major_locator(MaxNLocator(integer=True, nbins=10))

    mask = np.zeros((spec.height, spec.width), dtype=bool)
    for x, y in spec.obstacles:
        mask[y, x] = True
    layer = np.zeros((spec.height, spec.width, 4))
    layer[mask] = (0.27, 0.27, 0.27, 1.0)
    ax.imshow(
        layer,
        origin="lower",
        extent=(-0.5, spec.width - 0.5, -0.5, spec.height - 0.5),
        interpolation="nearest",
        zorder=1,
    )

    for poly in spec.no_fly_polygons:
        for ring in polygon_rings(poly):
            ax.add_patch(
                MplPolygon(ring, closed=True, color="#ffa07a", alpha=0.4, zorder=2)
            )

    for depot in spec.depots:
        ax.plot(depot[0], depot[1], marker="s", color="black", markersize=6, zorder=5)


def draw_tours(ax, spec: MapSpec, tours: List[dict]) -> None:
    detailed = max(spec.width, spec.height) <= DETAIL_LIMIT
    for idx, tour in enumerate(tours):
        color = COLORS[idx % len(COLORS)]
        waypoints = sorted(tour.get("waypoints", []), key=lambda wp: wp["index"])
        xs = [wp["x"] for wp in waypoints]
        ys = [wp["y"] for wp in waypoints]
        ax.plot(
            xs,
            ys,
            marker="o" if detailed else None,
            markersize=3,
            color=color,
            linewidth=1.5 if detailed else 0.6,
            alpha=1.0 if detailed else 0.8,
            label=f"Drone {tour.get('drone_id', idx)}",
            zorder=3,
        )
    if tours:
        ax.legend(loc="upper right")


def render_coverage_age(spec: MapSpec, tours: Sequence[dict], path: Path) -> Path:
    """Heat map of first-visit time (early = cool, late = hot)."""

    age = np.full((spec.height, spec.width), np.nan, dtype=float)
    for tour in tours:
        for wp in tour.get("waypoints") or []:
            x, y = int(wp["x"]), int(wp["y"])
            if not (0 <= x < spec.width and 0 <= y < spec.height):
                continue
            t = float(wp.get("t", 0.0))
            prev = age[y, x]
            if np.isnan(prev) or t < prev:
                age[y, x] = t

    fig, ax = plt.subplots(figsize=(8, 7))
    masked = np.ma.masked_invalid(age)
    cmap = plt.get_cmap("viridis").copy()
    cmap.set_bad(color="#2b2b2b")
    im = ax.imshow(
        masked,
        origin="lower",
        extent=(-0.5, spec.width - 0.5, -0.5, spec.height - 0.5),
        interpolation="nearest",
        cmap=cmap,
    )
    for depot in spec.depots:
        ax.plot(depot[0], depot[1], marker="s", color="white", markersize=5)
    ax.set_title("Coverage age (first visit time)")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.set_aspect("equal")
    fig.colorbar(im, ax=ax, fraction=0.046, pad=0.04, label="t (s)")
    path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(path, dpi=180, bbox_inches="tight")
    plt.close(fig)
    return path


def render_sortie_gantt(tours: Sequence[dict], path: Path) -> Path:
    """Horizontal Gantt of sorties per drone (split on recharge stops)."""

    fig, ax = plt.subplots(figsize=(10, max(2.5, 0.9 * max(len(tours), 1) + 1.5)))
    yticks = []
    ylabels = []
    for row, tour in enumerate(sorted(tours, key=lambda t: int(t.get("drone_id", 0)))):
        drone = int(tour.get("drone_id", row))
        color = COLORS[row % len(COLORS)]
        wps = sorted(tour.get("waypoints") or [], key=lambda wp: wp["index"])
        if not wps:
            continue
        stops = set(tour.get("recharge_stops") or [])
        bounds = [wps[0]["index"], *sorted(stops), wps[-1]["index"]]
        uniq: list[int] = []
        for b in bounds:
            if not uniq or b != uniq[-1]:
                uniq.append(b)
        for i in range(len(uniq) - 1):
            start_idx, end_idx = uniq[i], uniq[i + 1]
            window = [wp for wp in wps if start_idx <= wp["index"] <= end_idx]
            if len(window) < 2:
                continue
            t0 = float(window[0].get("t", 0.0))
            t1 = float(window[-1].get("t", t0))
            width = max(t1 - t0, 0.05)
            ax.add_patch(
                Rectangle(
                    (t0, row - 0.35),
                    width,
                    0.7,
                    facecolor=color,
                    edgecolor="#111111",
                    linewidth=0.4,
                    alpha=0.85,
                )
            )
        yticks.append(row)
        ylabels.append(f"Drone {drone}")

    ax.set_yticks(yticks)
    ax.set_yticklabels(ylabels)
    ax.set_xlabel("time (s)")
    ax.set_title("Sortie Gantt")
    ax.set_ylim(-0.8, max(len(tours) - 0.2, 0.8))
    ax.grid(True, axis="x", linestyle="--", alpha=0.4)
    if tours:
        tmax = max(
            float(wp.get("t", 0.0))
            for tour in tours
            for wp in (tour.get("waypoints") or [{"t": 0.0}])
        )
        ax.set_xlim(0, max(tmax * 1.05, 1.0))
    path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(path, dpi=180, bbox_inches="tight")
    plt.close(fig)
    return path


def animate(fig, ax, tours: List[dict], mp4_path: Path) -> None:
    frames = []
    for idx, tour in enumerate(tours):
        color = COLORS[idx % len(COLORS)]
        waypoints = sorted(tour.get("waypoints", []), key=lambda wp: wp["index"])
        frames.append(
            (color, [wp["x"] for wp in waypoints], [wp["y"] for wp in waypoints])
        )

    max_points = max((len(xs) for _, xs, _ in frames), default=0)
    step = max(1, max_points // 300)
    lines = [ax.plot([], [], color=color, linewidth=2)[0] for color, _, _ in frames]

    def update(frame_index: int):
        progress = min((frame_index + 1) * step, max_points)
        for line, (_, xs, ys) in zip(lines, frames, strict=True):
            line.set_data(xs[:progress], ys[:progress])
        return lines

    ani = animation.FuncAnimation(
        fig, update, frames=(max_points + step - 1) // step, blit=True, interval=50
    )
    mp4_path.parent.mkdir(parents=True, exist_ok=True)
    ani.save(mp4_path, writer="ffmpeg")


__all__ = ["visualize", "render_coverage_age", "render_sortie_gantt"]
