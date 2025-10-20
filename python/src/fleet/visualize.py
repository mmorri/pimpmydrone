"""Static and animated visualization of drone tours."""

from __future__ import annotations

from pathlib import Path
from typing import Iterable, List

import matplotlib.pyplot as plt
from matplotlib import animation
from matplotlib.collections import PatchCollection
from matplotlib.patches import Polygon as MplPolygon

from . import io
from .map_utils import MapSpec

COLORS = [
    "#1f77b4",
    "#ff7f0e",
    "#2ca02c",
    "#d62728",
    "#9467bd",
    "#8c564b",
]


def visualize(map_path: Path, tours_path: Path, png_path: Path, mp4_path: Path | None = None) -> None:
    map_data = io.read_json(map_path)
    tours = io.read_json(tours_path)
    spec = MapSpec.from_json(map_data)

    fig, ax = plt.subplots(figsize=(8, 8))
    ax.set_xlim(-1, spec.width + 1)
    ax.set_ylim(-1, spec.height + 1)
    ax.set_aspect("equal")
    ax.set_title("Optimized Drone Paths")
    ax.set_xlabel("x")
    ax.set_ylabel("y")

    draw_workspace(ax, spec)
    draw_tours(ax, tours)

    png_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(png_path, dpi=200)

    if mp4_path is not None:
        try:
            animate(fig, ax, spec, tours, mp4_path)
        except FileNotFoundError:
            # ffmpeg is optional; fall back gracefully
            print("Warning: ffmpeg not available, skipping MP4 export")

    plt.close(fig)


def draw_workspace(ax, spec: MapSpec) -> None:
    ax.grid(True, which="both", color="#bbbbbb", linewidth=0.5, linestyle="--")
    ax.set_xticks(range(spec.width))
    ax.set_yticks(range(spec.height))

    for cell in spec.obstacles:
        rect = plt.Rectangle((cell[0] - 0.5, cell[1] - 0.5), 1, 1, color="#444444")
        ax.add_patch(rect)

    patches = [MplPolygon(poly.exterior.coords, closed=True, color="#ffa07a", alpha=0.4) for poly in spec.no_fly_polygons]
    for patch in patches:
        ax.add_patch(patch)

    for depot in spec.depots:
        ax.plot(depot[0], depot[1], marker="s", color="black", markersize=6)


def draw_tours(ax, tours: List[dict]) -> None:
    for idx, tour in enumerate(tours):
        color = COLORS[idx % len(COLORS)]
        waypoints = sorted(tour.get("waypoints", []), key=lambda wp: wp["index"])
        xs = [wp["x"] for wp in waypoints]
        ys = [wp["y"] for wp in waypoints]
        ax.plot(xs, ys, marker="o", color=color, linewidth=1.5, label=f"Drone {tour.get('drone_id', idx)}")
    if tours:
        ax.legend(loc="upper right")


def animate(fig, ax, spec: MapSpec, tours: List[dict], mp4_path: Path) -> None:
    frames = []
    for idx, tour in enumerate(tours):
        color = COLORS[idx % len(COLORS)]
        waypoints = sorted(tour.get("waypoints", []), key=lambda wp: wp["index"])
        xs = [wp["x"] for wp in waypoints]
        ys = [wp["y"] for wp in waypoints]
        frames.append((color, xs, ys))

    artists = []
    def init():
        return []

    def update(frame_index: int):
        artists = []
        for color, xs, ys in frames:
            progress = min(frame_index + 1, len(xs))
            artists.extend(ax.plot(xs[:progress], ys[:progress], color=color))
        return artists

    max_points = max((len(xs) for _, xs, _ in frames), default=0)
    ani = animation.FuncAnimation(fig, update, frames=max_points, init_func=init, blit=True, interval=200)
    mp4_path.parent.mkdir(parents=True, exist_ok=True)
    ani.save(mp4_path, writer="ffmpeg")


__all__ = ["visualize"]
