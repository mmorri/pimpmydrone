# Multi-drone Area Coverage Optimizer

_End-to-end open-source toolkit for planning and simulating cooperative drone coverage missions over cluttered 2D workspaces._

## Problem Statement

We target complete coverage of a 100x100 grid containing obstacles and polygonal no-fly zones using up to five identical drones. Drones must obey 4-connected kinematics, battery limits, and depot recharge constraints while minimizing (1) the number of deployed drones, (2) mission makespan, and (3) cumulative path length. The planner handles random map generation, region partitioning, coverage path creation, optimization via integer programming, simulation, and visualization.

## Repository Layout

- `haskell/` – Purely functional core: domain models, partitioners, coverage planners, cost evaluators, and JSON I/O.
- `python/` – Map generator, OR-Tools optimizer, mission simulator, visualizer, and CLI orchestrating the pipeline.
- `schema/` – JSON schemas for interop between components.
- `examples/` – Seeds, inputs, and outputs for regression checks.
- `ci/` – GitHub Actions workflows for both stacks.
- `docs/` – Conceptual overviews and diagrams.

## Quick Start

```bash
make setup
make run SEED=1337
make viz SEED=1337
```

Outputs and metrics are stored in `outputs/<SEED>/`.

Full documentation, algorithm details, and LOC parity assurances will be added as the implementation progresses.
