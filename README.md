# Multi-drone Area Coverage Optimizer

_End-to-end toolkit for planning, scheduling and simulating cooperative drone coverage missions over cluttered 2D workspaces._

![Boustrophedon coverage with three drones](docs/figures/coverage_boustro.png)

## Problem Statement

Given a `W x H` grid with blocked cells (obstacles), polygonal no-fly zones and one or more depots, plan tours for up to `max_drones` identical drones so that **every free cell a depot can reach is visited**. Drones move on the 4-connected grid at speed `v`, pay one time unit per heading change, carry a battery of `B` time units per sortie and must take off from and land at a depot (costing `takeoff_cost` / `landing_cost`). The pipeline minimises makespan while keeping the fleet as small as that allows, and can alternatively minimise the fleet under a makespan deadline.

## Pipeline

```
generate ──► partition ──► plan ──► optimize ──► simulate ──► visualize
 (python)    (haskell)    (haskell)   (python)     (python)     (python)
 map.json    regions.json tours_initial.json  tours_optimized.json  metrics.json  path.png
```

| Stage | Where | What it does |
| --- | --- | --- |
| generate | `python/src/fleet/generate_map.py` | Random obstacles, star-shaped no-fly polygons, depots on free cells; writes `map.json` + `params.json`. |
| partition | `haskell/src/Partition/` | Splits the cells **reachable from the depots** into one connected region per drone. `region` (default) grows regions round-robin from the depots and farthest-point seeds so sizes stay balanced; `strip` cuts vertical strips. |
| plan | `haskell/src/Coverage/` | Builds a coverage route per region (`stc`: spanning-tree walk, `boustro`: lawnmower sweep) and turns it into a timed tour with `Coverage.Repair`, which keeps enough battery to fly back to the depot along a precomputed shortest-path tree, inserts recharge stops, and skips cells whose round trip cannot fit one charge. |
| optimize | `python/src/fleet/optimize.py` | CP-SAT model assigning tours to physical drones (a drone may fly several tours back to back after recharging), never mixing depots. Objective: `makespan + drone_cost · drones`, or drones-first under `--deadline`. |
| simulate | `python/src/fleet/simulate.py` | Replays the tours and checks continuity, obstacle avoidance, per-sortie battery budget and coverage of every reachable cell. Unreachable pockets are reported, not counted as gaps. |
| visualize | `python/src/fleet/visualize.py` | PNG of the workspace and tours, optional MP4 (needs ffmpeg). |

JSON contracts between the stages live in `schema/` and are validated by the Python test suite.

## Quick Start

Requirements: [`stack`](https://docs.haskellstack.org) (GHC 9.6 via LTS 22) and [`uv`](https://docs.astral.sh/uv/).

```bash
make setup                       # install toolchains and dependencies
make build                       # builds haskell/bin/coverage-{partitioner,planner}
make run SEED=1337               # full pipeline, outputs in outputs/1337/
make run SEED=7 ARGS="--width 60 --height 60 --depots 2 --battery 400 --coverage-algorithm boustro"
make test                        # QuickCheck properties + pytest (incl. end-to-end runs)
```

Every stage is also available on its own:

```bash
cd python
uv run python -m fleet generate --seed 42 --width 50 --height 50 --obstacle-density 0.2
uv run python -m fleet plan     --map ../outputs/42/map.json --params ../outputs/42/params.json \
                                --output ../outputs/42/tours_initial.json --algorithm stc --partition region
uv run python -m fleet optimize --input ../outputs/42/tours_initial.json --output ../outputs/42/tours_optimized.json \
                                --max-drones 5 --deadline 2000
uv run python -m fleet simulate --map ../outputs/42/map.json --params ../outputs/42/params.json \
                                --tours ../outputs/42/tours_optimized.json --metrics ../outputs/42/metrics.json
uv run python -m fleet visualize --map ../outputs/42/map.json --tours ../outputs/42/tours_optimized.json --png ../outputs/42/path.png
```

`simulate` (and `run`) exit non-zero when the plan violates a check; the details are in `metrics.json` under `checks.problems`.

Set `FLEET_ROOT`, `FLEET_OUTPUTS` or `FLEET_HASKELL_BIN` to relocate the repository root, the output directory or the Haskell binaries.

## Scale

Measured on a laptop, single depot, `--obstacle-density 0.01 --battery 5000`:

| Grid | Reachable cells | Partition | Plan | Optimize | Simulate | Peak RSS |
| --- | --- | --- | --- | --- | --- | --- |
| 100 x 100 | ~8k | < 1 s | < 1 s | ~2 s | < 1 s | small |
| 1000 x 1000 | ~990k | 8 s | 34 s | 11 s | 10 s | ~3 GB |

## Design Notes

- **Reachability first.** Partitioning, planning and the coverage metric all work on the set of free cells reachable from a depot. Pockets sealed off by obstacles or no-fly zones are neither assigned nor counted.
- **Battery invariant.** While airborne the drone always holds `battery_used + return_cost(current) ≤ B`, where `return_cost` follows the depot's BFS tree (distance, turns and landing included). A move that would break the invariant triggers a recharge first. Cells whose round trip exceeds `B` are reported on stderr and skipped rather than failing the mission.
- **Makespan vs fleet size.** Executing all tours on one drone is always feasible, so "minimise drones" alone is degenerate. The optimizer therefore minimises makespan and uses `--drone-cost` (seconds of makespan one extra drone is worth, default 1) to prefer smaller fleets on ties; `--deadline` flips the priority.
- **Path length** is fixed once tours are planned (rearranging tours between drones does not change it), so it is reported but not optimised.

## Repository Layout

- `haskell/` – pure core: domain model, A*/BFS, partitioners, coverage planners, battery-aware tour builder, CLI executables.
- `python/` – map generator, CP-SAT optimizer, simulator, visualizer and the `fleet` CLI.
- `schema/` – JSON schemas for every artefact exchanged between stages.
- `docs/` – algorithm overview and figures.
- `tools/` – LOC reporting.
- `.github/workflows/` – Haskell, Python and end-to-end pipeline CI.

See [docs/overview.md](docs/overview.md) for the algorithms and [CONTRIBUTING.md](CONTRIBUTING.md) for the development workflow.
