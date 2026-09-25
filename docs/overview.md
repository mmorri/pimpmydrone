# Algorithms Overview

## Partitioning (`haskell/src/Partition/`)

Both partitioners first compute the free cells reachable from the depots with a breadth-first flood fill (`Domain.Map.reachableFree`); everything else is ignored.

- **Region growing** (`--method region`, default). Seeds are the depots plus farthest-point samples of the reachable set. Regions then grow breadth-first in round-robin order, each claiming one unclaimed cell per round. Because every region grows at the same rate, sizes stay balanced where the geometry allows, and every region is connected by construction.
- **Strips** (`--method strip`). Vertical strips of equal width. Simple and predictable, but strips may be disconnected by obstacles; the planner bridges the gaps.

## Coverage patterns (`haskell/src/Coverage/`)

| STC | Boustrophedon |
| --- | --- |
| ![STC](figures/coverage_stc.png) | ![Boustrophedon](figures/coverage_boustro.png) |

- **STC** (`--coverage-algorithm stc`) walks a depth-first spanning tree of the region and traverses every edge twice, so the walk ends where it started. Every connected component of the region gets its own walk.
- **Boustrophedon** (`--coverage-algorithm boustro`) sweeps rows in alternating directions and joins non-adjacent consecutive cells with a BFS shortest path inside the region. It produces fewer turns and shorter routes on open areas.

## Tour building (`Coverage.Repair`)

The route is a list of target cells; `buildPath` turns it into a timed, battery-feasible tour:

1. A BFS tree rooted at the depot gives, for every cell, the distance home, the first step of the way back and the number of turns along it.
2. Cells whose round trip (`takeoff + 2·(distance/v + turns) + 1 + landing`) exceeds `B` are dropped and reported.
3. Consecutive route cells that are not adjacent are bridged with A* restricted to in-range cells.
4. Before every single move the planner checks `battery + move + return_cost(target) ≤ B`. If it fails, the drone flies home along the tree, lands (a recharge stop), takes off and flies to the target along the tree, then resumes.

The invariant guarantees a recharge is always possible from anywhere the drone is allowed to be.

## Scheduling (`python/src/fleet/optimize.py`)

A CP-SAT model with one optional interval per (tour, drone). Constraints: each tour on exactly one drone, no overlap per drone, tours of different depots never share a drone, symmetry breaking on drone ids. The objective is `makespan + drone_cost · drones`; with `--deadline` the fleet size is minimised lexicographically first.

## Simulation (`python/src/fleet/simulate.py`)

Replays every tour and checks continuity (consecutive waypoints identical or adjacent), obstacle avoidance, per-sortie battery accounting (reset at recharge stops) and coverage of all reachable cells. `metrics.json` records coverage, reachable/unreachable counts, makespan, total time and length, per-drone stats and the list of detected problems.
