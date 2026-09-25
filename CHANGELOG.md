# Changelog

## Unreleased

### Fixed

The pipeline could not complete a mission end to end. All three reported cases
(10x10 seed 10, 100x100 seed 100, 1000x1000 seed 1000) now run.

- **Regions unreachable from the depot.** Seeds were sampled over all free
  cells, so extra seeds landed inside pockets sealed off by obstacles and the
  planner failed with "No feasible path between points". Partitioning now works
  only on depot-reachable cells.
- **Battery and recharging.** Only the landing cost was reserved, so long
  routes failed with "Battery budget insufficient while returning to depot";
  after a recall the planner compared the depot with a distant target and
  failed with "Invalid adjacency". Tour building is now organised around a
  reserve invariant and recharges work.
- **Cells refused as out of range.** The battery reserve was sized from a
  breadth-first tree that ignores turn costs, so the planner gave up on cells
  it could serve. The depot field is now a turn-aware Dijkstra, and the cost it
  quotes is always the cost of the route it hands out.
- **A\* dropping nodes.** The open set ordered entries by f-score alone, so
  distinct cells with equal f collapsed into one and paths could be missed.
- **Region imbalance.** Regions now grow round-robin. On 100x100 this moved
  region sizes from 591-3214 cells to 1284-1698 and makespan from 17709s to
  12912s.
- **STC skipping cells** not connected to the region seed.
- **CLI paths.** The repository root resolved to `python/`, so the Haskell
  binaries were never found and outputs were written to `python/outputs/`.
- **Optimizer objective.** Weights were unit-inconsistent, with makespan in
  milliseconds dominating the per-drone term, and the length term was constant.
  Tours belonging to different depots could be merged onto one drone.
- **Coverage accounting.** Cells in sealed pockets counted as coverage gaps.

### Added

- Battery-range reporting: the planner writes `skipped.json`, and the simulator
  verifies each claim against an independently computed lower bound.
- Simulator checks for path continuity and per-sortie battery use.
- `make bench` and `tools/bench.py`: a fixed mission matrix reporting makespan,
  flight length, commute share and steps per cell, with stored baselines under
  `benchmarks/`.
- `fleet.analyze`: breaks flight down into commuting and in-region work.
- Golden tests pinning planner output, end-to-end pipeline tests, and schema
  validation of every artefact.
- CI under `.github/workflows/` including an end-to-end pipeline job. Nothing
  under `ci/` had ever run.
- `--partition`, `--deadline`, `--drone-cost` options, and `FLEET_ROOT`,
  `FLEET_OUTPUTS`, `FLEET_HASKELL_BIN` overrides.
- `docs/ROADMAP.md`.

### Changed

- The Haskell library and executables build with `-Wall` and no warnings.
- The Haskell test suite fails the build when a property fails; it previously
  used `quickCheck`, which never sets a non-zero exit code.
- `simulate` and `run` exit non-zero when the plan violates a check.
- 1000x1000 missions complete in about 90 seconds. Planning is slower than a
  turn-oblivious breadth-first field would be (50 s against 34 s) because the
  reserve is now computed from the route the drone actually flies; on 100x100
  that buys a 23% lower makespan and removes the spurious out-of-range skips.
