# Changelog

## Unreleased

### Added

- **Perception Lab (0.4.0):** YOLO / ONNX CNN / VLM / domain adapters, shared
  detection schema, class→role ontologies, compile-to-site GeoJSON, API + CLI,
  hangar Perception Lab UI. CV outputs are untrusted — not sense-and-avoid.

### Changed

- Mid-tour battery recalls now recharge at the **nearest depot** (cheapest
  turn-aware return) instead of always the region\'s home depot. Tours still
  start and finish at the assigned home.

- Depot commute legs **cover uncovered work cells on the way** (biased routing
  with cheapest-tree fallback), and already-covered route cells are skipped.
- Region growing balances by **accumulated round-trip commute cost**, not
  raw cell count.

### Added

- Hangar-glam website art, product gallery, favicon, cache-busted assets.
- **Public website** (`website/`) with a mandatory liability gate, `/legal`, and
  `/donate`. Funding model is **donations only** — no subscriptions.
- **Safety bundle** on every export: `SAFETY_REPORT.md`, `DISCLAIMER.txt`,
  `README_FLYING.md`, plus planner warnings in the API response.
- API requires `accepted_disclaimer: true` before planning (`LEGAL.md`).
- Product slice: GeoJSON site import, vertical presets, QGC/GeoJSON/CSV export,
  `fleet from-geo` / `export` / `serve`.

### Changed

- Removed API-key / paid-tier framing. Demo grid caps are fair-use only.

## Earlier

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

- `--coverage-algorithm auto`, now the default: each region is planned with
  both coverage patterns and the faster tour is kept. The patterns suit
  different terrain and the crossover happens within a single map, so mixing
  them per region beats either one. Across the benchmark matrix this is 13.9%
  less flight and 15.5% less makespan than the previous STC default, and 4.3%
  less flight than always choosing the better single pattern.
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
