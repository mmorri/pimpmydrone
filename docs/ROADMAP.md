# Roadmap

Priorities come from measurements, not intuition. `make bench` writes the table
this document argues from; `benchmarks/baseline.md` is the current state.

## Where the flight time goes

Across the benchmark matrix (24 missions, all reaching 100% coverage of the
cells in range):

| | share of all flown steps |
| --- | --- |
| Commuting between a depot and the area being covered | **57.7%** |
| Flying inside the region | 42.3% |

Per-cell, the average covered cell is flown over **2.3 to 13.6 times** depending
on the case. Two independent problems produce that number:

1. **Commuting.** Every sortie pays a return trip to the depot. On
   `tight-battery-40` this reaches 90% of all flight; even on the roomy
   `large-100` it is 32%.
2. **Overlap.** The spanning-tree walk traverses every tree edge twice, so
   in-region flight alone is about 2x the cell count. On open maps
   boustrophedon already does this in 2.33 steps per cell against STC's 4.09.

Everything below attacks one of those two numbers, or makes them cheaper to
attack.

## Phase 1 — Cut the commute

- **Recharge at the nearest depot.** Done: mid-tour recalls divert to the
  cheapest turn-aware depot; tours still start/finish at the assigned home.
  Multi-depot maps already showed the payoff (`twodepot-60` at 30-50% commute
  vs 67-90% single-depot); recharges can now exploit that mid-sortie.
- **Cover ground while commuting.** Done: depot legs (recall, outbound after
  recharge, and the final home leg) run a turn-aware search that prefers still-
  uncovered work cells, then fall back to the cheapest return tree if the biased
  path would break the battery reserve. Route cells already flown over during a
  commute are skipped so the work is not repeated.
- **Cost-balanced partitioning.** Done: region growing claims the next cell for
  whichever region has the lowest accumulated round-trip cost to its serving
  depot, so distant work does not pile onto one drone the way equal cell
  counts did.

### Tried and rejected: sortie-first decomposition

The idea was to split each region into battery-feasible clusters banded by
distance from the depot, so a long trip out is paid once for a whole
neighbourhood instead of once per interruption. Implemented and measured, it
was worse in every case -- on `tight-battery-40` it raised flight from 8576 to
22672 steps and sorties from 123 to 324.

The reason is visible in the budget profile of that map. Of the 922 cells in
range, 337 sit in the 30-39 distance band, where a round trip leaves about
seven cells' worth of battery for actual work. Clusters are seeded at the
furthest cell, so they inherit that near-zero budget and degenerate into
hundreds of tiny sorties, while the direct strategy covers those same far cells
opportunistically as it passes over them.

The lesson generalises: where the commute has already eaten the charge, no
regrouping of the work helps. Only shortening the commute (more depots) or
making the commute productive (covering on the way) can. Hence the two items
above. The implementation is in the history if it is worth revisiting for
short-commute maps.

## Phase 2 — Cut the overlap

- **Spanning-tree coverage over 2x2 mega-cells.** Done: `--coverage-algorithm stc`
  (and auto) plans a DFS spanning tree on complete 2x2 blocks, covers each block
  with a 4-cell snake, and patches orphan cells with the fine-cell walk. On
  `open-40` seed 1 this cut STC flight from ~6544 to ~2618 steps at 100% coverage.
- **Route shortcutting.** Done: drop revisit loops whose interior cells still
  appear elsewhere, then re-patch any cells the shortcut uniquely removed.
- **Per-region pattern choice** is done: see below.

## Phase 3 — Give the optimizer a real decision

- **Sortie assignment.** Done (default `--schedule-mode sortie`): recharge stops
  split into CP-SAT tasks so drones can interleave work from the same depot.
  `--schedule-mode tour` keeps the old whole-region behaviour.
- Longer term: set-partitioning over generated sortie columns (column
  generation), which folds partitioning and assignment into one model.

## Phase 4 — Scale and fidelity

- **Spatial deconfliction.** Done at discrete fidelity: detect same-cell /
  same-time clashes, stagger later drones, report in metrics / safety / QGC
  readme. Not certified airborne separation.
- **Sensor / GSD inspection report.** Done: FOV→footprint model writes
  `metrics.inspection` (approx. imaged cells + GSD estimate).
- **QGC export fidelity.** Done: RelHome/AGL altitude mode, inclusion site +
  avoid keep-outs as geofence polygons when present on `geo.json`.
- **Flat arrays instead of hash maps.** Done: `DepotField` stores Dijkstra
  state in unboxed vectors indexed by `y * width + x` (and heading). Same
  turn-aware semantics; less HashMap heap traffic on large grids.
- **Compact tour encoding.** Done: `fleet.compact` writes `tours_compact.json`
  (`runs-v1` direction runs) plus `compact_stats.json` beside expanded tours.
- **Parallel region planning.** Done: `coverage-planner` builds unique depot
  fields once in parallel, then plans regions with `mapConcurrently` (RTS `-N`).
- **Remove the duplicated `params`.** Done: kinematics live only in
  `params.json`; `map.json` no longer embeds a copy.
- **Richer visuals.** Done: `coverage_age.png` (first-visit heat map) and
  `sortie_gantt.png` beside `path.png`.
- **OSM building keep-outs.** Done: `fleet enrich-osm` and
  `--osm-buildings` on `from-geo` (Overpass → avoid polygons). Still not DEM /
  wind / NOTAM — flat-world disclaimer stands.

## Done

- **Flat depot field, compact tours, parallel region planning.** See Phase 4.
- **Coverage-age heatmap, sortie Gantt, OSM building import, params dedup.**
- **Sortie scheduling + discrete deconfliction + inspection metrics + QGC
  geofence/AGL export + industrial solar example + dual donation CTAs.**
- **Mega-cell STC + route shortcutting.** See Phase 2. Auto now compares
  mega-STC, fine-cell STC, and boustrophedon per region.
- **Perception + ML adapters.** Shared detection GeoJSON, solar/security/ag/thermal/wildlife
  ontologies, compile→site (cover/avoid/inspect), YOLO / ONNX / VLM / domain
  backends, API/CLI, and hangar Perception Lab. Not sense-and-avoid.
- **Per-region pattern choice.** The two coverage patterns win on different
  terrain, and the crossover happens within a single map, not just between
  maps. `--coverage-algorithm auto` (now the default) plans a region with both
  and keeps the faster tour. Across the matrix that is 13.9% less flight and
  15.5% less makespan than the previous STC default, and it beats the better
  single pattern by 4.3% because it mixes them across regions.
- **Phase 0 — measurement.** `make bench` over a fixed matrix with stored
  baselines, `fleet.analyze` breaking flight into commuting and useful work,
  golden tests pinning planner output, and the simulator independently
  verifying every cell the planner declares out of range.
