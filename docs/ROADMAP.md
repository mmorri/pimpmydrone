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

- **Sortie-first decomposition.** Today the route is a coverage pattern and the
  drone recharges wherever the battery happens to run out, which is often deep
  in the region. Split each region into battery-feasible sorties by distance
  band from the depot first, then run the coverage pattern within each sortie.
  Target: commute share below 35% on the tight-battery cases.
- **Recharge at the nearest depot.** `Coverage.Repair` is built around a single
  depot field. Multi-depot maps already show the payoff: `twodepot-60` runs at
  30-50% commute against 67-90% for single-depot cases of similar size.
- **Cost-balanced partitioning.** Regions are balanced by cell count, but a
  distant region costs far more per cell. Weight cells by round-trip cost when
  growing regions.

## Phase 2 — Cut the overlap

- **Spanning-tree coverage over 2x2 mega-cells.** The textbook formulation
  covers each fine cell once (about 1.05x) instead of the current 2x. This is
  the single largest reduction available to in-region flight.
- **Route shortcutting.** A cheap post-pass: where a route revisits cells,
  replace the detour with an A* leg when that is shorter. Applies to both
  patterns.
- **Choose the pattern per region.** STC wins on cluttered regions, boustro on
  open ones; the benchmark shows the crossover. Pick per region instead of
  per mission.

## Phase 3 — Give the optimizer a real decision

The CP-SAT stage only assigns whole regions to drones, so total path length is
fixed before it runs and the stage is really a scheduler.

- Assign **sorties** rather than whole regions, letting drones share a region.
- Longer term, set-partitioning over generated sortie columns (column
  generation), which folds partitioning and assignment into one model.

## Phase 4 — Scale and fidelity

- **Flat arrays instead of hash maps** in the Haskell core, indexed by
  `y * width + x`. The cheapest-return field keeps one entry per cell and
  heading, so on 1000x1000 the planner spends 50 s and 2.5 GB building it --
  the largest single cost in the pipeline, and the reason this item outranks
  the rest of this phase.
- **Compact tour encoding.** Tours serialise to roughly 360 MB of JSON at
  1000x1000; encoding moves as direction runs would cut that by an order of
  magnitude.
- **Parallel region planning.** Regions are independent.
- **Deconfliction.** Drones fly concurrently in the schedule but nothing checks
  that two of them never occupy a cell at the same time.
- **Remove the duplicated `params`** between `map.json` and `params.json`.
- **Richer visuals**: a sortie Gantt per drone, and a coverage-age heat map.

## Done

- **Phase 0 — measurement.** `make bench` over a fixed matrix with stored
  baselines, `fleet.analyze` breaking flight into commuting and useful work,
  golden tests pinning planner output, and the simulator independently
  verifying every cell the planner declares out of range.
