# Auto vs lawnmower

Two measurements, both from 2026-10-03. “Lawnmower” means `--coverage-algorithm boustro` with region partition. “Auto” mixes mega-STC, fine STC, and boustrophedon per region (`--coverage-algorithm auto`).

## Industrial solar example

Site: `examples/sites/solar_industrial.geojson` (dual depots, substation keep-out, solar preset). Grid 96×98 at ~7.5 m, 8846 reachable cells, battery large enough that commute is about 2%.

| | makespan | length | drones | coverage |
| --- | --- | --- | --- | --- |
| Auto (`--partition auto`) | 458 s | 9498 | 4 | 100% |
| Lawnmower | 458 s | 9482 | 4 | 100% |

They tie. Auto is 0.2% longer. On a site that fits in one charge, pattern mixing does not buy flight. Raw JSON: `benchmarks/solar_industrial_comparison.json`. Reproduce with `tools/compare_lawnmower.py`.

## October matrix (where mixing pays)

Same 6 maps × 2 seeds as `benchmarks/baseline.md`. Auto flew less than boustro-only on **12/12** rows.

| | length | makespan |
| --- | --- | --- |
| Auto total | 94642 | — |
| Boustro-only total | 112466 | — |
| Auto vs lawnmower | **−15.8%** flight | **−13.9%** makespan |

Largest gaps are cluttered maps and `large-100` (about 21–24% less flight). Open and two-depot maps are only a few percent.
