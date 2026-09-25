# coverage-core

Pure Haskell core of the multi-drone coverage optimizer: domain model, grid search (`Algorithm.AStar`, `Algorithm.Flood`), partitioners, coverage planners and the battery-aware tour builder, plus the `coverage-partitioner` and `coverage-planner` executables.

```bash
stack build --copy-bins --local-bin-path bin   # or `make haskell-build` from the root
stack test                                      # QuickCheck properties
bin/coverage-partitioner --help
bin/coverage-planner --help
```

See the root `README.md` and `docs/overview.md` for the algorithms.
