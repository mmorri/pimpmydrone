# Coverage Strategies Overview

This document illustrates the differences between the spanning tree coverage (STC) and boustrophedon (lawnmower) strategies used in the project.

![STC vs Boustrophedon](../examples/figures/coverage_strategies.png)

- **STC** builds a spanning tree over the target region and traverses each edge twice to guarantee coverage with minimal overlap.
- **Boustrophedon** sweeps the region in alternating horizontal or vertical strips, which is efficient for rectangular spaces but may require partition-specific adjustments around obstacles.

Both strategies are implemented by the Haskell planner and can be selected via `--coverage-algorithm`.
