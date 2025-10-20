module Partition.RegionGrow
  ( regionGrow
  ) where

import Domain.Map (GridMap)
import Domain.Depot (Depot)
import Partition.Common (RegionAssignment, assignRegions, chooseSeeds)

-- | Partition using multi-source breadth-first flood fill from depot-derived seeds.
regionGrow :: GridMap -> [Depot] -> Int -> [RegionAssignment]
regionGrow gm depots n =
  let seeds = chooseSeeds gm depots n
   in assignRegions gm seeds
