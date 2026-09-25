module Partition.RegionGrow
  ( regionGrow
  ) where

import Domain.Depot (Depot)
import Domain.Map (GridMap)
import Partition.Common (RegionAssignment, assignRegions, chooseSeeds)

-- | Partition using multi-source breadth-first flood fill from depot-derived seeds.
regionGrow :: GridMap -> [Depot] -> Int -> [RegionAssignment]
regionGrow gm depots n = assignRegions gm (chooseSeeds gm depots n)
