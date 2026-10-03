module Partition.RegionGrow
  ( regionGrow
  ) where

import Domain.Depot (Depot)
import Domain.Drone (DroneSpec)
import Domain.Map (GridMap)
import Partition.Common (RegionAssignment, assignRegions, chooseSeeds)

-- | Partition using multi-source flood fill from depot-derived seeds.
-- Regions are balanced by accumulated round-trip commute cost rather than
-- raw cell count, so distant work does not overload a single drone.
regionGrow :: GridMap -> [Depot] -> DroneSpec -> Int -> [RegionAssignment]
regionGrow gm depots drone n =
  assignRegions gm drone depots (chooseSeeds gm depots n)
