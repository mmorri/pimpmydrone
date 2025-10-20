module Coverage.STC
  ( planSTC
  ) where

import Data.List (foldl')
import qualified Data.HashSet as HS
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Domain.Cell (Cell, neighbors4)
import Partition.Common (RegionAssignment (..))

-- | Spanning tree coverage path returning to the seed.
-- Optimized version using Seq for O(1) append operations
planSTC :: RegionAssignment -> [Cell]
planSTC RegionAssignment {..} = seed : toList trail
  where
    seed = raSeed
    regionSet = HS.fromList raCells
    (_, trail) = dfs seed (HS.singleton seed)

    dfs :: Cell -> HS.HashSet Cell -> (HS.HashSet Cell, Seq.Seq Cell)
    dfs cell visited = foldl' step (visited, Seq.empty) (neighbors cell)
      where
        neighbors c = filter (`HS.member` regionSet) (neighbors4 c)
        step (vis, acc) nxt
          | HS.member nxt vis = (vis, acc)
          | otherwise =
              let vis' = HS.insert nxt vis
                  (visFinal, childTrail) = dfs nxt vis'
                  -- Use Seq for O(1) append operations
                  trail' = acc Seq.>< ((nxt Seq.<| childTrail) Seq.|> cell)
               in (visFinal, trail')
