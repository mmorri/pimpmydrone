module Coverage.STC
  ( planSTC
  ) where

import Data.Foldable (toList)
import qualified Data.HashSet as HS
import Data.List (foldl')
import qualified Data.Sequence as Seq
import Domain.Cell (Cell, neighbors4)
import Partition.Common (RegionAssignment (..))

-- | Spanning-tree coverage: depth-first walk of a spanning tree over the
-- region, traversing every tree edge twice so the walk returns to its root.
-- If the region has several connected components each one gets its own walk;
-- the planner stitches the walks together through free space.
planSTC :: RegionAssignment -> [Cell]
planSTC RegionAssignment {..} = concat (go (HS.singleton raSeed) (raSeed : raCells))
  where
    regionSet = HS.fromList (raSeed : raCells)
    neighbors c = filter (`HS.member` regionSet) (neighbors4 c)

    -- Walk one component per unvisited root, in the order cells were listed.
    go _ [] = []
    go visited (root : rest)
      | root /= raSeed && HS.member root visited = go visited rest
      | otherwise =
          let (visited', trail) = dfs root (HS.insert root visited)
           in (root : toList trail) : go visited' rest

    dfs :: Cell -> HS.HashSet Cell -> (HS.HashSet Cell, Seq.Seq Cell)
    dfs cell visited = foldl' step (visited, Seq.empty) (neighbors cell)
      where
        step (vis, acc) nxt
          | HS.member nxt vis = (vis, acc)
          | otherwise =
              let (visFinal, childTrail) = dfs nxt (HS.insert nxt vis)
               in (visFinal, acc Seq.>< ((nxt Seq.<| childTrail) Seq.|> cell))
