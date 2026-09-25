module Coverage.Boustro
  ( planBoustro
  ) where

import Data.List (elemIndex, sortOn)
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import Domain.Cell (Cell (..), neighbors4)
import Partition.Common (RegionAssignment (..))

-- | Boustrophedon (lawnmower) sweep. Rows are visited in alternating
-- directions; consecutive cells that are not adjacent are joined by a
-- shortest path inside the region when one exists. When the region itself is
-- disconnected the route simply jumps and the planner bridges the gap.
planBoustro :: RegionAssignment -> [Cell]
planBoustro RegionAssignment {..}
  | null raCells = [raSeed]
  | otherwise = dedupe (stitch raSeed visitOrder)
  where
    regionSet = HS.fromList (raSeed : raCells)
    rowMap = M.fromListWith (<>) [(y, [Cell x y]) | Cell x y <- raCells]
    serpentine = concat (zipWith rowToList [0 :: Int ..] (M.elems rowMap))
    rowToList idx cells
      | even idx = sortOn cellX cells
      | otherwise = sortOn (negate . cellX) cells
    cellX (Cell x _) = x
    visitOrder = case elemIndex raSeed serpentine of
      Just idx -> drop idx serpentine ++ take idx serpentine
      Nothing -> raSeed : serpentine
    stitch current [] = [current]
    stitch current (next : rest)
      | current == next = stitch current rest
      | otherwise = init (shortestPath regionSet current next) ++ stitch next rest
    dedupe (x : y : zs)
      | x == y = dedupe (y : zs)
      | otherwise = x : dedupe (y : zs)
    dedupe xs = xs

-- | BFS shortest path within the region. Falls back to @[start, end]@ when the
-- two cells are not connected inside the region.
shortestPath :: HS.HashSet Cell -> Cell -> Cell -> [Cell]
shortestPath region start end
  | start == end = [start]
  | otherwise = maybe [start, end] id (bfs (Seq.singleton start) (M.singleton start Nothing))
  where
    bfs queue parents = case Seq.viewl queue of
      Seq.EmptyL -> Nothing
      cell Seq.:< rest
        | cell == end -> Just (walkBack parents end [])
        | otherwise ->
            let fresh = [n | n <- neighbors4 cell, HS.member n region, not (M.member n parents)]
                parents' = foldr (\n -> M.insert n (Just cell)) parents fresh
             in bfs (foldl (Seq.|>) rest fresh) parents'
    walkBack parents cell acc = case M.lookup cell parents of
      Just (Just parent) -> walkBack parents parent (cell : acc)
      _ -> cell : acc
