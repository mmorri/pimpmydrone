module Coverage.Boustro
  ( planBoustro
  ) where

import Data.Function (on)
import Data.List (elemIndex, sortBy, sortOn)
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import Domain.Cell (Cell (..), neighbors4)
import Partition.Common (RegionAssignment (..))

-- | Boustrophedon (lawnmower) pattern with BFS connection across sparse regions.
planBoustro :: RegionAssignment -> [Cell]
planBoustro RegionAssignment {..}
  | null raCells = [raSeed]
  | otherwise = dedupe (stitch seed visitOrder)
  where
    regionSet = HS.fromList (raSeed : raCells)
    seed = raSeed
    rowMap = M.fromListWith (<>) [(y, [Cell x y]) | Cell x y <- raCells]
    orderedRows = zip [0 ..] (M.toAscList rowMap)
    serpentine = concatMap rowToList orderedRows
    rowToList (idx, (_y, cells))
      | even idx = sortOn Coverage.Boustro.cellX cells
      | otherwise = reverse (sortOn Coverage.Boustro.cellX cells)
    visitOrder = rotate seed serpentine
    rotate s cells =
      case elemIndex s cells of
        Just idx -> drop idx cells ++ take idx cells
        Nothing -> s : cells
    stitch current [] = [current]
    stitch current (next : rest)
      | current == next = current : stitch current rest
      | otherwise =
          let leg = shortestPath regionSet current next
           in init leg ++ stitch next rest
    dedupe (x : y : zs)
      | x == y = dedupe (y : zs)
      | otherwise = x : dedupe (y : zs)
    dedupe xs = xs

cellX :: Cell -> Int
cellX (Cell x _) = x

shortestPath :: HS.HashSet Cell -> Cell -> Cell -> [Cell]
shortestPath region start end
  | start == end = [start]
  | otherwise = reconstruct parents end
  where
    -- Use Seq for efficient queue operations
    parents = bfs (Seq.singleton (start, Nothing)) M.empty
    bfs queue acc
      | Seq.null queue = acc
      | otherwise =
          case Seq.viewl queue of
            Seq.EmptyL -> acc
            (cell, parent) Seq.:< rest
              | M.member cell acc -> bfs rest acc
              | cell == end -> M.insert cell parent acc
              | otherwise ->
                  let acc' = M.insert cell parent acc
                      neighbors = filter (`HS.member` region) (neighbors4 cell)
                      -- O(1) append to queue instead of O(n) list concatenation
                      queue' = foldl (Seq.|>) rest [(n, Just cell) | n <- neighbors]
                   in bfs queue' acc'
    reconstruct m cell = reverse (go cell [])
      where
        go c acc
          | c == start = start : acc
          | otherwise = case M.lookup c m of
              Nothing -> start : c : acc
              Just Nothing -> c : acc
              Just (Just parent) -> go parent (c : acc)
