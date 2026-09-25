module Partition.Strip
  ( stripPartition
  ) where

import qualified Data.HashSet as HS
import Domain.Cell (Cell (..))
import Domain.Depot (Depot (..))
import Domain.Map (GridMap (..), reachableFree)
import Partition.Common (RegionAssignment (..))

-- | Partition the depot-reachable free cells into vertical strips of equal width.
-- Empty strips are dropped so every region has at least one cell.
stripPartition :: GridMap -> [Depot] -> Int -> [RegionAssignment]
stripPartition gm depots nRequested =
  zipWith mkAssignment [0 ..] (filter (not . null) stripes)
  where
    n = max 1 nRequested
    free = HS.toList (reachableFree gm (fmap depotCell depots))
    width = gmWidth gm
    stripeWidth = max 1 (ceiling (fromIntegral width / fromIntegral n :: Double))
    ranges = [(i * stripeWidth, min width ((i + 1) * stripeWidth)) | i <- [0 .. n - 1]]
    stripes = [filter (\(Cell x _) -> x >= start && x < end) free | (start, end) <- ranges]
    mkAssignment idx cells = RegionAssignment idx (minimum cells) cells
