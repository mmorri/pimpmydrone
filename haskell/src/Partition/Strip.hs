module Partition.Strip
  ( stripPartition
  ) where

import Domain.Cell (Cell (..))
import Domain.Depot (Depot (..))
import Domain.Map (GridMap (..), freeCells)
import Partition.Common (RegionAssignment (..))

-- | Partition using vertical strips with approximately equal numbers of cells.
stripPartition :: GridMap -> [Depot] -> Int -> [RegionAssignment]
stripPartition gm _depots nRequested
  | n <= 1 = [RegionAssignment 0 seed free]
  | otherwise = zipWith mkAssignment [0 ..] stripes
  where
    n = max 1 nRequested
    free = freeCells gm
    seed = case free of
      (c : _) -> c
      [] -> Cell 0 0
    width = gmWidth gm
    stripeWidth = ceiling (fromIntegral width / fromIntegral n :: Double)
    ranges = [ (i, i * stripeWidth, min width ((i + 1) * stripeWidth)) | i <- [0 .. n - 1] ]
    stripes =
      [ filter (\(Cell x _) -> x >= start && x < end) free
      | (_, start, end) <- ranges
      ]
    mkAssignment idx cells =
      let seed = case cells of
            (c : _) -> c
            [] -> Cell (fstRange idx) 0
       in RegionAssignment idx seed cells
    fstRange idx =
      let (_, start, _) = ranges !! idx
       in start
