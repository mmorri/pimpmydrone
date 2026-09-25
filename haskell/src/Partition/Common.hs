module Partition.Common
  ( RegionAssignment (..)
  , chooseSeeds
  , assignRegions
  ) where

import Data.Function (on)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List (foldl', maximumBy, sort)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import Domain.Cell (Cell (..), manhattan)
import Domain.Depot (Depot (..))
import Domain.Map (GridMap (..), freeNeighbors, isFree, reachableFree)

-- | Region assigned to a drone, including its anchor seed.
data RegionAssignment = RegionAssignment
  { raDroneId :: !Int
  , raSeed :: !Cell
  , raCells :: ![Cell]
  }
  deriving (Eq, Show)

-- | Pick @n@ seeds: depots first, then farthest-point sampling over the cells
-- reachable from the depots, so that every seed can actually be served.
-- Cells in pockets that no depot can reach are never chosen.
chooseSeeds :: GridMap -> [Depot] -> Int -> [Cell]
chooseSeeds gm depots n
  | n <= 0 = []
  | n <= length depotSeeds = take n depotSeeds
  | null candidates = depotSeeds
  | otherwise = grow depotSeeds
  where
    depotSeeds = filter (isFree gm) (fmap depotCell depots)
    reachable = reachableFree gm depotSeeds
    candidates = HS.toList reachable
    grow seeds
      | length seeds >= n = seeds
      | otherwise =
          let unused = filter (`notElem` seeds) candidates
           in case unused of
                [] -> seeds
                _ -> grow (seeds ++ [maximumBy (compare `on` minDist seeds) unused])
    minDist seeds cell = minimum (fmap (`manhattan` cell) seeds)

-- | Balanced region growing. Regions expand breadth-first from their seeds in
-- round-robin fashion, each claiming one unclaimed cell per round, so that
-- sizes stay as even as the geometry allows while every region remains
-- connected. Growth continues until no frontier is left, hence every cell
-- reachable from some seed ends up assigned.
assignRegions :: GridMap -> [Cell] -> [RegionAssignment]
assignRegions _ [] = []
assignRegions gm seeds =
  [ RegionAssignment idx seed (sort (M.findWithDefault [] idx grouped))
  | (idx, seed) <- zip [0 ..] seeds
  ]
  where
    labelled = zip [0 ..] seeds
    initialOwner = HM.fromList [(seed, idx) | (idx, seed) <- labelled]
    initialFrontiers = M.fromList [(idx, Seq.fromList (freeNeighbors gm seed)) | (idx, seed) <- labelled]
    owners = grow initialFrontiers initialOwner
    grouped = foldl' (\m (cell, idx) -> M.insertWith (++) idx [cell] m) M.empty (HM.toList owners)

    grow frontiers owner
      | M.null frontiers = owner
      | otherwise =
          let (frontiers', owner') = foldl' claim (M.empty, owner) (M.toList frontiers)
           in grow frontiers' owner'

    -- Pop frontier cells until an unclaimed one is found; claim it and enqueue
    -- its neighbours. Regions whose frontier runs dry are dropped.
    claim (acc, owner) (idx, frontier) = case Seq.viewl frontier of
      Seq.EmptyL -> (acc, owner)
      cell Seq.:< rest
        | HM.member cell owner -> claim (acc, owner) (idx, rest)
        | otherwise ->
            let fresh = filter (not . (`HM.member` owner)) (freeNeighbors gm cell)
             in (M.insert idx (foldl' (Seq.|>) rest fresh) acc, HM.insert cell idx owner)
