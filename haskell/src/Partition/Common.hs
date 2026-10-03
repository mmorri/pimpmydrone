module Partition.Common
  ( RegionAssignment (..)
  , chooseSeeds
  , assignRegions
  ) where

import Algorithm.Flood (bfsDistances, depotField)
import Coverage.Repair (roundTripCost)
import Data.Function (on)
import Data.List (foldl', maximumBy, minimumBy, sort)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import Domain.Cell (Cell (..), manhattan)
import Domain.Depot (Depot (..))
import Domain.Drone (DroneSpec (..))
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

-- | Cost-balanced region growing.
--
-- Regions expand from their seeds by claiming frontier cells one at a time.
-- Each step, the region with the lowest accumulated round-trip commute cost
-- (to its serving depot) claims next, so distant cells count for more than
-- nearby ones and makespan stays fairer than equal cell counts.
assignRegions :: GridMap -> DroneSpec -> [Depot] -> [Cell] -> [RegionAssignment]
assignRegions _ _ _ [] = []
assignRegions gm drone depots seeds =
  [ RegionAssignment idx seed (sort (M.findWithDefault [] idx grouped))
  | (idx, seed) <- zip [0 ..] seeds
  ]
  where
    labelled = zip [0 ..] seeds
    freeDepots = filter (isFree gm . depotCell) depots
    depotIndex = bfsDistances (freeNeighbors gm) (fmap depotCell freeDepots)
    servingDepot seed = case HM.lookup seed depotIndex of
      Nothing -> seed
      Just (label, _) ->
        depotCell $
          minimumBy (comparing depotId) [d | (i, d) <- zip [0 ..] freeDepots, i == label]
    fieldCells =
      HS.toList . HS.fromList $
        if null freeDepots
          then seeds
          else fmap servingDepot seeds
    fields =
      HM.fromList
        [ ( cell
          , depotField (gmWidth gm) (gmHeight gm) (freeNeighbors gm) (dsSpeed drone) cell
          )
        | cell <- fieldCells
        ]
    weight seed cell =
      case HM.lookup (servingDepot seed) fields of
        Nothing -> 1
        Just field -> fromMaybe 1 (roundTripCost drone field cell)
    seedOf = M.fromList labelled
    initialOwner = HM.fromList [(seed, idx) | (idx, seed) <- labelled]
    initialFrontiers = M.fromList [(idx, Seq.fromList (freeNeighbors gm seed)) | (idx, seed) <- labelled]
    initialTotals =
      M.fromList
        [ (idx, weight seed seed)
        | (idx, seed) <- labelled
        ]
    owners = grow initialFrontiers initialOwner initialTotals
    grouped = foldl' (\m (cell, idx) -> M.insertWith (++) idx [cell] m) M.empty (HM.toList owners)

    grow frontiers owner totals
      | M.null frontiers = owner
      | otherwise =
          let idx =
                fst $
                  minimumBy
                    (comparing snd <> comparing fst)
                    [ (i, M.findWithDefault 0 i totals)
                    | i <- M.keys frontiers
                    ]
           in case claim frontiers owner idx of
                Nothing -> grow (M.delete idx frontiers) owner totals
                Just (frontiers', owner', cell) ->
                  let seed = seedOf M.! idx
                      totals' = M.insertWith (+) idx (weight seed cell) totals
                   in grow frontiers' owner' totals'

    -- Pop frontier cells until an unclaimed one is found; claim it and enqueue
    -- its neighbours. 'Nothing' means this region\'s frontier is empty.
    claim frontiers owner idx = case M.lookup idx frontiers of
      Nothing -> Nothing
      Just frontier -> go frontier
      where
        go frontier = case Seq.viewl frontier of
          Seq.EmptyL -> Nothing
          cell Seq.:< rest
            | HM.member cell owner -> go rest
            | otherwise ->
                let fresh = filter (not . (`HM.member` owner)) (freeNeighbors gm cell)
                    frontier' = foldl' (Seq.|>) rest fresh
                 in Just (M.insert idx frontier' frontiers, HM.insert cell idx owner, cell)
