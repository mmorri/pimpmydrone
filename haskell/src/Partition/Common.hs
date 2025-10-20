module Partition.Common
  ( RegionAssignment (..)
  , chooseSeeds
  , assignRegions
  ) where

import Data.Function (on)
import Data.List (foldl', maximumBy)
import qualified Data.Map.Strict as M
import Domain.Cell (Cell (..), manhattan)
import Domain.Depot (Depot (..))
import Domain.Map (GridMap (..), freeCells, isFree)

-- | Region assigned to a drone, including its anchor seed.
data RegionAssignment = RegionAssignment
  { raDroneId :: !Int
  , raSeed :: !Cell
  , raCells :: ![Cell]
  }
  deriving (Eq, Show)

chooseSeeds :: GridMap -> [Depot] -> Int -> [Cell]
chooseSeeds gm depots n
  | n <= length depotSeeds && n > 0 = take n depotSeeds
  | otherwise = take n (grow depotSeeds)
  where
    depotSeeds = filter (isFree gm) (fmap depotCell depots)
    free = freeCells gm
    grow [] = case free of
      [] -> []
      (s : _) -> grow [s]
    grow seeds
      | length seeds >= n = seeds
      | otherwise =
          let unused = filter (`notElem` seeds) free
              next = case unused of
                [] -> head seeds
                _ -> maximumBy (compare `on` minDist seeds) unused
           in grow (seeds ++ [next])
    minDist seeds cell = minimum (fmap (`manhattan` cell) seeds)

assignRegions :: GridMap -> [Cell] -> [RegionAssignment]
assignRegions gm seeds =
  let assignments = bfs seeds
      grouped0 = M.fromList (zip [0 ..] (fmap (,[]) seeds))
      grouped = foldl' insert grouped0 (M.toList assignments)
   in [ RegionAssignment idx seed cells | (idx, (seed, cells)) <- M.toAscList grouped ]
  where
    neighbors (Cell x y) =
      filter (isFree gm)
        [ Cell (x + 1) y
        , Cell (x - 1) y
        , Cell x (y + 1)
        , Cell x (y - 1)
        ]
    bfs [] = M.empty
    bfs seedList = go initialQueue initialAssign
      where
        initialQueue = [(seed, sid, 0) | (seed, sid) <- zip seedList [0 ..]]
        initialAssign = M.fromList [(seed, (sid, 0)) | (seed, sid) <- zip seedList [0 ..]]
        go [] acc = acc
        go ((cell, region, dist) : rest) acc =
          let (newQueue, newAcc) = foldl' (expand region (dist + 1)) (rest, acc) (neighbors cell)
           in go newQueue newAcc
        expand region dist (queue, acc) ncell =
          case M.lookup ncell acc of
            Nothing -> (queue ++ [(ncell, region, dist)], M.insert ncell (region, dist) acc)
            Just (_, existingDist)
              | dist < existingDist -> (queue ++ [(ncell, region, dist)], M.insert ncell (region, dist) acc)
              | otherwise -> (queue, acc)
    insert m (cell, (region, _dist)) =
      M.adjust
        (\(seed, cells) -> (seed, cell : cells))
        region
        m
