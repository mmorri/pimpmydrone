{-# LANGUAGE BangPatterns #-}
module Algorithm.AStar
  ( astar
  , astarPath
  ) where

import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import Domain.Cell (Cell(..), manhattan)

-- | Priority queue entry for A*
data Node = Node
  { nCell :: !Cell
  , nGScore :: !Int
  , nFScore :: !Int
  } deriving (Eq, Show)

instance Ord Node where
  compare (Node _ _ f1) (Node _ _ f2) = compare f1 f2

-- | A* pathfinding algorithm
-- Returns path from start to goal, or Nothing if no path exists
astar :: (Cell -> [Cell])        -- ^ Neighbor function
      -> (Cell -> Cell -> Int)    -- ^ Heuristic function (e.g., manhattan)
      -> Cell                     -- ^ Start
      -> Cell                     -- ^ Goal
      -> Maybe [Cell]
astar getNeighbors heuristic start goal
  | start == goal = Just [start]
  | otherwise = search initOpen initGScore initCameFrom
  where
    initOpen = S.singleton (Node start 0 (heuristic start goal))
    initGScore = HM.singleton start 0
    initCameFrom = HM.empty

    search !open !gScore !cameFrom
      | S.null open = Nothing
      | current == goal = Just (reconstructPath cameFrom current)
      | otherwise = search open' gScore' cameFrom'
      where
        (Node current currentG _, openRest) = S.deleteFindMin open
        neighbors = getNeighbors current

        (open', gScore', cameFrom') =
          foldr (processNeighbor current currentG)
                (openRest, gScore, cameFrom)
                neighbors

    processNeighbor current currentG neighbor (openAcc, gScoreAcc, cameFromAcc) =
      let tentativeG = currentG + 1  -- Cost between adjacent cells is 1
          neighborG = HM.lookupDefault maxBound neighbor gScoreAcc
      in if tentativeG < neighborG
         then let f = tentativeG + heuristic neighbor goal
                  newNode = Node neighbor tentativeG f
                  openAcc' = S.insert newNode openAcc
                  gScoreAcc' = HM.insert neighbor tentativeG gScoreAcc
                  cameFromAcc' = HM.insert neighbor current cameFromAcc
              in (openAcc', gScoreAcc', cameFromAcc')
         else (openAcc, gScoreAcc, cameFromAcc)

    reconstructPath cameFrom current =
      case HM.lookup current cameFrom of
        Nothing -> [current]
        Just prev -> reconstructPath cameFrom prev ++ [current]

-- | Convenience function for pathfinding with manhattan distance heuristic
astarPath :: (Cell -> [Cell]) -> Cell -> Cell -> Maybe [Cell]
astarPath getNeighbors = astar getNeighbors manhattan