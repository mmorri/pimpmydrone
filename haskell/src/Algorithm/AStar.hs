{-# LANGUAGE BangPatterns #-}

module Algorithm.AStar
  ( astar
  , astarPath
  ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import Domain.Cell (Cell (..), manhattan)

-- | Priority queue entry. The ordering key is (f, g, cell) so that two
-- distinct cells with the same f-score are never conflated by 'S.Set'.
data Node = Node
  { nFScore :: !Int
  , nGScore :: !Int
  , nCell :: !Cell
  }
  deriving (Eq, Ord, Show)

-- | A* over a unit-cost grid. Returns the cell sequence from start to goal
-- (inclusive) or 'Nothing' when the goal is unreachable.
astar
  :: (Cell -> [Cell])
  -- ^ Neighbour function (must only return traversable cells)
  -> (Cell -> Cell -> Int)
  -- ^ Admissible heuristic, e.g. 'manhattan'
  -> Cell
  -> Cell
  -> Maybe [Cell]
astar getNeighbors heuristic start goal
  | start == goal = Just [start]
  | otherwise = search (S.singleton (Node (heuristic start goal) 0 start)) (HM.singleton start 0) HM.empty
  where
    search !open !gScore !cameFrom
      | S.null open = Nothing
      | current == goal = Just (reconstruct cameFrom current [])
      -- Stale entry: a cheaper path to this cell was already expanded.
      | currentG > HM.lookupDefault maxBound current gScore = search openRest gScore cameFrom
      | otherwise =
          let (open', gScore', cameFrom') =
                foldr (relax current currentG) (openRest, gScore, cameFrom) (getNeighbors current)
           in search open' gScore' cameFrom'
      where
        (Node _ currentG current, openRest) = S.deleteFindMin open

    relax current currentG neighbor (!openAcc, !gAcc, !cameAcc) =
      let tentativeG = currentG + 1
       in if tentativeG < HM.lookupDefault maxBound neighbor gAcc
            then
              ( S.insert (Node (tentativeG + heuristic neighbor goal) tentativeG neighbor) openAcc
              , HM.insert neighbor tentativeG gAcc
              , HM.insert neighbor current cameAcc
              )
            else (openAcc, gAcc, cameAcc)

    reconstruct cameFrom cell acc =
      case HM.lookup cell cameFrom of
        Nothing -> cell : acc
        Just prev -> reconstruct cameFrom prev (cell : acc)

-- | A* with the Manhattan heuristic.
astarPath :: (Cell -> [Cell]) -> Cell -> Cell -> Maybe [Cell]
astarPath getNeighbors = astar getNeighbors manhattan
