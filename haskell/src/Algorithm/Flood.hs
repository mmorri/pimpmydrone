module Algorithm.Flood
  ( FieldEntry (..)
  , bfsDistances
  , reachableFrom
  , depotField
  , chainToDepot
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.List (foldl')
import Data.Sequence (ViewL (..), viewl, (|>))
import qualified Data.Sequence as Seq
import Domain.Cell (Cell, headingFromTo)

-- | Multi-source breadth-first distances. Each source is labelled with the
-- index of the source that reached it first; ties are broken by source order.
bfsDistances :: (Cell -> [Cell]) -> [Cell] -> HashMap Cell (Int, Int)
bfsDistances neighbors sources = go (Seq.fromList seeds) (HM.fromList seeds)
  where
    seeds = [(cell, (label, 0)) | (label, cell) <- zip [0 ..] sources]
    go queue acc = case viewl queue of
      EmptyL -> acc
      (cell, (label, dist)) :< rest ->
        let (queue', acc') = foldl' (visit label (dist + 1)) (rest, acc) (neighbors cell)
         in go queue' acc'
    visit label dist (queue, acc) next
      | HM.member next acc = (queue, acc)
      | otherwise = (queue |> (next, (label, dist)), HM.insert next (label, dist) acc)

-- | Every cell reachable from any of the sources.
reachableFrom :: (Cell -> [Cell]) -> [Cell] -> HashSet Cell
reachableFrom neighbors = HS.fromMap . HM.map (const ()) . bfsDistances neighbors

-- | Shortest-path tree rooted at a depot. For every reachable cell it records
-- the grid distance to the depot, the next cell on the way back, the heading
-- of that first step and the number of heading changes along the whole chain.
data FieldEntry = FieldEntry
  { feDistance :: !Int
  , feParent :: !(Maybe Cell)
  , feHeading :: !(Maybe Int)
  , feTurns :: !Int
  }
  deriving (Eq, Show)

depotField :: (Cell -> [Cell]) -> Cell -> HashMap Cell FieldEntry
depotField neighbors depot = go (Seq.singleton depot) (HM.singleton depot root)
  where
    root = FieldEntry 0 Nothing Nothing 0
    go queue acc = case viewl queue of
      EmptyL -> acc
      cell :< rest ->
        let entry = acc HM.! cell
            (queue', acc') = foldl' (visit cell entry) (rest, acc) (neighbors cell)
         in go queue' acc'
    visit parent parentEntry (queue, acc) next
      | HM.member next acc = (queue, acc)
      | otherwise =
          let heading = headingFromTo next parent
              turns = case (heading, feHeading parentEntry) of
                (Just h, Just ph) | h /= ph -> feTurns parentEntry + 1
                _ -> feTurns parentEntry
              entry = FieldEntry (feDistance parentEntry + 1) (Just parent) heading turns
           in (queue |> next, HM.insert next entry acc)

-- | The cells from a start cell back to the depot (inclusive of both ends),
-- following the shortest-path tree. 'Nothing' if the cell is not in the field.
chainToDepot :: HashMap Cell FieldEntry -> Cell -> Maybe [Cell]
chainToDepot field = go []
  where
    go acc cell = case HM.lookup cell field of
      Nothing -> Nothing
      Just entry -> case feParent entry of
        Nothing -> Just (reverse (cell : acc))
        Just parent -> go (cell : acc) parent
