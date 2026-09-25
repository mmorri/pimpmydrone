module Algorithm.Flood
  ( DepotField
  , bfsDistances
  , reachableFrom
  , depotField
  , fieldCost
  , fieldReturn
  , chainHome
  , fieldCells
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
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

-- | Cheapest routes home to a depot.
--
-- A breadth-first tree minimises the number of steps home, but the drone also
-- pays one time unit per heading change, so the shortest route is often not
-- the cheapest one -- and a planner that sizes its battery reserve from it
-- will refuse cells it could actually serve. This is therefore a Dijkstra
-- search whose states are @(cell, heading)@ pairs, the heading being the
-- direction of the first step home.
--
-- Keeping the heading in the state matters for more than accuracy: the cost
-- recorded for a state and the route reconstructed from it always describe the
-- same flight, so the battery reserved for the trip home is never less than
-- the trip actually costs.
data DepotField = DepotField
  { dfStates :: !(HashMap State StateEntry)
  , dfBest :: !(HashMap Cell (Double, Int))
  -- ^ Cheapest cost home per cell, with the heading that achieves it.
  }

-- | A cell together with the heading of the first step home. The depot itself
-- is the only state with no heading, represented by 'depotHeading'.
type State = (Cell, Int)

data StateEntry = StateEntry
  { seCost :: !Double
  , sePrev :: !(Maybe State)
  }

depotHeading :: Int
depotHeading = -1

depotField :: (Cell -> [Cell]) -> Double -> Cell -> DepotField
depotField neighbors speed depot = build initialQueue initialBest HM.empty
  where
    step = 1 / speed
    source = (depot, depotHeading)
    initialQueue = S.singleton (0, source)
    initialBest = HM.singleton source (0, Nothing)

    -- @best@ holds the cheapest route found so far for a state, @settled@ the
    -- states whose cost is final. A state is expanded the first time it is
    -- popped and ignored afterwards.
    build open best settled
      | S.null open = DepotField settled (summarise settled)
      | otherwise =
          let ((cost, state), rest) = S.deleteFindMin open
           in if HM.member state settled
                then build rest best settled
                else
                  let predecessor = snd (HM.lookupDefault (cost, Nothing) state best)
                      settled' = HM.insert state (StateEntry cost predecessor) settled
                      (open', best') = foldl' (relax cost state) (rest, best) (neighbors (fst state))
                   in build open' best' settled'

    -- Arriving at @cell@ from @next@ means the drone flies home from @next@ in
    -- the opposite direction, then turns if the onward heading differs.
    relax cost state@(cell, heading) (open, best) next =
      case headingFromTo next cell of
        Nothing -> (open, best)
        Just back ->
          let turn = if heading /= depotHeading && heading /= back then 1 else 0
              cost' = cost + step + turn
              nextState = (next, back)
           in if cost' < fst (HM.lookupDefault (1 / 0, Nothing) nextState best)
                then
                  ( S.insert (cost', nextState) open
                  , HM.insert nextState (cost', Just state) best
                  )
                else (open, best)

    summarise settled =
      HM.fromListWith
        min
        [ (cell, (seCost entry, heading))
        | ((cell, heading), entry) <- HM.toList settled
        ]

-- | Cheapest cost of flying from a cell to the depot, over all headings.
-- Turn costs are symmetric, so this is also the cost of flying out to it.
fieldCost :: DepotField -> Cell -> Maybe Double
fieldCost field cell = fst <$> HM.lookup cell (dfBest field)

-- | Cost of flying home from a cell given the heading the drone arrives with,
-- together with the heading it should leave on. Turning on the spot to pick a
-- cheaper route home is allowed and costs one time unit. At the depot itself
-- the cost is zero whatever the heading, since there is nothing to fly.
fieldReturn :: DepotField -> Maybe Int -> Cell -> Maybe (Double, Int)
fieldReturn field arrival cell =
  case mapMaybe cost (depotHeading : [0 .. 3]) of
    [] -> Nothing
    options -> Just (minimum options)
  where
    cost heading = do
      entry <- HM.lookup (cell, heading) (dfStates field)
      let turn = case arrival of
            Just a | heading /= depotHeading && a /= heading -> 1
            _ -> 0
      pure (seCost entry + turn, heading)

-- | The cells from a cell back to the depot, inclusive of both ends, along the
-- route whose cost 'fieldReturn' reports for that heading.
chainHome :: DepotField -> Cell -> Int -> Maybe [Cell]
chainHome field cell heading = go (cell, heading) []
  where
    go state@(c, _) acc = case HM.lookup state (dfStates field) of
      Nothing -> Nothing
      Just entry -> case sePrev entry of
        Nothing -> Just (reverse (c : acc))
        Just prev -> go prev (c : acc)

-- | Every cell the depot can reach.
fieldCells :: DepotField -> [Cell]
fieldCells = HM.keys . dfBest
