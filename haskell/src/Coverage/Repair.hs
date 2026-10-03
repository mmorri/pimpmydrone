module Coverage.Repair
  ( AssignedDepot (..)
  , BuildResult (..)
  , PlannerEnv
  , prepareDepot
  , prepareDepots
  , plannerEnvWithFields
  , buildPath
  , buildPathWith
  , roundTripCost
  , minRoundTripCost
  ) where

import Control.Monad (foldM)
import Data.Function (on)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.List (foldl', minimumBy)
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as S
import Algorithm.AStar (astarPath)
import Algorithm.Flood (DepotField, chainHome, depotField, fieldCost, fieldReturn)
import Domain.Cell (Cell (..), headingFromTo, manhattan)
import Domain.Cost (CostBreakdown (..), addSegmentCost, emptyCost)
import Domain.Depot (Depot (..))
import Domain.Drone (DroneSpec (..))
import Domain.Map (GridMap (..), freeNeighbors, isFree)
import Domain.Path (Path (..), Segment (..), Waypoint (..))
import Domain.Tour (TourCost (..))

-- | Depot assignment wrapper.
data AssignedDepot = AssignedDepot
  { adDepot :: !Depot
  , adCell :: !Cell
  }
  deriving (Show)

-- | Result of turning a coverage route into a flyable tour.
data BuildResult = BuildResult
  { brPath :: !Path
  , brRechargeStops :: ![Int]
  , brCost :: !TourCost
  , brSkipped :: ![Cell]
  -- ^ Route cells dropped because no round trip from any depot fits the battery.
  }
  deriving (Show)

-- | Planner environment required to convert cell routes into timed trajectories.
--
-- 'peHome' is where the tour starts and ends (the region\'s assigned depot).
-- 'peFields' holds a cheapest-return field for every depot on the map so mid-tour
-- recharges can divert to whichever depot is cheapest to reach from here.
data PlannerEnv = PlannerEnv
  { peMap :: !GridMap
  , peDrone :: !DroneSpec
  , peHome :: !Cell
  , peFields :: ![(Cell, DepotField)]
  }

data PlannerState = PlannerState
  { psCurrent :: !Cell
  , psHeading :: !(Maybe Int)
  , psTime :: !Double
  , psBattery :: !Double
  , psAirborne :: !Bool
  , psActiveDepot :: !Cell
  -- ^ Depot the drone last took off from (or started at).
  , psNextIndex :: !Int
  , psWaypointsRev :: ![Waypoint]
  , psSegmentsRev :: ![Segment]
  , psRechargeStops :: ![Int]
  , psCost :: !CostBreakdown
  , psUncovered :: !(HashSet Cell)
  -- ^ Work cells not yet flown over; used to bias commute routes.
  }

epsilon :: Double
epsilon = 1e-9

-- | Convert a target route into a full tour with timing and recharge stops.
--
-- The drone always keeps enough battery to fly back to the *nearest* depot
-- (cheapest turn-aware return) and land; when the next move would break that
-- invariant it returns there, recharges and resumes. Depot legs prefer still-
-- uncovered work cells so commuting does useful coverage; the reserve itself
-- still uses the unbiased cheapest-return field. The tour starts and finishes
-- at the assigned home depot. Cells that no depot can serve on a single charge
-- are reported in 'brSkipped'.
buildPath :: GridMap -> DroneSpec -> AssignedDepot -> [Cell] -> Either String BuildResult
buildPath gm drone depot route = do
  env <- prepareDepot gm drone depot
  buildPathWith env route

-- | Single-depot convenience: home field only.
prepareDepot :: GridMap -> DroneSpec -> AssignedDepot -> Either String PlannerEnv
prepareDepot gm drone depot = prepareDepots gm drone depot [adCell depot]

-- | Multi-depot environment: home plus every other free depot cell.
prepareDepots :: GridMap -> DroneSpec -> AssignedDepot -> [Cell] -> Either String PlannerEnv
prepareDepots gm drone depot@(AssignedDepot _ home) depotCells =
  let freeDepots = filter (isFree gm) (nubCells (home : depotCells))
      fields =
        [ ( cell
          , depotField
              (gmWidth gm)
              (gmHeight gm)
              (freeNeighbors gm)
              (dsSpeed drone)
              cell
          )
        | cell <- freeDepots
        ]
   in plannerEnvWithFields gm drone depot fields
  where
    nubCells = foldr (\c acc -> if c `elem` acc then acc else c : acc) []

-- | Assemble an env from pre-built depot fields (shared across homes).
plannerEnvWithFields ::
  GridMap -> DroneSpec -> AssignedDepot -> [(Cell, DepotField)] -> Either String PlannerEnv
plannerEnvWithFields gm drone (AssignedDepot _ home) fields
  | not (isFree gm home) = Left ("Depot cell " ++ show home ++ " is blocked")
  | dsSpeed drone <= 0 = Left "Drone speed must be positive"
  | dsTakeoffCost drone + dsLandingCost drone > dsBattery drone + epsilon =
      Left "Battery budget is smaller than takeoff plus landing cost"
  | null fields = Left "No free depot cells"
  | otherwise = Right (PlannerEnv gm drone home fields)

buildPathWith :: PlannerEnv -> [Cell] -> Either String BuildResult
buildPathWith env route = do
  let (flyable, skipped) = partitionRange env route
      work = HS.fromList flyable
  st <- foldM (moveAlong env) (initialState (peHome env) work) flyable
  final <- finishAtHome env st
  let cost = psCost final
  pure
    BuildResult
      { brPath = Path (reverse (psWaypointsRev final)) (reverse (psSegmentsRev final))
      , brRechargeStops = reverse (psRechargeStops final)
      , brCost = TourCost (cbTime cost) (cbTurns cost) (cbLength cost)
      , brSkipped = skipped
      }

initialState :: Cell -> HashSet Cell -> PlannerState
initialState home work =
  PlannerState
    { psCurrent = home
    , psHeading = Nothing
    , psTime = 0
    , psBattery = 0
    , psAirborne = False
    , psActiveDepot = home
    , psNextIndex = 0
    , psWaypointsRev = [Waypoint 0 home 0]
    , psSegmentsRev = []
    , psRechargeStops = []
    , psCost = emptyCost
    , psUncovered = HS.delete home work
    }

partitionRange :: PlannerEnv -> [Cell] -> ([Cell], [Cell])
partitionRange env = foldr classify ([], [])
  where
    budget = dsBattery (peDrone env) + epsilon
    classify cell (ok, bad) = case minRoundTripCost (peDrone env) (peFields env) cell of
      Just cost | cost <= budget -> (cell : ok, bad)
      _ -> (ok, cell : bad)

-- | Battery needed to take off, fly out to a cell along a depot\'s
-- cheapest-return tree, turn around, fly back and land.
roundTripCost :: DroneSpec -> DepotField -> Cell -> Maybe Double
roundTripCost DroneSpec {..} field cell = do
  cost <- fieldCost field cell
  let uTurn = if cost > 0 then 1 else 0
  pure (dsTakeoffCost + 2 * cost + uTurn + dsLandingCost)

-- | Cheapest round trip over any available depot.
minRoundTripCost :: DroneSpec -> [(Cell, DepotField)] -> Cell -> Maybe Double
minRoundTripCost drone fields cell =
  case mapMaybe (\(_, field) -> roundTripCost drone field cell) fields of
    [] -> Nothing
    costs -> Just (minimum costs)

-- | Best depot to return to from a cell (cheapest turn-aware return + landing).
bestReturn :: PlannerEnv -> Cell -> Maybe Int -> Maybe (Cell, DepotField, Double, Int)
bestReturn PlannerEnv {peDrone = DroneSpec {..}, peFields} cell arrival =
  case mapMaybe option peFields of
    [] -> Nothing
    opts -> Just (minimumBy (compare `on` \(_, _, cost, _) -> cost) opts)
  where
    option (depot, field) = do
      (cost, heading) <- fieldReturn field arrival cell
      pure (depot, field, cost + dsLandingCost, heading)

-- | Battery needed to get from a cell back to the nearest depot and land.
returnCost :: PlannerEnv -> Cell -> Maybe Int -> Double
returnCost env cell arrival =
  case bestReturn env cell arrival of
    Nothing -> 1 / 0
    Just (_, _, cost, _) -> cost

turnPenalty :: Maybe Int -> Int -> Int
turnPenalty previous heading = case previous of
  Just prev | prev /= heading -> 1
  _ -> 0

fieldOf :: PlannerEnv -> Cell -> Maybe DepotField
fieldOf env depot = lookup depot (peFields env)

moveAlong :: PlannerEnv -> PlannerState -> Cell -> Either String PlannerState
moveAlong env st target
  | psCurrent st == target = Right st
  -- Already covered during a commute (or earlier hop): do not revisit.
  | not (HS.member target (psUncovered st)) = Right st
  | manhattan (psCurrent st) target == 1 = step env st target
  | otherwise =
      case astarPath (inRangeNeighbors env) (psCurrent st) target of
        Nothing -> Left ("No feasible path between " ++ show (psCurrent st) ++ " and " ++ show target)
        Just path -> foldM (step env) st (tail path)

inRangeNeighbors :: PlannerEnv -> Cell -> [Cell]
inRangeNeighbors env = filter inRange . freeNeighbors (peMap env)
  where
    budget = dsBattery (peDrone env) + epsilon
    inRange c = maybe False (<= budget) (minRoundTripCost (peDrone env) (peFields env) c)

step :: PlannerEnv -> PlannerState -> Cell -> Either String PlannerState
step env st target = do
  airborne <- ensureAirborne env st
  heading <- maybe (Left ("Invalid adjacency " ++ show (psCurrent airborne, target))) Right
    (headingFromTo (psCurrent airborne) target)
  let moveCost = travelCost env + fromIntegral (turnPenalty (psHeading airborne) heading)
      needed = psBattery airborne + moveCost + returnCost env target (Just heading)
  if needed <= dsBattery (peDrone env) + epsilon
    then applyMove env airborne target
    else do
      recharged <- returnToNearestDepot True env airborne
      flyFromActiveDepot env recharged target

-- | Fly from the active (last-landed) depot to a cell, preferring uncovered work.
flyFromActiveDepot :: PlannerEnv -> PlannerState -> Cell -> Either String PlannerState
flyFromActiveDepot env st target = do
  field <- maybe (Left ("Missing field for depot " ++ show (psActiveDepot st))) Right
    (fieldOf env (psActiveDepot st))
  (_, heading) <- maybe (Left ("Cell " ++ show target ++ " is unreachable from the depot")) Right
    (fieldReturn field Nothing target)
  cheap <- maybe (Left ("No route from the depot to " ++ show target)) Right
    (chainHome field target heading)
  airborne <- ensureAirborne env st
  let start = psCurrent airborne
      biased = commutePath env start Nothing target (psUncovered airborne)
      candidates = catMaybes [biased, continuousPath start target (reverse cheap)]
  flyChain env airborne target candidates
    ("No flyable route from depot to " ++ show target)

-- | Try candidate inclusive cell chains with reserve checks; first success wins.
-- Rejects chains that do not actually start at the current cell and finish at the goal.
flyChain :: PlannerEnv -> PlannerState -> Cell -> [[Cell]] -> String -> Either String PlannerState
flyChain _ _ _ [] err = Left err
flyChain env st goal (chain : rest) err =
  case continuousPath (psCurrent st) goal chain of
    Nothing -> flyChain env st goal rest err
    Just (_ : steps) -> case foldM (stepNoRecall env) st steps of
      Right st'
        | psCurrent st' == goal -> Right st'
        | otherwise -> flyChain env st goal rest err
      Left _ -> flyChain env st goal rest err
    Just [] -> flyChain env st goal rest err

-- | Accept only inclusive start→goal paths whose consecutive cells are 4-adjacent.
continuousPath :: Cell -> Cell -> [Cell] -> Maybe [Cell]
continuousPath start goal path = case path of
  [] -> Nothing
  (p : _)
    | p == start && last path == goal && adjacentPairs path -> Just path
    | otherwise -> Nothing
  where
    adjacentPairs cells = and (zipWith (\a b -> manhattan a b == 1) cells (drop 1 cells))
      || start == goal && path == [start]

stepNoRecall :: PlannerEnv -> PlannerState -> Cell -> Either String PlannerState
stepNoRecall env st target = do
  heading <- maybe (Left ("Invalid adjacency " ++ show (psCurrent st, target))) Right
    (headingFromTo (psCurrent st) target)
  let moveCost = travelCost env + fromIntegral (turnPenalty (psHeading st) heading)
      needed = psBattery st + moveCost + returnCost env target (Just heading)
  if needed <= dsBattery (peDrone env) + epsilon
    then applyMove env st target
    else Left ("Battery budget insufficient at " ++ show target)

travelCost :: PlannerEnv -> Double
travelCost env = 1 / dsSpeed (peDrone env)

-- | Extra cost for stepping onto an already-covered (or non-work) cell.
-- Comparable to one turn so short detours through uncovered cells win.
coveredPenalty :: Double
coveredPenalty = 1.0

-- | Heading-aware path from start to goal that prefers still-uncovered cells.
-- Cost here is only for route choice; reserve checks still use 'returnCost'.
commutePath :: PlannerEnv -> Cell -> Maybe Int -> Cell -> HashSet Cell -> Maybe [Cell]
commutePath env start arrival goal uncovered
  | start == goal = Just [start]
  | otherwise =
      let stepCost = travelCost env
          neighbors = freeNeighbors (peMap env)
          startHeading = maybe (-1) id arrival
          source = (start, startHeading)
          initialOpen = S.singleton (0 :: Double, source)
          initialBest = HM.singleton source (0 :: Double, Nothing :: Maybe (Cell, Int))
       in continuousPath start goal
            =<< search neighbors stepCost uncovered goal initialOpen initialBest HM.empty

search
  :: (Cell -> [Cell])
  -> Double
  -> HashSet Cell
  -> Cell
  -> S.Set (Double, (Cell, Int))
  -> HashMap (Cell, Int) (Double, Maybe (Cell, Int))
  -> HashMap (Cell, Int) (Maybe (Cell, Int))
  -> Maybe [Cell]
search neighbors stepCost uncovered goal open best settled
  | S.null open = Nothing
  | otherwise =
      let ((cost, state@(cell, _)), rest) = S.deleteFindMin open
       in if HM.member state settled
            then search neighbors stepCost uncovered goal rest best settled
            else
              let predecessor = snd (HM.lookupDefault (cost, Nothing) state best)
                  settled' = HM.insert state predecessor settled
               in if cell == goal
                    then Just (reconstruct settled' state)
                    else
                      let (open', best') =
                            foldl'
                              (relaxCommute stepCost uncovered cost state)
                              (rest, best)
                              (neighbors cell)
                       in search neighbors stepCost uncovered goal open' best' settled'

relaxCommute
  :: Double
  -> HashSet Cell
  -> Double
  -> (Cell, Int)
  -> (S.Set (Double, (Cell, Int)), HashMap (Cell, Int) (Double, Maybe (Cell, Int)))
  -> Cell
  -> (S.Set (Double, (Cell, Int)), HashMap (Cell, Int) (Double, Maybe (Cell, Int)))
relaxCommute stepCost uncovered cost (cell, heading) (open, best) next =
  case headingFromTo cell next of
    Nothing -> (open, best)
    Just forward ->
      let turn = if heading >= 0 && heading /= forward then (1 :: Int) else 0
          bias = if HS.member next uncovered then 0 else coveredPenalty
          cost' = cost + stepCost + fromIntegral turn + bias
          nextState = (next, forward)
       in if cost' < fst (HM.lookupDefault (1 / 0, Nothing) nextState best)
            then
              ( S.insert (cost', nextState) open
              , HM.insert nextState (cost', Just (cell, heading)) best
              )
            else (open, best)

reconstruct :: HashMap (Cell, Int) (Maybe (Cell, Int)) -> (Cell, Int) -> [Cell]
reconstruct parents = reverse . go []
  where
    go acc state@(cell, _) = case HM.lookupDefault Nothing state parents of
      Nothing -> cell : acc
      Just prev -> go (cell : acc) prev

ensureAirborne :: PlannerEnv -> PlannerState -> Either String PlannerState
ensureAirborne PlannerEnv {peDrone = DroneSpec {..}} st
  | psAirborne st = Right st
  | otherwise =
      let time' = psTime st + dsTakeoffCost
       in Right
            st
              { psAirborne = True
              , psTime = time'
              , psBattery = dsTakeoffCost
              , psHeading = Nothing
              , psWaypointsRev = retime time' (psWaypointsRev st)
              , psCost = addSegmentCost (psCost st) dsTakeoffCost 0 0
              }

applyMove :: PlannerEnv -> PlannerState -> Cell -> Either String PlannerState
applyMove env st target = do
  heading <- maybe (Left "Invalid move") Right (headingFromTo (psCurrent st) target)
  let penalty = turnPenalty (psHeading st) heading
      turnCost = fromIntegral penalty
      travel = travelCost env
      segmentCost = travel + turnCost
      idxFrom = psNextIndex st
      idxTo = idxFrom + 1
      time' = psTime st + segmentCost
      segment =
        Segment
          { segFrom = idxFrom
          , segTo = idxTo
          , segLength = 1
          , segTurnCost = turnCost
          , segBattery = segmentCost
          }
  pure
    st
      { psCurrent = target
      , psHeading = Just heading
      , psTime = time'
      , psBattery = psBattery st + segmentCost
      , psNextIndex = idxTo
      , psWaypointsRev = Waypoint idxTo target time' : psWaypointsRev st
      , psSegmentsRev = segment : psSegmentsRev st
      , psCost = addSegmentCost (psCost st) segmentCost penalty 1
      , psUncovered = HS.delete target (psUncovered st)
      }

-- | Fly back to the cheapest depot and land. Updates 'psActiveDepot'.
returnToNearestDepot :: Bool -> PlannerEnv -> PlannerState -> Either String PlannerState
returnToNearestDepot recordRecharge env st
  | not (psAirborne st) = Right st
  | otherwise = do
      (depot, field, _, heading) <-
        maybe (Left ("No return path from " ++ show (psCurrent st))) Right
          (bestReturn env (psCurrent st) (psHeading st))
      cheap <- maybe (Left ("No route home from " ++ show (psCurrent st))) Right
        (chainHome field (psCurrent st) heading)
      let start = psCurrent st
          biased = commutePath env start (psHeading st) depot (psUncovered st)
          candidates = catMaybes [biased, continuousPath start depot cheap]
      atDepot <-
        flyChain env st depot candidates ("No flyable return to depot from " ++ show start)
      landed <- land recordRecharge env atDepot
      pure landed {psActiveDepot = depot, psCurrent = depot}

-- | End the tour at the assigned home depot (not necessarily the nearest).
finishAtHome :: PlannerEnv -> PlannerState -> Either String PlannerState
finishAtHome env st
  | not (psAirborne st) && psCurrent st == peHome env = Right st
  | not (psAirborne st) = do
      airborne <- ensureAirborne env st {psActiveDepot = psCurrent st}
      goHome env airborne
  | psCurrent st == peHome env = land False env st
  | otherwise = goHome env st

goHome :: PlannerEnv -> PlannerState -> Either String PlannerState
goHome env st = do
  field <- maybe (Left "Missing home field") Right (fieldOf env (peHome env))
  (_, heading) <-
    maybe (Left ("No return path home from " ++ show (psCurrent st))) Right
      (fieldReturn field (psHeading st) (psCurrent st))
  cheap <-
    maybe (Left ("No route to home from " ++ show (psCurrent st))) Right
      (chainHome field (psCurrent st) heading)
  let start = psCurrent st
      home = peHome env
      biased = commutePath env start (psHeading st) home (psUncovered st)
      candidates = catMaybes [biased, continuousPath start home cheap]
  atHome <-
    flyChain env st home candidates ("No flyable route home from " ++ show start)
  land False env atHome

land :: Bool -> PlannerEnv -> PlannerState -> Either String PlannerState
land recordRecharge PlannerEnv {peDrone = DroneSpec {..}} st =
  let time' = psTime st + dsLandingCost
      stops = if recordRecharge then psNextIndex st : psRechargeStops st else psRechargeStops st
   in Right
        st
          { psAirborne = False
          , psTime = time'
          , psBattery = 0
          , psHeading = Nothing
          , psWaypointsRev = retime time' (psWaypointsRev st)
          , psCost = addSegmentCost (psCost st) dsLandingCost 0 0
          , psRechargeStops = stops
          }

retime :: Double -> [Waypoint] -> [Waypoint]
retime _ [] = []
retime t (wp : rest) = wp {wpTime = t} : rest
