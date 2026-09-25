module Coverage.Repair
  ( AssignedDepot (..)
  , BuildResult (..)
  , PlannerEnv
  , prepareDepot
  , buildPath
  , buildPathWith
  , roundTripCost
  ) where

import Control.Monad (foldM)
import Algorithm.AStar (astarPath)
import Algorithm.Flood (DepotField, chainHome, depotField, fieldCost, fieldReturn)
import Domain.Cell (Cell (..), headingFromTo, manhattan)
import Domain.Cost (CostBreakdown (..), addSegmentCost, emptyCost)
import Domain.Depot (Depot (..))
import Domain.Drone (DroneSpec (..))
import Domain.Map (GridMap, freeNeighbors, isFree)
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
  -- ^ Route cells dropped because no round trip from the depot fits the battery.
  }
  deriving (Show)

-- | Planner environment required to convert cell routes into timed trajectories.
data PlannerEnv = PlannerEnv
  { peMap :: !GridMap
  , peDrone :: !DroneSpec
  , peDepotCell :: !Cell
  , peField :: !DepotField
  -- ^ Cheapest routes home, used for every return leg and range check.
  }

data PlannerState = PlannerState
  { psCurrent :: !Cell
  , psHeading :: !(Maybe Int)
  , psTime :: !Double
  , psBattery :: !Double
  , psAirborne :: !Bool
  , psNextIndex :: !Int
  , psWaypointsRev :: ![Waypoint]
  , psSegmentsRev :: ![Segment]
  , psRechargeStops :: ![Int]
  , psCost :: !CostBreakdown
  }

epsilon :: Double
epsilon = 1e-9

-- | Convert a target route into a full tour with timing and recharge stops.
--
-- The drone always keeps enough battery to fly back to the depot along the
-- shortest-path tree and land; when the next move would break that invariant
-- it returns, recharges and resumes from the depot. Cells that cannot be
-- reached and returned from on a single charge are reported in 'brSkipped'.
buildPath :: GridMap -> DroneSpec -> AssignedDepot -> [Cell] -> Either String BuildResult
buildPath gm drone depot route = do
  env <- prepareDepot gm drone depot
  buildPathWith env route

-- | Precompute everything that depends only on the map, the drone and the
-- depot. Building the cheapest-return field is the expensive part of planning,
-- so regions served by the same depot should share one environment.
prepareDepot :: GridMap -> DroneSpec -> AssignedDepot -> Either String PlannerEnv
prepareDepot gm drone (AssignedDepot _ depotCell)
  | not (isFree gm depotCell) = Left ("Depot cell " ++ show depotCell ++ " is blocked")
  | dsSpeed drone <= 0 = Left "Drone speed must be positive"
  | dsTakeoffCost drone + dsLandingCost drone > dsBattery drone + epsilon =
      Left "Battery budget is smaller than takeoff plus landing cost"
  | otherwise =
      Right (PlannerEnv gm drone depotCell (depotField (freeNeighbors gm) (dsSpeed drone) depotCell))

buildPathWith :: PlannerEnv -> [Cell] -> Either String BuildResult
buildPathWith env route = do
      let depotCell = peDepotCell env
          (flyable, skipped) = partitionRange env route
      st <- foldM (moveAlong env) (initialState depotCell) flyable
      final <- finishAtDepot env st
      let cost = psCost final
      pure
        BuildResult
          { brPath = Path (reverse (psWaypointsRev final)) (reverse (psSegmentsRev final))
          , brRechargeStops = reverse (psRechargeStops final)
          , brCost = TourCost (cbTime cost) (cbTurns cost) (cbLength cost)
          , brSkipped = skipped
          }

initialState :: Cell -> PlannerState
initialState depotCell =
  PlannerState
    { psCurrent = depotCell
    , psHeading = Nothing
    , psTime = 0
    , psBattery = 0
    , psAirborne = False
    , psNextIndex = 0
    , psWaypointsRev = [Waypoint 0 depotCell 0]
    , psSegmentsRev = []
    , psRechargeStops = []
    , psCost = emptyCost
    }

partitionRange :: PlannerEnv -> [Cell] -> ([Cell], [Cell])
partitionRange env = foldr classify ([], [])
  where
    budget = dsBattery (peDrone env) + epsilon
    classify cell (ok, bad) = case roundTripCost (peDrone env) (peField env) cell of
      Just cost | cost <= budget -> (cell : ok, bad)
      _ -> (ok, cell : bad)

-- | Battery needed to take off, fly out to a cell along the depot's
-- cheapest-return tree, turn around, fly back and land. Turn costs are
-- symmetric, so the outbound and inbound legs cost the same. 'Nothing' if the
-- cell is not reachable from the depot at all.
roundTripCost :: DroneSpec -> DepotField -> Cell -> Maybe Double
roundTripCost DroneSpec {..} field cell = do
  cost <- fieldCost field cell
  let uTurn = if cost > 0 then 1 else 0
  pure (dsTakeoffCost + 2 * cost + uTurn + dsLandingCost)

-- | Battery needed to get from a cell back to the depot and land, given the
-- heading the drone arrives with.
returnCost :: PlannerEnv -> Cell -> Maybe Int -> Double
returnCost PlannerEnv {peDrone = DroneSpec {..}, peField} cell arrival =
  case fieldReturn peField arrival cell of
    Nothing -> 1 / 0
    Just (cost, _) -> cost + dsLandingCost

turnPenalty :: Maybe Int -> Int -> Int
turnPenalty previous heading = case previous of
  Just prev | prev /= heading -> 1
  _ -> 0

-- | Fly to the next route cell, bridging non-adjacent cells with A*. The bridge
-- only uses cells within round-trip range so that a recharge is always
-- possible from anywhere along it; those cells are connected through the depot.
moveAlong :: PlannerEnv -> PlannerState -> Cell -> Either String PlannerState
moveAlong env st target
  | psCurrent st == target = Right st
  | manhattan (psCurrent st) target == 1 = step env st target
  | otherwise =
      case astarPath (inRangeNeighbors env) (psCurrent st) target of
        Nothing -> Left ("No feasible path between " ++ show (psCurrent st) ++ " and " ++ show target)
        Just path -> foldM (step env) st (tail path)

inRangeNeighbors :: PlannerEnv -> Cell -> [Cell]
inRangeNeighbors env = filter inRange . freeNeighbors (peMap env)
  where
    budget = dsBattery (peDrone env) + epsilon
    inRange c = maybe False (<= budget) (roundTripCost (peDrone env) (peField env) c)

-- | Move one cell, recharging at the depot first when the battery invariant
-- would otherwise be violated.
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
      recharged <- returnToDepot True env airborne
      flyFromDepot env recharged target

-- | Fly from the depot to a cell along the shortest-path tree. Feasible by
-- construction for every cell that passed 'partitionRange'.
flyFromDepot :: PlannerEnv -> PlannerState -> Cell -> Either String PlannerState
flyFromDepot env st target = do
  (_, heading) <- maybe (Left ("Cell " ++ show target ++ " is unreachable from the depot")) Right
    (fieldReturn (peField env) Nothing target)
  chain <- maybe (Left ("No route from the depot to " ++ show target)) Right
    (chainHome (peField env) target heading)
  airborne <- ensureAirborne env st
  foldM (stepNoRecall env) airborne (tail (reverse chain))

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
      }

-- | Fly back along the shortest-path tree and land. When @recordRecharge@ is
-- set the landing is registered as a recharge stop.
returnToDepot :: Bool -> PlannerEnv -> PlannerState -> Either String PlannerState
returnToDepot recordRecharge env st
  | not (psAirborne st) = Right st
  | otherwise = do
      (_, heading) <- maybe (Left ("No return path from " ++ show (psCurrent st))) Right
        (fieldReturn (peField env) (psHeading st) (psCurrent st))
      chain <- maybe (Left ("No route home from " ++ show (psCurrent st))) Right
        (chainHome (peField env) (psCurrent st) heading)
      atDepot <- foldM (stepNoRecall env) st (tail chain)
      land recordRecharge env atDepot

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

finishAtDepot :: PlannerEnv -> PlannerState -> Either String PlannerState
finishAtDepot = returnToDepot False

-- | Ground operations (takeoff, landing) update the time stamp of the waypoint
-- the drone is sitting on.
retime :: Double -> [Waypoint] -> [Waypoint]
retime _ [] = []
retime t (wp : rest) = wp {wpTime = t} : rest
