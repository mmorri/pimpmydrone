module Coverage.Repair
  ( AssignedDepot (..)
  , BuildResult (..)
  , buildPath
  , roundTripCost
  ) where

import Control.Monad (foldM)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Algorithm.AStar (astarPath)
import Algorithm.Flood (FieldEntry (..), chainToDepot, depotField)
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
  , peField :: !(HashMap Cell FieldEntry)
  -- ^ Shortest-path tree rooted at the depot, used for every return leg.
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
buildPath gm drone (AssignedDepot _ depotCell) route
  | not (isFree gm depotCell) = Left ("Depot cell " ++ show depotCell ++ " is blocked")
  | dsSpeed drone <= 0 = Left "Drone speed must be positive"
  | dsTakeoffCost drone + dsLandingCost drone > dsBattery drone + epsilon =
      Left "Battery budget is smaller than takeoff plus landing cost"
  | otherwise = do
      let env = PlannerEnv gm drone (depotField (freeNeighbors gm) depotCell)
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
-- shortest-path tree, turn around, fly back and land. 'Nothing' if the cell
-- is not reachable from the depot at all.
roundTripCost :: DroneSpec -> HashMap Cell FieldEntry -> Cell -> Maybe Double
roundTripCost DroneSpec {..} field cell = do
  FieldEntry {..} <- HM.lookup cell field
  let oneWay = fromIntegral feDistance / dsSpeed + fromIntegral feTurns
      uTurn = if feDistance > 0 then 1 else 0
  pure (dsTakeoffCost + 2 * oneWay + uTurn + dsLandingCost)

-- | Battery needed to get from a cell back to the depot and land, given the
-- heading the drone arrives with.
returnCost :: PlannerEnv -> Cell -> Maybe Int -> Double
returnCost PlannerEnv {peDrone = DroneSpec {..}, peField} cell arrival =
  case HM.lookup cell peField of
    Nothing -> 1 / 0
    Just FieldEntry {..} ->
      let firstTurn = case (arrival, feHeading) of
            (Just a, Just h) | a /= h -> 1
            _ -> 0
       in fromIntegral feDistance / dsSpeed + fromIntegral feTurns + firstTurn + dsLandingCost

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
  chain <- maybe (Left ("Cell " ++ show target ++ " is unreachable from the depot")) Right
    (chainToDepot (peField env) target)
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
      chain <- maybe (Left ("No return path from " ++ show (psCurrent st))) Right
        (chainToDepot (peField env) (psCurrent st))
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
