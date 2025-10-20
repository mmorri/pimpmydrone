module Coverage.Repair
  ( buildPath
  , AssignedDepot (..)
  ) where

import Control.Monad (foldM)
import qualified Data.HashMap.Strict as HM
import Algorithm.AStar (astarPath)
import Domain.Cell (Cell (..), headingFromTo, neighbors4, manhattan)
import Domain.Depot (Depot (..))
import Domain.Drone (DroneSpec (..))
import Domain.Map (GridMap, isFree)
import Domain.Path (Path (..), Segment (..), Waypoint (..))
import Domain.Tour (TourCost (..))
import Domain.Cost (CostBreakdown (..), addSegmentCost, emptyCost)
import Domain.Geometry (bresenham, lineOfSight)

-- | Depot assignment wrapper.
data AssignedDepot = AssignedDepot
  { adDepot :: !Depot
  , adCell :: !Cell
  }
  deriving (Show)

-- | Planner environment required to convert cell routes into timed trajectories.
data PlannerEnv = PlannerEnv
  { peMap :: !GridMap
  , peDrone :: !DroneSpec
  , peDepot :: !AssignedDepot
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

-- | Convert a target route into a full tour with timing and recharge stops.
buildPath :: GridMap -> DroneSpec -> AssignedDepot -> [Cell] -> Either String (Path, [Int], TourCost)
buildPath gm drone depot route = buildWithEnv env route
  where
    env = PlannerEnv gm drone depot

buildWithEnv :: PlannerEnv -> [Cell] -> Either String (Path, [Int], TourCost)
buildWithEnv env targetRoute = do
  fullRoute <- augmentRoute env targetRoute
  folded <- foldM (applyStep env) initialState (tail fullRoute)
  finalState <- landIfNeeded env folded
  let waypoints = reverse (psWaypointsRev finalState)
      segments = reverse (psSegmentsRev finalState)
      cost = psCost finalState
      tourCost = TourCost (cbTime cost) (cbTurns cost) (cbLength cost)
  pure (Path waypoints segments, reverse (psRechargeStops finalState), tourCost)
  where
    depotCell = adCell (peDepot env)
    initialState =
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

augmentRoute :: PlannerEnv -> [Cell] -> Either String [Cell]
augmentRoute PlannerEnv {..} baseRoute
  | null baseRoute = Right [startCell]
  | otherwise = do
      startPath <- shortestPathCells peMap startCell (head baseRoute)
      endPath <- shortestPathCells peMap (last baseRoute) startCell
      pure $ concatPaths [startPath, drop 1 baseRoute, drop 1 endPath]
  where
    startCell = adCell peDepot

applyStep :: PlannerEnv -> PlannerState -> Cell -> Either String PlannerState
applyStep env st target = do
  airborneState <- ensureAirborne env st
  moveAlong env airborneState target

ensureAirborne :: PlannerEnv -> PlannerState -> Either String PlannerState
ensureAirborne PlannerEnv {peDrone = DroneSpec {..}} st
  | psAirborne st = Right st
  | otherwise =
      let time' = psTime st + dsTakeoffCost
          battery' = dsTakeoffCost
          waypoints' =
            case psWaypointsRev st of
              [] -> [Waypoint 0 (psCurrent st) time']
              (wp : rest) -> Waypoint (wpIndex wp) (wpCell wp) time' : rest
          cost' = addSegmentCost (psCost st) dsTakeoffCost 0 0
       in Right
            st
              { psAirborne = True
              , psTime = time'
              , psBattery = battery'
              , psWaypointsRev = waypoints'
              , psCost = cost'
              }

moveAlong :: PlannerEnv -> PlannerState -> Cell -> Either String PlannerState
moveAlong env@PlannerEnv {peMap} st target
  | psCurrent st == target = Right st
  | otherwise = do
      path <- shortestPathCells peMap (psCurrent st) target
      foldM (execute env True) st (tail path)

execute :: PlannerEnv -> Bool -> PlannerState -> Cell -> Either String PlannerState
execute env allowRecall st target = do
  ready <- ensureBattery env allowRecall st target
  applyMove env ready target

ensureBattery :: PlannerEnv -> Bool -> PlannerState -> Cell -> Either String PlannerState
ensureBattery env@PlannerEnv {peDrone = DroneSpec {..}} allowRecall st target = do
  heading <- maybe (Left "Invalid adjacency") Right (headingFromTo (psCurrent st) target)
  let turnPenalty = case psHeading st of
        Nothing -> 0
        Just prev -> if prev == heading then 0 else 1
      travelCost = 1.0 / dsSpeed
      moveCost = travelCost + fromIntegral turnPenalty
      projected = psBattery st + moveCost + dsLandingCost
  if projected <= dsBattery + 1e-9
    then Right st
    else
      if allowRecall
        then do
          returned <- returnToDepot True env st
          airborne <- ensureAirborne env returned
          ensureBattery env allowRecall airborne target
        else Left "Battery budget insufficient while returning to depot"

applyMove :: PlannerEnv -> PlannerState -> Cell -> Either String PlannerState
applyMove PlannerEnv {peDrone = DroneSpec {..}} st target = do
  heading <- maybe (Left "Invalid move") Right (headingFromTo (psCurrent st) target)
  let turnPenalty = case psHeading st of
        Nothing -> 0
        Just prev -> if prev == heading then 0 else 1
      turnCost = fromIntegral turnPenalty
      travelCost = 1.0 / dsSpeed
      time' = psTime st + travelCost + turnCost
      battery' = psBattery st + travelCost + turnCost
      idxFrom = psNextIndex st
      idxTo = idxFrom + 1
      waypoint = Waypoint idxTo target time'
      segment =
        Segment
          { segFrom = idxFrom
          , segTo = idxTo
          , segLength = 1
          , segTurnCost = turnCost
          , segBattery = travelCost + turnCost
          }
      cost' = addSegmentCost (psCost st) (travelCost + turnCost) turnPenalty 1
   in Right
        st
          { psCurrent = target
          , psHeading = Just heading
          , psTime = time'
          , psBattery = battery'
          , psNextIndex = idxTo
          , psWaypointsRev = waypoint : psWaypointsRev st
          , psSegmentsRev = segment : psSegmentsRev st
          , psCost = cost'
          }

returnToDepot :: Bool -> PlannerEnv -> PlannerState -> Either String PlannerState
returnToDepot recordRecharge env@PlannerEnv {peMap, peDepot} st
  | not (psAirborne st) = Right st
  | psCurrent st == adCell peDepot = landVariant recordRecharge env st
  | otherwise = do
      path <- shortestPathCells peMap (psCurrent st) (adCell peDepot)
      travelled <- foldM (execute env False) st (tail path)
      returnToDepot recordRecharge env travelled

landVariant :: Bool -> PlannerEnv -> PlannerState -> Either String PlannerState
landVariant recordRecharge PlannerEnv {peDrone = DroneSpec {..}} st =
  case psWaypointsRev st of
    [] -> Left "Missing waypoint for landing"
    (wp : rest) ->
      let time' = psTime st + dsLandingCost
          cost' = addSegmentCost (psCost st) dsLandingCost 0 0
          recharge = if recordRecharge then psNextIndex st : psRechargeStops st else psRechargeStops st
       in Right
            st
              { psAirborne = False
              , psTime = time'
              , psBattery = 0
              , psHeading = Nothing
              , psWaypointsRev = Waypoint (wpIndex wp) (wpCell wp) time' : rest
              , psCost = cost'
              , psRechargeStops = recharge
              }

landIfNeeded :: PlannerEnv -> PlannerState -> Either String PlannerState
landIfNeeded env@PlannerEnv {peDrone = DroneSpec {..}, peDepot} st
  | not (psAirborne st) = Right st
  | psCurrent st == adCell peDepot =
      case psWaypointsRev st of
        [] -> Left "Missing waypoint when landing"
        (wp : rest) ->
          let time' = psTime st + dsLandingCost
              cost' = addSegmentCost (psCost st) dsLandingCost 0 0
           in Right
                st
                  { psAirborne = False
                  , psTime = time'
                  , psBattery = 0
                  , psHeading = Nothing
                  , psWaypointsRev = Waypoint (wpIndex wp) (wpCell wp) time' : rest
                  , psCost = cost'
                  }
  | otherwise = returnToDepot False env st

concatPaths :: [[Cell]] -> [Cell]
concatPaths [] = []
concatPaths (p : ps) = foldl merge p ps
  where
    merge acc path
      | null path = acc
      | null acc = path
      | last acc == head path = acc ++ tail path
      | otherwise = acc ++ path

shortestPathCells :: GridMap -> Cell -> Cell -> Either String [Cell]
shortestPathCells gm start end
  | start == end = Right [start]
  | otherwise =
      case astarPath (\c -> filter (isFree gm) (neighbors4 c)) start end of
        Nothing -> Left "No feasible path between points"
        Just path -> Right path
