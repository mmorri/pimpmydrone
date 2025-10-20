module Cost.Evaluate
  ( pathCells
  , pathTotalTime
  , pathTotalLength
  , pathTotalTurns
  , validateBattery
  ) where

import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Set as Set
import Domain.Cell (Cell)
import Domain.Drone (DroneSpec (..))
import Domain.Path (Path (..), Segment (..), Waypoint (..))

pathCells :: Path -> [Cell]
pathCells (Path waypoints _) = fmap wpCell (sortBy (compare `on` wpIndex) waypoints)

pathTotalTime :: Path -> Double
pathTotalTime (Path waypoints _) =
  case waypoints of
    [] -> 0
    _ -> maximum (fmap wpTime waypoints)

pathTotalLength :: Path -> Double
pathTotalLength (Path _ segments) = sum (fmap segLength segments)

pathTotalTurns :: Path -> Double
pathTotalTurns (Path _ segments) = sum (fmap segTurnCost segments)

validateBattery :: DroneSpec -> [Int] -> Path -> Bool
validateBattery DroneSpec {..} rechargeStops (Path _ segments) =
  let (accum, ok) = foldl step (dsTakeoffCost, True) segments
      finalOk = ok && accum + dsLandingCost <= dsBattery + 1e-6
   in finalOk
  where
    stopSet = Set.fromList rechargeStops
    step (acc, ok) seg
      | not ok = (acc, ok)
      | otherwise =
          let acc' = acc + segBattery seg
              ok' = acc' <= dsBattery + 1e-6
           in if segTo seg `Set.member` stopSet
                then
                  let landed = acc' + dsLandingCost
                      ok'' = ok' && landed <= dsBattery + 1e-6
                   in (dsTakeoffCost, ok'')
                else (acc', ok')
