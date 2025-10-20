module Domain.Tour
  ( Tour (..)
  , TourCost (..)
  , mkTour
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.=), (.:))
import qualified Data.Aeson as A
import Domain.Path (Path (..), Segment, Waypoint)

-- | Per-drone tour with identifiers and aggregated metrics.
data Tour = Tour
  { tourDroneId :: !Int
  , tourWaypoints :: ![Waypoint]
  , tourSegments :: ![Segment]
  , tourRechargeStops :: ![Int]
  , tourCost :: !TourCost
  }
  deriving (Eq, Show)

instance FromJSON Tour where
  parseJSON = withObject "Tour" $ \o ->
    Tour
      <$> o .: "drone_id"
      <*> o .: "waypoints"
      <*> o .: "segments"
      <*> o .: "recharge_stops"
      <*> o .: "cost"

instance ToJSON Tour where
  toJSON Tour {..} =
    A.object
      [ "drone_id" .= tourDroneId
      , "waypoints" .= tourWaypoints
      , "segments" .= tourSegments
      , "recharge_stops" .= tourRechargeStops
      , "cost" .= tourCost
      ]

-- | Reported cost metrics for interoperability.
data TourCost = TourCost
  { tcTime :: !Double
  , tcTurns :: !Int
  , tcLength :: !Double
  }
  deriving (Eq, Show)

instance FromJSON TourCost where
  parseJSON = withObject "TourCost" $ \o ->
    TourCost
      <$> o .: "time"
      <*> o .: "turns"
      <*> o .: "length"

instance ToJSON TourCost where
  toJSON TourCost {..} =
    A.object
      [ "time" .= tcTime
      , "turns" .= tcTurns
      , "length" .= tcLength
      ]

mkTour :: Int -> Path -> [Int] -> TourCost -> Tour
mkTour droneId (Path waypoints segments) rechargeStops cost =
  Tour droneId waypoints segments rechargeStops cost
