module Domain.Path
  ( Waypoint (..)
  , Segment (..)
  , Path (..)
  , pathLength
  , pathTurns
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.=), (.:))
import qualified Data.Aeson as A
import Domain.Cell (Cell (..))

-- | A waypoint along a tour with cumulative time.
data Waypoint = Waypoint
  { wpIndex :: !Int
  , wpCell :: !Cell
  , wpTime :: !Double
  }
  deriving (Eq, Show)

instance FromJSON Waypoint where
  parseJSON = withObject "Waypoint" $ \o ->
    Waypoint
      <$> o .: "index"
      <*> (Cell <$> o .: "x" <*> o .: "y")
      <*> o .: "t"

instance ToJSON Waypoint where
  toJSON Waypoint {..} =
    let Cell x y = wpCell
     in A.object
          [ "index" .= wpIndex
          , "x" .= x
          , "y" .= y
          , "t" .= wpTime
          ]

-- | Directed segment between two waypoints.
data Segment = Segment
  { segFrom :: !Int
  , segTo :: !Int
  , segLength :: !Double
  , segTurnCost :: !Double
  , segBattery :: !Double
  }
  deriving (Eq, Show)

instance FromJSON Segment where
  parseJSON = withObject "Segment" $ \o ->
    Segment
      <$> o .: "from"
      <*> o .: "to"
      <*> o .: "length"
      <*> o .: "turn_cost"
      <*> o .: "battery_used"

instance ToJSON Segment where
  toJSON Segment {..} =
    A.object
      [ "from" .= segFrom
      , "to" .= segTo
      , "length" .= segLength
      , "turn_cost" .= segTurnCost
      , "battery_used" .= segBattery
      ]

-- | Complete path for a single drone.
data Path = Path
  { pathWaypoints :: ![Waypoint]
  , pathSegments :: ![Segment]
  }
  deriving (Eq, Show)

instance FromJSON Path where
  parseJSON = withObject "Path" $ \o ->
    Path <$> o .: "waypoints" <*> o .: "segments"

instance ToJSON Path where
  toJSON Path {..} =
    A.object ["waypoints" .= pathWaypoints, "segments" .= pathSegments]

pathLength :: Path -> Double
pathLength = sum . fmap segLength . pathSegments

pathTurns :: Path -> Double
pathTurns = sum . fmap segTurnCost . pathSegments
