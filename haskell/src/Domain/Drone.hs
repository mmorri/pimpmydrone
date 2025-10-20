module Domain.Drone
  ( DroneSpec (..)
  , Sortie (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.=), (.:))
import qualified Data.Aeson as A

-- | Drone capabilities shared by the fleet.
data DroneSpec = DroneSpec
  { dsSpeed :: !Double -- ^ Cells per second.
  , dsBattery :: !Double -- ^ Battery budget in seconds per sortie.
  , dsTakeoffCost :: !Double
  , dsLandingCost :: !Double
  , dsMaxDrones :: !Int
  }
  deriving (Eq, Show)

instance FromJSON DroneSpec where
  parseJSON = withObject "DroneSpec" $ \o ->
    DroneSpec
      <$> o .: "v"
      <*> o .: "B"
      <*> o .: "takeoff_cost"
      <*> o .: "landing_cost"
      <*> o .: "max_drones"

instance ToJSON DroneSpec where
  toJSON DroneSpec {..} =
    A.object
      [ "v" .= dsSpeed
      , "B" .= dsBattery
      , "takeoff_cost" .= dsTakeoffCost
      , "landing_cost" .= dsLandingCost
      , "max_drones" .= dsMaxDrones
      ]

-- | A sortie between two depot visits.
data Sortie = Sortie
  { sortieStart :: !Int
  , sortieEnd :: !Int
  , sortieBatteryUse :: !Double
  }
  deriving (Eq, Show)

instance FromJSON Sortie where
  parseJSON = withObject "Sortie" $ \o ->
    Sortie
      <$> o .: "start"
      <*> o .: "end"
      <*> o .: "battery"

instance ToJSON Sortie where
  toJSON Sortie {..} =
    A.object
      [ "start" .= sortieStart
      , "end" .= sortieEnd
      , "battery" .= sortieBatteryUse
      ]
