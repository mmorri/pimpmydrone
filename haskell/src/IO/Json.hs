module IO.Json
  ( MapContext (..)
  , readMapContext
  , readParams
  , writeRegions
  , readRegions
  , writeTours
  , encodeRegionsLazy
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecodeFileStrict', encode, encodeFile, withObject, (.=), (.:))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Domain.Cell (Cell (..))
import Domain.Depot (Depot (..))
import Domain.Drone (DroneSpec (..))
import Domain.Map (GridMap, RawMap (..), fromRaw)
import Domain.Tour (Tour)
import Partition.Common (RegionAssignment (..))

-- | Combined map metadata used by the planner.
data MapContext = MapContext
  { mcMap :: !GridMap
  , mcDepots :: ![Depot]
  , mcSeed :: !Int
  }

readMapContext :: FilePath -> IO (Either String MapContext)
readMapContext path = do
  result <- eitherDecodeFileStrict' path
  pure $ case result of
    Left err -> Left err
    Right raw ->
      let gm = fromRaw raw
          depots = zipWith Depot [0 ..] (rawDepots raw)
       in Right MapContext {mcMap = gm, mcDepots = depots, mcSeed = rawSeed raw}

readParams :: FilePath -> IO (Either String DroneSpec)
readParams = eitherDecodeFileStrict'

-- | JSON representation of region assignments.
data RegionDoc = RegionDoc
  { rdDroneId :: !Int
  , rdSeed :: !Cell
  , rdCells :: ![Cell]
  }

instance ToJSON RegionDoc where
  toJSON RegionDoc {..} =
    A.object
      [ "drone_id" .= rdDroneId
      , "seed" .= rdSeed
      , "cells" .= rdCells
      ]

instance FromJSON RegionDoc where
  parseJSON = withObject "Region" $ \o ->
    RegionDoc
      <$> o .: "drone_id"
      <*> o .: "seed"
      <*> o .: "cells"

regionsToDocs :: [RegionAssignment] -> [RegionDoc]
regionsToDocs regions =
  [ RegionDoc
      { rdDroneId = raDroneId ra
      , rdSeed = raSeed ra
      , rdCells = raCells ra
      }
  | ra <- regions
  ]

encodeRegionsLazy :: [RegionAssignment] -> BL.ByteString
encodeRegionsLazy = encode . regionsToDocs

writeRegions :: FilePath -> [RegionAssignment] -> IO ()
writeRegions path regions = encodeFile path (regionsToDocs regions)

readRegions :: FilePath -> IO (Either String [RegionAssignment])
readRegions path = do
  result <- eitherDecodeFileStrict' path
  pure $ case result of
    Left err -> Left err
    Right docs -> Right (fmap toAssignment docs)
  where
    toAssignment doc =
      RegionAssignment
        { raDroneId = rdDroneId doc
        , raSeed = rdSeed doc
        , raCells = rdCells doc
        }

writeTours :: FilePath -> [Tour] -> IO ()
writeTours = encodeFile
