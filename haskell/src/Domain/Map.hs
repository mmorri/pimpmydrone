module Domain.Map
  ( GridMap (..)
  , RawMap (..)
  , fromRaw
  , isInside
  , isBlocked
  , isFree
  , freeCells
  , allCells
  ) where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Domain.Cell (Cell (..))
import Domain.NoFly (NoFlyZone (..), Vertex (..), pointInPolygon)
import GHC.Generics (Generic)

-- | Grid map with blocked cells for obstacles and no-fly zones discretized to cells.
data GridMap = GridMap
  { gmWidth :: !Int
  , gmHeight :: !Int
  , gmSeed :: !Int
  , gmObstacles :: !(HashSet Cell)
  , gmNoFly :: !(HashSet Cell)
  }
  deriving (Eq, Show)

-- | Raw map structure matching JSON input.
data RawMap = RawMap
  { rawWidth :: !Int
  , rawHeight :: !Int
  , rawSeed :: !Int
  , rawObstacles :: ![Cell]
  , rawNoFlyZones :: ![NoFlyZone]
  , rawDepots :: ![Cell]
  }
  deriving (Eq, Show, Generic)

instance FromJSON RawMap where
  parseJSON = withObject "Map" $ \o ->
    RawMap
      <$> o .: "width"
      <*> o .: "height"
      <*> o .: "seed"
      <*> o .: "obstacles"
      <*> o .: "no_fly"
      <*> o .: "depots"

fromRaw :: RawMap -> GridMap
fromRaw RawMap {..} =
  let obstacleSet = HS.fromList rawObstacles
      noFlyPolys = fmap nfVertices rawNoFlyZones
      noFlySet = rasterizeNoFly rawWidth rawHeight noFlyPolys
   in GridMap rawWidth rawHeight rawSeed obstacleSet noFlySet

rasterizeNoFly :: Int -> Int -> [[Vertex]] -> HashSet Cell
rasterizeNoFly w h polys = HS.fromList $ do
  x <- [0 .. w - 1]
  y <- [0 .. h - 1]
  let center = (fromIntegral x + 0.5, fromIntegral y + 0.5)
  if any (pointInPolygon center) polys
    then pure (Cell x y)
    else []

isInside :: GridMap -> Cell -> Bool
isInside GridMap {gmWidth = w, gmHeight = h} (Cell x y) = x >= 0 && y >= 0 && x < w && y < h

isBlocked :: GridMap -> Cell -> Bool
isBlocked GridMap {..} cell = cell `HS.member` gmObstacles || cell `HS.member` gmNoFly

isFree :: GridMap -> Cell -> Bool
isFree gm cell = isInside gm cell && not (isBlocked gm cell)

allCells :: GridMap -> [Cell]
allCells GridMap {gmWidth = w, gmHeight = h} = [Cell x y | x <- [0 .. w - 1], y <- [0 .. h - 1]]

freeCells :: GridMap -> [Cell]
freeCells gm = filter (isFree gm) (allCells gm)
