module Domain.Map
  ( GridMap (..)
  , RawMap (..)
  , fromRaw
  , isInside
  , isBlocked
  , isFree
  , freeCells
  , freeNeighbors
  , reachableFree
  , allCells
  ) where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Algorithm.Flood (reachableFrom)
import Domain.Cell (Cell (..), neighbors4)
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

-- | Cells whose centre lies inside any polygon. Only the cells within each
-- polygon's bounding box are tested, which keeps large maps cheap.
rasterizeNoFly :: Int -> Int -> [[Vertex]] -> HashSet Cell
rasterizeNoFly w h polys = HS.fromList (concatMap rasterize polys)
  where
    rasterize [] = []
    rasterize poly =
      let xs = fmap vx poly
          ys = fmap vy poly
          x0 = max 0 (floor (minimum xs))
          x1 = min (w - 1) (ceiling (maximum xs))
          y0 = max 0 (floor (minimum ys))
          y1 = min (h - 1) (ceiling (maximum ys))
       in [ Cell x y
          | x <- [x0 .. x1]
          , y <- [y0 .. y1]
          , pointInPolygon (fromIntegral x + 0.5, fromIntegral y + 0.5) poly
          ]

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

-- | 4-connected free neighbours of a cell.
freeNeighbors :: GridMap -> Cell -> [Cell]
freeNeighbors gm = filter (isFree gm) . neighbors4

-- | All free cells reachable from any of the given cells (blocked sources are ignored).
reachableFree :: GridMap -> [Cell] -> HashSet Cell
reachableFree gm sources = reachableFrom (freeNeighbors gm) (filter (isFree gm) sources)
