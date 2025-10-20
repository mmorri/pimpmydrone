module Domain.NoFly
  ( NoFlyZone (..)
  , Vertex (..)
  , pointInPolygon
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.=), (.:))
import qualified Data.Aeson as A
import GHC.Generics (Generic)

-- | Vertex of a polygon with floating-point coordinates.
data Vertex = Vertex
  { vx :: !Double
  , vy :: !Double
  }
  deriving (Eq, Show, Generic)

instance FromJSON Vertex where
  parseJSON = withObject "Vertex" $ \o ->
    Vertex <$> o .: "x" <*> o .: "y"

instance ToJSON Vertex where
  toJSON (Vertex x y) = A.object ["x" .= x, "y" .= y]

-- | Polygonal no-fly zone specified by ordered vertices.
data NoFlyZone = NoFlyZone
  { nfVertices :: ![Vertex]
  }
  deriving (Eq, Show, Generic)

instance FromJSON NoFlyZone where
  parseJSON = withObject "NoFlyZone" $ \o -> NoFlyZone <$> o .: "vertices"

instance ToJSON NoFlyZone where
  toJSON (NoFlyZone vs) = A.object ["vertices" .= vs]

-- | Point-in-polygon test using the ray casting algorithm.
pointInPolygon :: (Double, Double) -> [Vertex] -> Bool
pointInPolygon _ [] = False
pointInPolygon p verts = odd . length . filter id $ zipWith crosses verts (tail (cycle verts))
  where
    crosses (Vertex x1 y1) (Vertex x2 y2) =
      let (_, py) = p
          cond1 = (y1 > py) /= (y2 > py)
          xIntersect = (x2 - x1) * (py - y1) / (y2 - y1 + 1e-9) + x1
       in cond1 && fst p < xIntersect
