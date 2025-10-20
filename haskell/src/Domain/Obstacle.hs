module Domain.Obstacle
  ( Obstacle (..)
  , obstacleCells
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Domain.Cell (Cell (..))
import GHC.Generics (Generic)

-- | Obstacles are axis-aligned blocked cells.
data Obstacle = Obstacle
  { obstacleCell :: !Cell
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON Obstacle where
  parseJSON v = Obstacle <$> parseJSON v

instance ToJSON Obstacle where
  toJSON (Obstacle cell) = toJSON cell

obstacleCells :: [Obstacle] -> [Cell]
obstacleCells = fmap obstacleCell
