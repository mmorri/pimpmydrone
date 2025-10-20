module Domain.Cell
  ( Cell (..)
  , neighbors4
  , manhattan
  , headingFromTo
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.:), (.=))
import qualified Data.Aeson as A
import Data.Hashable (Hashable (hashWithSalt))
import GHC.Generics (Generic)

-- | A discrete grid cell with integer coordinates.
data Cell = Cell
  { cellX :: !Int
  , cellY :: !Int
  }
  deriving (Eq, Ord, Show, Generic)

instance Hashable Cell where
  hashWithSalt salt (Cell x y) = salt `hashWithSalt` x `hashWithSalt` y

instance FromJSON Cell where
  parseJSON = withObject "Cell" $ \o ->
    Cell <$> o .: "x" <*> o .: "y"

instance ToJSON Cell where
  toJSON (Cell x y) = A.object ["x" .= x, "y" .= y]

-- | L1 distance between two grid cells.
manhattan :: Cell -> Cell -> Int
manhattan (Cell x1 y1) (Cell x2 y2) = abs (x1 - x2) + abs (y1 - y2)

-- | 4-connected neighbors of a cell.
neighbors4 :: Cell -> [Cell]
neighbors4 (Cell x y) =
  [ Cell (x + 1) y
  , Cell (x - 1) y
  , Cell x (y + 1)
  , Cell x (y - 1)
  ]

-- | Heading represented as an integer (0: east, 1: west, 2: north, 3: south).
headingFromTo :: Cell -> Cell -> Maybe Int
headingFromTo (Cell x1 y1) (Cell x2 y2)
  | x2 == x1 + 1 && y2 == y1 = Just 0
  | x2 == x1 - 1 && y2 == y1 = Just 1
  | y2 == y1 + 1 && x2 == x1 = Just 2
  | y2 == y1 - 1 && x2 == x1 = Just 3
  | otherwise = Nothing
