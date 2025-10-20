module Domain.Depot
  ( Depot (..)
  , depotCells
  ) where

import Domain.Cell (Cell (..))

-- | Depot where drones can take off, land, and recharge.
data Depot = Depot
  { depotId :: !Int
  , depotCell :: !Cell
  }
  deriving (Eq, Show)

depotCells :: [Depot] -> [Cell]
depotCells = fmap depotCell
