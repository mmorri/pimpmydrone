module Domain.Cost
  ( CostBreakdown (..)
  , emptyCost
  , addSegmentCost
  ) where

-- | Aggregated mission-level cost metrics.
data CostBreakdown = CostBreakdown
  { cbTime :: !Double
  , cbTurns :: !Int
  , cbLength :: !Double
  }
  deriving (Eq, Show)

emptyCost :: CostBreakdown
emptyCost = CostBreakdown 0 0 0

addSegmentCost :: CostBreakdown -> Double -> Int -> Double -> CostBreakdown
addSegmentCost (CostBreakdown t turns len) dt dturn dlen =
  CostBreakdown (t + dt) (turns + dturn) (len + dlen)
