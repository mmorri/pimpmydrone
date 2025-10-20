module Domain.Geometry
  ( bresenham
  , lineOfSight
  ) where

import Domain.Cell (Cell (..))
import Domain.Map (GridMap, isFree)

-- | Bresenham's line discretisation between two cells, inclusive.
bresenham :: Cell -> Cell -> [Cell]
bresenham start@(Cell x0 y0) end@(Cell x1 y1)
  | start == end = [start]
  | otherwise = go x0 y0 dx dy sx sy err
  where
    dx = abs (x1 - x0)
    dy = negate (abs (y1 - y0))
    sx = if x0 < x1 then 1 else -1
    sy = if y0 < y1 then 1 else -1
    err = dx + dy
    go x y dx' dy' sx' sy' err' =
      let cell = Cell x y
       in if x == x1 && y == y1
            then [cell]
            else
              let e2 = 2 * err'
                  (xNext, errX)
                    | e2 >= dy' = (x + sx', err' + dy')
                    | otherwise = (x, err')
                  (yNext, errY)
                    | e2 <= dx' = (y + sy', errX + dx')
                    | otherwise = (y, errX)
               in cell : go xNext yNext dx' dy' sx' sy' errY

-- | True when the straight line between two cells stays within free space.
lineOfSight :: GridMap -> Cell -> Cell -> Bool
lineOfSight gm a b = all (isFree gm) (bresenham a b)
