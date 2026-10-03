-- | Cheap post-pass that removes useless revisit loops from a coverage route.
--
-- Where the route returns to a cell and every cell on that loop appears again
-- outside the loop, the detour is dropped so in-region overlap shrinks without
-- losing coverage.
module Coverage.Shortcut
  ( shortcutRoute
  ) where

import Data.List (foldl')
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Domain.Cell (Cell (..))

shortcutRoute :: HS.HashSet Cell -> [Cell] -> [Cell]
shortcutRoute _region route
  | length route < 3 = route
  | otherwise = dedupe (removeLoops route)

removeLoops :: [Cell] -> [Cell]
removeLoops route = go route
  where
    totals = countCells route
    go [] = []
    go [c] = [c]
    go (c : rest) =
      case findLoop c rest of
        Nothing -> c : go rest
        Just (interior, after) ->
          if disposable totals interior
            then go (c : after) -- drop interior; keep the revisit end via after
            else c : go rest

-- | Find the nearest later revisit of 'target' with a non-empty interior.
findLoop :: Cell -> [Cell] -> Maybe ([Cell], [Cell])
findLoop target xs = search [] xs
  where
    search _ [] = Nothing
    search acc (y : ys)
      | y == target && not (null acc) = Just (reverse acc, ys)
      | otherwise = search (y : acc) ys

-- | Interior cells may be dropped only when each still appears outside.
disposable :: HM.HashMap Cell Int -> [Cell] -> Bool
disposable totals interior =
  let interiorCounts = countCells interior
   in all
        ( \(cell, n) -> HM.lookupDefault 0 cell totals > n
        )
        (HM.toList interiorCounts)

countCells :: [Cell] -> HM.HashMap Cell Int
countCells = foldl' (\m c -> HM.insertWith (+) c 1 m) HM.empty

dedupe :: [Cell] -> [Cell]
dedupe (x : y : zs)
  | x == y = dedupe (y : zs)
  | otherwise = x : dedupe (y : zs)
dedupe xs = xs
