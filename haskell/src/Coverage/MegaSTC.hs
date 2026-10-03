-- | Spanning-tree coverage over 2x2 mega-cells (Gabriely & Rimon style).
--
-- Fine-cell DFS STC walks every tree edge twice (~2x per cell). Mega-STC builds
-- a spanning tree on complete 2x2 blocks, covers each block once along a DFS
-- Euler tour of that tree (oriented 4-cell snakes + short bridges), then finishes
-- leftover cells (odd borders / partial blocks) with the fine-cell walk.
module Coverage.MegaSTC
  ( planMegaSTC
  ) where

import Data.List (foldl', sortOn)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import Domain.Cell (Cell (..), neighbors4)
import Partition.Common (RegionAssignment (..))
import qualified Coverage.STC as Fine

type Mega = (Int, Int)

data Dir = DirE | DirW | DirS | DirN
  deriving (Eq, Show)

planMegaSTC :: RegionAssignment -> [Cell]
planMegaSTC region@RegionAssignment {..}
  | HS.null megaCells = Fine.planSTC region
  | otherwise =
      -- Only snake cells count as covered for leftover detection. Bridges may
      -- graze orphan cells, and a later shortcut pass can drop those grazes.
      let (megaPath, snakeCovered) = coverTour regionSet seedMega children
          leftovers =
            [ c
            | c <- raSeed : raCells
            , not (HS.member c snakeCovered)
            ]
          base =
            if null leftovers
              then megaPath
              else
                stitch
                  regionSet
                  megaPath
                  ( Fine.planSTC
                      RegionAssignment
                        { raDroneId = raDroneId
                        , raSeed = head leftovers
                        , raCells = leftovers
                        }
                  )
       in dedupe (ensureCoverage raDroneId regionSet base)
  where
    regionSet = HS.fromList (raSeed : raCells)
    megaCells = completeMegas regionSet
    seedMega = nearestMega raSeed megaCells
    children = buildChildren seedMega megaCells

-- | If anything is still missing (e.g. after an external shortcut), append a
-- fine-STC patch so the route remains a complete cover of the region.
ensureCoverage :: Int -> HS.HashSet Cell -> [Cell] -> [Cell]
ensureCoverage droneId regionSet route =
  let missing =
        [ c
        | c <- HS.toList regionSet
        , not (HS.member c (HS.fromList route))
        ]
   in if null missing
        then route
        else
          stitch
            regionSet
            route
            ( Fine.planSTC
                RegionAssignment
                  { raDroneId = droneId
                  , raSeed = head missing
                  , raCells = missing
                  }
            )

completeMegas :: HS.HashSet Cell -> HS.HashSet Mega
completeMegas region =
  HS.fromList
    [ (mx, my)
    | Cell x y <- HS.toList region
    , let mx = x `div` 2
          my = y `div` 2
    , all (`HS.member` region) (quadCells (mx, my))
    ]

-- | NW, NE, SW, SE
quadCells :: Mega -> [Cell]
quadCells (mx, my) =
  [ Cell (2 * mx) (2 * my)
  , Cell (2 * mx + 1) (2 * my)
  , Cell (2 * mx) (2 * my + 1)
  , Cell (2 * mx + 1) (2 * my + 1)
  ]

nearestMega :: Cell -> HS.HashSet Mega -> Mega
nearestMega (Cell x y) megas =
  case sortOn (\(mx, my) -> abs (x - 2 * mx) + abs (y - 2 * my)) (HS.toList megas) of
    (m : _) -> m
    [] -> (x `div` 2, y `div` 2)

megaNeighbors :: Mega -> [Mega]
megaNeighbors (mx, my) =
  [(mx, my + 1), (mx + 1, my), (mx, my - 1), (mx - 1, my)]

direction :: Mega -> Mega -> Dir
direction (mx1, my1) (mx2, my2)
  | my1 == my2 && mx2 == mx1 + 1 = DirE
  | my1 == my2 && mx2 == mx1 - 1 = DirW
  | mx1 == mx2 && my2 == my1 + 1 = DirS
  | mx1 == mx2 && my2 == my1 - 1 = DirN
  | otherwise = error ("MegaSTC: non-cardinal " ++ show ((mx1, my1), (mx2, my2)))

buildChildren :: Mega -> HS.HashSet Mega -> HM.HashMap Mega [Mega]
buildChildren root megas = fst (go root (HS.singleton root) HM.empty)
  where
    go current visited kids =
      let nbrs =
            [ n
            | n <- megaNeighbors current
            , HS.member n megas
            , not (HS.member n visited)
            ]
          step (vis, acc, childList) n =
            let (acc', vis') = go n (HS.insert n vis) acc
             in (vis', acc', childList ++ [n])
          (visF, kidsF, childList) = foldl' step (visited, kids, []) nbrs
       in (HM.insert current childList kidsF, visF)

-- | Euler tour of the mega tree (each edge traversed both ways).
eulerTour :: Mega -> HM.HashMap Mega [Mega] -> [Mega]
eulerTour root kids = go root
  where
    go mega =
      mega
        : concatMap (\child -> go child ++ [mega]) (HM.lookupDefault [] mega kids)

-- | Cover each mega on first visit; bridge through the region between visits.
-- Returns the route and the set of cells covered by intentional 2x2 snakes.
coverTour :: HS.HashSet Cell -> Mega -> HM.HashMap Mega [Mega] -> ([Cell], HS.HashSet Cell)
coverTour region root kids =
  let tour = eulerTour root kids
      step (path, seen, prev, snakes) mega
        | HS.member mega seen = (path, seen, Just mega, snakes)
        | otherwise =
            let entry =
                  case prev of
                    Just p | adjacentMega p mega -> entryCell mega (direction p mega)
                    _ -> head (quadCells mega)
                exitDir = nextUnseenChild mega seen kids
                snake = snakeQuad mega entry exitDir
                path' =
                  if null path
                    then snake
                    else path ++ drop 1 (shortestPath region (last path) (head snake)) ++ drop 1 snake
             in (path', HS.insert mega seen, Just mega, foldr HS.insert snakes snake)
      (pathF, _, _, snakesF) = foldl' step ([], HS.empty, Nothing, HS.empty) tour
   in if null pathF
        then
          let snake = snakeQuad root (head (quadCells root)) Nothing
           in (snake, HS.fromList snake)
        else (pathF, snakesF)

adjacentMega :: Mega -> Mega -> Bool
adjacentMega a b = abs (fst a - fst b) + abs (snd a - snd b) == 1

nextUnseenChild :: Mega -> HS.HashSet Mega -> HM.HashMap Mega [Mega] -> Maybe Dir
nextUnseenChild mega seen kids =
  case [c | c <- HM.lookupDefault [] mega kids, not (HS.member c seen)] of
    (c : _) -> Just (direction mega c)
    [] -> Nothing

-- | Fine cell first reached when entering 'mega' from direction 'dir'.
entryCell :: Mega -> Dir -> Cell
entryCell (mx, my) dir = case dir of
  DirE -> Cell (2 * mx) (2 * my + 1) -- from west, prefer SW/NW; pick SW
  DirW -> Cell (2 * mx + 1) (2 * my + 1)
  DirS -> Cell (2 * mx + 1) (2 * my) -- from north
  DirN -> Cell (2 * mx + 1) (2 * my + 1) -- from south

-- | Hamiltonian path on the 2x2 starting at 'entry', biased to finish toward 'exit'.
snakeQuad :: Mega -> Cell -> Maybe Dir -> [Cell]
snakeQuad mega entry exitDir =
  let cells = quadCells mega
      cycleCCW = case cells of
        [nw, ne, sw, se] -> [nw, ne, se, sw]
        xs -> xs
      cycleCW = reverse cycleCCW
      startAt xs = case break (== entry) xs of
        (pre, y : post) -> y : post ++ pre
        _ -> entry : filter (/= entry) cells
      forward = startAt cycleCCW
      backward = startAt cycleCW
   in case exitDir of
        Nothing -> forward
        Just dir ->
          let end = exitCell mega dir
              score xs = case reverse xs of
                (e : _) | e == end -> 0
                (e : _) | manhattan e end == 1 -> 1
                _ -> 2
           in if score forward <= score backward then forward else backward

exitCell :: Mega -> Dir -> Cell
exitCell (mx, my) dir = case dir of
  DirE -> Cell (2 * mx + 1) (2 * my + 1)
  DirW -> Cell (2 * mx) (2 * my)
  DirS -> Cell (2 * mx + 1) (2 * my + 1)
  DirN -> Cell (2 * mx) (2 * my)

manhattan :: Cell -> Cell -> Int
manhattan (Cell x1 y1) (Cell x2 y2) = abs (x1 - x2) + abs (y1 - y2)

stitch :: HS.HashSet Cell -> [Cell] -> [Cell] -> [Cell]
stitch region primary secondary
  | null primary = secondary
  | null secondary = primary
  | otherwise =
      let bridge = shortestPath region (last primary) (head secondary)
       in primary ++ drop 1 bridge ++ drop 1 secondary

shortestPath :: HS.HashSet Cell -> Cell -> Cell -> [Cell]
shortestPath region start end
  | start == end = [start]
  | otherwise = maybe [start, end] id (bfs (Seq.singleton start) (M.singleton start Nothing))
  where
    bfs queue parents = case Seq.viewl queue of
      Seq.EmptyL -> Nothing
      cell Seq.:< rest
        | cell == end -> Just (walkBack parents end [])
        | otherwise ->
            let fresh =
                  [ n
                  | n <- neighbors4 cell
                  , HS.member n region
                  , not (M.member n parents)
                  ]
                parents' = foldr (\n -> M.insert n (Just cell)) parents fresh
             in bfs (foldl (Seq.|>) rest fresh) parents'
    walkBack parents cell acc = case M.lookup cell parents of
      Just (Just parent) -> walkBack parents parent (cell : acc)
      _ -> cell : acc

dedupe :: [Cell] -> [Cell]
dedupe (x : y : zs)
  | x == y = dedupe (y : zs)
  | otherwise = x : dedupe (y : zs)
dedupe xs = xs
