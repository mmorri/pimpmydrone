{-# LANGUAGE BangPatterns #-}
module Algorithm.Flood
  ( DepotField
  , bfsDistances
  , reachableFrom
  , depotField
  , fieldCost
  , fieldReturn
  , chainHome
  , fieldCells
  ) where

import Control.Monad.ST (ST, runST)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Data.Sequence (ViewL (..), viewl, (|>))
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Domain.Cell (Cell (..), headingFromTo)

-- | Multi-source breadth-first distances. Each source is labelled with the
-- index of the source that reached it first; ties are broken by source order.
bfsDistances :: (Cell -> [Cell]) -> [Cell] -> HashMap Cell (Int, Int)
bfsDistances neighbors sources = go (Seq.fromList seeds) (HM.fromList seeds)
  where
    seeds = [(cell, (label, 0)) | (label, cell) <- zip [0 ..] sources]
    go queue acc = case viewl queue of
      EmptyL -> acc
      (cell, (label, dist)) :< rest ->
        let (queue', acc') = foldl' (visit label (dist + 1)) (rest, acc) (neighbors cell)
         in go queue' acc'
    visit label dist (queue, acc) next
      | HM.member next acc = (queue, acc)
      | otherwise = (queue |> (next, (label, dist)), HM.insert next (label, dist) acc)

-- | Every cell reachable from any of the sources.
reachableFrom :: (Cell -> [Cell]) -> [Cell] -> HashSet Cell
reachableFrom neighbors = HS.fromMap . HM.map (const ()) . bfsDistances neighbors

-- | Cheapest routes home to a depot, stored in flat arrays indexed by
-- @y * width + x@ (and heading). Same Dijkstra semantics as the old HashMap
-- field, without per-cell heap overhead on large grids.
data DepotField = DepotField
  { dfWidth :: !Int
  , dfNCells :: !Int
  , dfStateCost :: !(VU.Vector Double)
  , dfStatePrev :: !(VU.Vector Int)
  , dfBestCost :: !(VU.Vector Double)
  , dfBestHeading :: !(VU.Vector Int)
  }

nHeadings :: Int
nHeadings = 5

depotHeading :: Int
depotHeading = -1

headingSlot :: Int -> Int
headingSlot h = h + 1

slotHeading :: Int -> Int
slotHeading s = s - 1

cellIndex :: Int -> Cell -> Int
cellIndex width (Cell x y) = y * width + x

indexCell :: Int -> Int -> Cell
indexCell width idx = Cell (idx `mod` width) (idx `div` width)

stateId :: Int -> Cell -> Int -> Int
stateId width cell heading = cellIndex width cell * nHeadings + headingSlot heading

stateParts :: Int -> Int -> (Cell, Int)
stateParts width sid =
  let (ci, slot) = sid `divMod` nHeadings
   in (indexCell width ci, slotHeading slot)

inf :: Double
inf = 1e300

-- | Build a turn-aware cheapest-return field.
--
-- @width@ and @height@ size the flat arrays; @neighbors@ must only yield cells
-- inside that rectangle.
depotField :: Int -> Int -> (Cell -> [Cell]) -> Double -> Cell -> DepotField
depotField width height neighbors speed depot
  | width <= 0 || height <= 0 = error "depotField: positive dimensions required"
  | otherwise =
      let !nCells = width * height
          !nStates = nCells * nHeadings
          !step = 1 / speed
          !source = stateId width depot depotHeading
          (stateCost, statePrev, bestCost, bestHeading) =
            runST $ do
              costs <- VUM.replicate nStates inf
              prevs <- VUM.replicate nStates (-1 :: Int)
              bestC <- VUM.replicate nCells inf
              bestH <- VUM.replicate nCells depotHeading
              VUM.write costs source 0
              VUM.write prevs source (-1)
              VUM.write bestC (cellIndex width depot) 0
              VUM.write bestH (cellIndex width depot) depotHeading
              let go !open !settled
                    | S.null open = pure ()
                    | otherwise =
                        let ((cost, sid), rest) = S.deleteFindMin open
                         in if HS.member sid settled
                              then go rest settled
                              else do
                                let settled' = HS.insert sid settled
                                    (cell, heading) = stateParts width sid
                                open' <-
                                  expand
                                    width
                                    nCells
                                    nStates
                                    neighbors
                                    step
                                    cost
                                    sid
                                    cell
                                    heading
                                    costs
                                    prevs
                                    bestC
                                    bestH
                                    rest
                                go open' settled'
              go (S.singleton (0 :: Double, source)) HS.empty
              (,,,)
                <$> VU.unsafeFreeze costs
                <*> VU.unsafeFreeze prevs
                <*> VU.unsafeFreeze bestC
                <*> VU.unsafeFreeze bestH
       in DepotField width nCells stateCost statePrev bestCost bestHeading

expand ::
  Int ->
  Int ->
  Int ->
  (Cell -> [Cell]) ->
  Double ->
  Double ->
  Int ->
  Cell ->
  Int ->
  VUM.MVector s Double ->
  VUM.MVector s Int ->
  VUM.MVector s Double ->
  VUM.MVector s Int ->
  S.Set (Double, Int) ->
  ST s (S.Set (Double, Int))
expand width nCells nStates neighbors step cost sid cell heading costs prevs bestC bestH open0 =
  foldlM'
    ( \open next ->
        case headingFromTo next cell of
          Nothing -> pure open
          Just back -> do
            let turn = if heading /= depotHeading && heading /= back then 1 else 0
                cost' = cost + step + turn
                nextSid = stateId width next back
                ci = cellIndex width next
            if nextSid < 0 || nextSid >= nStates || ci < 0 || ci >= nCells
              then pure open
              else do
                old <- VUM.read costs nextSid
                if cost' < old
                  then do
                    VUM.write costs nextSid cost'
                    VUM.write prevs nextSid sid
                    bc <- VUM.read bestC ci
                    if cost' < bc
                      then do
                        VUM.write bestC ci cost'
                        VUM.write bestH ci back
                      else pure ()
                    pure (S.insert (cost', nextSid) open)
                  else pure open
    )
    open0
    (neighbors cell)

foldlM' :: (b -> a -> ST s b) -> b -> [a] -> ST s b
foldlM' f z0 xs = go z0 xs
  where
    go z [] = pure z
    go z (x : rest) = do
      z' <- f z x
      go z' rest

fieldCost :: DepotField -> Cell -> Maybe Double
fieldCost field cell =
  let ci = cellIndex (dfWidth field) cell
   in if ci < 0 || ci >= dfNCells field
        then Nothing
        else
          let c = dfBestCost field VU.! ci
           in if c >= inf / 2 then Nothing else Just c

fieldReturn :: DepotField -> Maybe Int -> Cell -> Maybe (Double, Int)
fieldReturn field arrival cell =
  case mapMaybe cost (depotHeading : [0 .. 3]) of
    [] -> Nothing
    options -> Just (minimum options)
  where
    cost heading =
      let sid = stateId (dfWidth field) cell heading
       in if sid < 0 || sid >= VU.length (dfStateCost field)
            then Nothing
            else
              let c = dfStateCost field VU.! sid
               in if c >= inf / 2
                    then Nothing
                    else
                      let turn = case arrival of
                            Just a | heading /= depotHeading && a /= heading -> 1
                            _ -> 0
                       in Just (c + turn, heading)

chainHome :: DepotField -> Cell -> Int -> Maybe [Cell]
chainHome field cell heading =
  let sid0 = stateId (dfWidth field) cell heading
   in if sid0 < 0 || sid0 >= VU.length (dfStateCost field)
        then Nothing
        else go sid0 []
  where
    go sid acc
      | sid < 0 = Nothing
      | dfStateCost field VU.! sid >= inf / 2 = Nothing
      | otherwise =
          let (c, _) = stateParts (dfWidth field) sid
              prev = dfStatePrev field VU.! sid
           in if prev < 0
                then Just (reverse (c : acc))
                else go prev (c : acc)

fieldCells :: DepotField -> [Cell]
fieldCells field =
  [ indexCell (dfWidth field) i
  | i <- [0 .. dfNCells field - 1]
  , dfBestCost field VU.! i < inf / 2
  ]
