{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Control.Monad (unless)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Set as Set
import System.Exit (exitFailure)
import Test.QuickCheck

import Algorithm.AStar (astarPath)
import Algorithm.Flood (DepotField, bfsDistances, chainHome, depotField, fieldCells, fieldCost, fieldReturn)
import Coverage.Boustro (planBoustro)
import Coverage.MegaSTC (planMegaSTC)
import Coverage.Repair (AssignedDepot (..), BuildResult (..), buildPath, roundTripCost)
import Coverage.STC (planSTC)
import Cost.Evaluate (pathCells, validateBattery)
import Domain.Cell (Cell (..), headingFromTo, manhattan)
import Domain.Depot (Depot (..))
import Domain.Drone (DroneSpec (..))
import Domain.Map (GridMap (..), freeCells, freeNeighbors, isFree, reachableFree)
import Partition.Common (RegionAssignment (..))
import qualified Partition.RegionGrow as Region
import qualified Partition.Strip as Strip

main :: IO ()
main = do
  results <-
    mapM
      run
      [ ("region partition covers every free cell", property prop_region_cover)
      , ("regions only contain depot-reachable cells", property prop_regions_reachable)
      , ("strip partition covers reachable cells", property prop_strip_reachable)
      , ("A* path length equals BFS distance", property prop_astar_optimal)
      , ("STC visits every cell of a disconnected region", property prop_stc_disconnected)
      , ("mega-STC covers every cell of an open rectangle", property prop_mega_stc_rectangle)
      , ("planned path avoids obstacles", property prop_planned_path_respects_obstacles)
      , ("battery invariant with recharges on a corridor", property prop_battery_corridor)
      , ("depot field is cost optimal and self consistent", property prop_depot_field_optimal)
      , ("end-to-end coverage of reachable in-range cells", property prop_full_coverage)
      ]
  unless (and results) exitFailure
  where
    run (name, prop) = do
      putStrLn ("== " ++ name)
      result <- quickCheckWithResult stdArgs {maxSuccess = 200} prop
      pure (isSuccess result)

-- * Generators

data World = World
  { wMap :: GridMap
  , wDepot :: Cell
  }
  deriving (Show)

instance Arbitrary World where
  arbitrary = do
    w <- chooseInt (2, 12)
    h <- chooseInt (2, 12)
    density <- choose (0, 0.35 :: Double)
    obstacles <-
      fmap concat . mapM (\c -> do p <- choose (0, 1); pure [c | p < density]) $
        [Cell x y | x <- [0 .. w - 1], y <- [0 .. h - 1]]
    let gm0 = GridMap w h 0 (HS.fromList obstacles) HS.empty
    depot <- case freeCells gm0 of
      [] -> pure (Cell 0 0)
      cells -> elements cells
    pure (World gm0 {gmObstacles = HS.delete depot (gmObstacles gm0)} depot)

emptyMap :: Int -> Int -> GridMap
emptyMap w h = GridMap w h 0 HS.empty HS.empty

spec :: Double -> DroneSpec
spec battery = DroneSpec {dsSpeed = 1, dsBattery = battery, dsTakeoffCost = 1, dsLandingCost = 1, dsMaxDrones = 1}

-- * Partitioning

prop_region_cover :: Positive Int -> Positive Int -> Positive Int -> Bool
prop_region_cover (Positive rawW) (Positive rawH) (Positive rawD) =
  let gm = emptyMap (2 + rawW `mod` 6) (2 + rawH `mod` 6)
      regions = Region.regionGrow gm [Depot 0 (Cell 0 0)] (spec 100) (1 + rawD `mod` 4)
      assigned = concatMap raCells regions
   in length assigned == HS.size (HS.fromList assigned)
        && HS.fromList assigned == HS.fromList (freeCells gm)

prop_regions_reachable :: World -> Positive Int -> Bool
prop_regions_reachable (World gm depot) (Positive rawD) =
  let regions = Region.regionGrow gm [Depot 0 depot] (spec 100) (1 + rawD `mod` 5)
      assigned = concatMap raCells regions
      reachable = reachableFree gm [depot]
   in HS.fromList assigned == reachable
        && all ((`HS.member` reachable) . raSeed) regions
        && length assigned == HS.size (HS.fromList assigned)

prop_strip_reachable :: World -> Positive Int -> Bool
prop_strip_reachable (World gm depot) (Positive rawD) =
  let regions = Strip.stripPartition gm [Depot 0 depot] (1 + rawD `mod` 5)
      assigned = concatMap raCells regions
   in HS.fromList assigned == reachableFree gm [depot]
        && all (not . null . raCells) regions

-- * Path finding

prop_astar_optimal :: World -> Property
prop_astar_optimal (World gm depot) =
  let dists = bfsDistances (freeNeighbors gm) [depot]
   in conjoin
        [ counterexample (show cell) $
            case astarPath (freeNeighbors gm) depot cell of
              Nothing -> property False
              Just path ->
                length path - 1 === d
                  .&&. all (isFree gm) path
                  .&&. and (zipWith (\a b -> manhattan a b == 1) path (drop 1 path))
        | (cell, (_, d)) <- HM.toList dists
        ]

-- The field must agree with an independent Dijkstra on the cost home, and the
-- route it hands out must cost exactly what it charges. The second half is
-- the safety property: the battery reserved for the trip home is computed
-- from the cost, but the drone flies the route.
prop_depot_field_optimal :: World -> Property
prop_depot_field_optimal (World gm depot) =
  let field = depotField (gmWidth gm) (gmHeight gm) (freeNeighbors gm) 1 depot
      reference = referenceCosts gm depot
   in conjoin $
        -- Without this the properties below hold vacuously for an empty field.
        counterexample
          "field does not cover exactly the reachable cells"
          (HS.fromList (fieldCells field) === reachableFree gm [depot])
        : [ counterexample (show (cell, fieldCost field cell, HM.lookup cell reference)) $
            fieldCost field cell === HM.lookup cell reference
              .&&. counterexample "route costs more than the field charges" (routeMatches field cell)
          | cell <- fieldCells field
          ]

-- | Flying home along the route the field returns must cost what it quoted,
-- for every heading the drone might arrive with.
routeMatches :: DepotField -> Cell -> Property
routeMatches field cell =
  conjoin
    [ case fieldReturn field arrival cell of
        Nothing -> counterexample "no route home" False
        Just (quoted, heading) -> case chainHome field cell heading of
          Nothing -> counterexample "no chain home" False
          Just chain ->
            -- A single-cell chain means the drone is already at the depot,
            -- so no turn is paid to line up with a route home.
            let turnIn = case arrival of
                  Just a | length chain > 1 && a /= heading -> 1
                  _ -> 0
             in counterexample (show (arrival, quoted, chain)) $
                  quoted === routeCost chain + turnIn
    | arrival <- Nothing : fmap Just [0 .. 3]
    ]

-- | Steps plus heading changes along a route.
routeCost :: [Cell] -> Double
routeCost chain =
  let headings = zipWith headingFromTo chain (drop 1 chain)
      turns = length [() | (Just a, Just b) <- zip headings (drop 1 headings), a /= b]
   in fromIntegral (length chain - 1) + fromIntegral turns

-- | Dijkstra over (cell, heading) written independently of Algorithm.Flood.
referenceCosts :: GridMap -> Cell -> HM.HashMap Cell Double
referenceCosts gm depot = go (Set.singleton (0, depot, Nothing)) HM.empty HM.empty
  where
    go queue settled best = case Set.minView queue of
      Nothing -> best
      Just ((cost, cell, heading), rest)
        | HM.lookupDefault (1 / 0) (cell, heading) settled <= cost -> go rest settled best
        | otherwise ->
            let settled' = HM.insert (cell, heading) cost settled
                best' = HM.insertWith min cell cost best
                next =
                  [ (cost', n, Just h)
                  | n <- freeNeighbors gm cell
                  , Just h <- [headingFromTo n cell]
                  , let turn = case heading of
                          Just prev | prev /= h -> 1
                          _ -> 0
                  , let cost' = cost + 1 + turn
                  , HM.lookupDefault (1 / 0) (n, Just h) settled' > cost'
                  ]
             in go (foldr Set.insert rest next) settled' best'

-- * Coverage patterns

prop_stc_disconnected :: Bool
prop_stc_disconnected =
  let cells = [Cell x 0 | x <- [0 .. 2]] ++ [Cell x 0 | x <- [5 .. 7]]
      route = planSTC (RegionAssignment 0 (Cell 0 0) cells)
   in HS.fromList route == HS.fromList cells

-- Open even-sized rectangle: mega-STC should cover every cell, and on a clean
-- 2x2 tiling the route should stay close to one visit per cell.
prop_mega_stc_rectangle :: Bool
prop_mega_stc_rectangle =
  let cells = [Cell x y | x <- [0 .. 5], y <- [0 .. 5]]
      route = planMegaSTC (RegionAssignment 0 (Cell 0 0) cells)
      covered = HS.fromList route
   in covered == HS.fromList cells
        && length route <= length cells * 2

-- * Repair / battery

prop_planned_path_respects_obstacles :: Property
prop_planned_path_respects_obstacles =
  let gm = (emptyMap 5 5) {gmObstacles = HS.singleton (Cell 2 2)}
      region = RegionAssignment 0 (Cell 0 0) (freeCells gm)
   in case buildPath gm (spec 200) (AssignedDepot (Depot 0 (Cell 0 0)) (Cell 0 0)) (planSTC region) of
        Left err -> counterexample ("buildPath failed: " ++ err) False
        Right BuildResult {brPath} -> property (all (isFree gm) (pathCells brPath))

-- A long 1-cell-wide corridor with a battery that only allows short sorties:
-- the tour must recharge repeatedly and still cover every cell within range.
prop_battery_corridor :: Property
prop_battery_corridor =
  forAll (chooseInt (5, 40)) $ \len ->
    forAll (choose (6, 30 :: Double)) $ \battery ->
      let gm = emptyMap len 1
          drone = spec battery
          depot = AssignedDepot (Depot 0 (Cell 0 0)) (Cell 0 0)
          region = RegionAssignment 0 (Cell 0 0) (freeCells gm)
          field = depotField (gmWidth gm) (gmHeight gm) (freeNeighbors gm) 1 (Cell 0 0)
          inRange c = maybe False (<= battery) (roundTripCost drone field c)
       in case buildPath gm drone depot (planSTC region) of
            Left err -> counterexample ("buildPath failed: " ++ err) False
            Right BuildResult {..} ->
              let covered = HS.fromList (pathCells brPath)
                  expected = filter inRange (freeCells gm)
               in counterexample ("recharges " ++ show brRechargeStops) $
                    validateBattery drone brRechargeStops brPath
                      .&&. all (`HS.member` covered) expected
                      .&&. all (not . inRange) brSkipped

-- Whole pipeline on random maps: every depot-reachable cell whose round trip
-- fits the battery is covered, the path is continuous and obstacle free, and
-- the battery accounting checks out.
prop_full_coverage :: World -> Positive Int -> Bool -> Property
prop_full_coverage (World gm depotCell) (Positive rawD) useStc =
  forAll (choose (4, 60 :: Double)) $ \battery ->
    let drone = spec battery
        depots = [Depot 0 depotCell]
        regions = Region.regionGrow gm depots drone (1 + rawD `mod` 4)
        planner = if useStc then planSTC else planBoustro
        depot = AssignedDepot (Depot 0 depotCell) depotCell
        field = depotField (gmWidth gm) (gmHeight gm) (freeNeighbors gm) 1 depotCell
        inRange c = maybe False (<= battery) (roundTripCost drone field c)
        expected = filter inRange (HS.toList (reachableFree gm [depotCell]))
        built = [buildPath gm drone depot (planner r) | r <- regions, not (null (raCells r))]
     in case sequence built of
          Left err -> counterexample ("buildPath failed: " ++ err) False
          Right results ->
            let covered = HS.fromList (concatMap (pathCells . brPath) results)
                continuous path = and (zipWith (\a b -> manhattan a b <= 1) path (drop 1 path))
             in conjoin
                  [ counterexample "uncovered cells" $ all (`HS.member` covered) expected
                  , counterexample "path leaves free space" $ all (all (isFree gm) . pathCells . brPath) results
                  , counterexample "path not continuous" $ all (continuous . pathCells . brPath) results
                  , counterexample "battery violated" $ all (\r -> validateBattery drone (brRechargeStops r) (brPath r)) results
                  , counterexample "skipped an in-range cell" $ all (all (not . inRange) . brSkipped) results
                  , counterexample "tour does not end at depot" $
                      all (\r -> last (pathCells (brPath r)) == depotCell) results
                  ]

