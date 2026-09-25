{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Control.Monad (unless)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import System.Exit (exitFailure)
import Test.QuickCheck

import Algorithm.AStar (astarPath)
import Algorithm.Flood (bfsDistances, depotField)
import Coverage.Boustro (planBoustro)
import Coverage.Repair (AssignedDepot (..), BuildResult (..), buildPath, roundTripCost)
import Coverage.STC (planSTC)
import Cost.Evaluate (pathCells, validateBattery)
import Domain.Cell (Cell (..), manhattan)
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
      , ("planned path avoids obstacles", property prop_planned_path_respects_obstacles)
      , ("battery invariant with recharges on a corridor", property prop_battery_corridor)
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
      regions = Region.regionGrow gm [Depot 0 (Cell 0 0)] (1 + rawD `mod` 4)
      assigned = concatMap raCells regions
   in length assigned == HS.size (HS.fromList assigned)
        && HS.fromList assigned == HS.fromList (freeCells gm)

prop_regions_reachable :: World -> Positive Int -> Bool
prop_regions_reachable (World gm depot) (Positive rawD) =
  let regions = Region.regionGrow gm [Depot 0 depot] (1 + rawD `mod` 5)
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

-- * Coverage patterns

prop_stc_disconnected :: Bool
prop_stc_disconnected =
  let cells = [Cell x 0 | x <- [0 .. 2]] ++ [Cell x 0 | x <- [5 .. 7]]
      route = planSTC (RegionAssignment 0 (Cell 0 0) cells)
   in HS.fromList route == HS.fromList cells

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
          field = depotField (freeNeighbors gm) (Cell 0 0)
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
        regions = Region.regionGrow gm depots (1 + rawD `mod` 4)
        planner = if useStc then planSTC else planBoustro
        depot = AssignedDepot (Depot 0 depotCell) depotCell
        field = depotField (freeNeighbors gm) depotCell
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

