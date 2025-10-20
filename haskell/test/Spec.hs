{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Coverage.Repair (AssignedDepot (..), buildPath)
import Coverage.STC (planSTC)
import Cost.Evaluate (pathCells, validateBattery)
import Domain.Cell (Cell (..), manhattan)
import Domain.Depot (Depot (..))
import Domain.Drone (DroneSpec (..))
import Domain.Map (GridMap (..), freeCells, isFree)
import Partition.Common (RegionAssignment (..))
import qualified Partition.RegionGrow as Region
import qualified Partition.Strip as Strip
import Test.QuickCheck
import qualified Data.HashSet as HS

main :: IO ()
main = do
  putStrLn "Running prop_region_cover:"
  quickCheck prop_region_cover
  putStrLn "Running prop_planned_path_respects_obstacles:"
  quickCheck prop_planned_path_respects_obstacles
  putStrLn "Running prop_battery_respected:"
  quickCheck prop_battery_respected

emptyMap :: Int -> Int -> GridMap
emptyMap w h = GridMap w h 0 HS.empty HS.empty

defaultDepot :: Depot
defaultDepot = Depot 0 (Cell 0 0)

prop_region_cover :: Positive Int -> Positive Int -> Positive Int -> Bool
prop_region_cover (Positive rawW) (Positive rawH) (Positive rawD) =
  let w = 2 + rawW `mod` 6
      h = 2 + rawH `mod` 6
      drones = 1 + rawD `mod` 4
      gm = emptyMap w h
      depots = [defaultDepot]
      regions = Region.regionGrow gm depots drones
      assignedCells = concatMap raCells regions
      unique = length assignedCells == HS.size (HS.fromList assignedCells)
   in unique && HS.fromList assignedCells == HS.fromList (freeCells gm)

prop_planned_path_respects_obstacles :: Property
prop_planned_path_respects_obstacles = property $
  let gm = emptyMap 5 5
      obstacle = Cell 2 2
      gmBlocked = gm {gmObstacles = HS.singleton obstacle}
      region = RegionAssignment 0 (Cell 0 0) (filter (/= obstacle) (freeCells gmBlocked))
      drone = DroneSpec {dsSpeed = 1, dsBattery = 200, dsTakeoffCost = 1, dsLandingCost = 1, dsMaxDrones = 1}
      depot = AssignedDepot defaultDepot (Cell 0 0)
   in case buildPath gmBlocked drone depot (planSTC region) of
        Left err -> counterexample ("buildPath failed: " ++ err) $ property False
        Right (path, _, _) -> counterexample "Path enters blocked cell" $ all (isFree gmBlocked) (pathCells path)

prop_battery_respected :: Property
prop_battery_respected = property $
  let gm = emptyMap 10 1
      regionCells = freeCells gm
      region = RegionAssignment 0 (Cell 0 0) regionCells
      drone = DroneSpec {dsSpeed = 1, dsBattery = 50, dsTakeoffCost = 1, dsLandingCost = 1, dsMaxDrones = 1}
      depot = AssignedDepot defaultDepot (Cell 0 0)
   in case buildPath gm drone depot (planSTC region) of
        Left err -> counterexample ("buildPath failed: " ++ err) $ property False
        Right (path, recharges, _) -> property (validateBattery drone recharges path)
