module Main (main) where

import Control.Monad (forM, unless)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Function (on)
import qualified Data.HashMap.Strict as HM
import Data.List (minimumBy)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Options.Applicative
import System.Exit (die)
import System.IO (hPutStrLn, stderr)
import Algorithm.Flood (bfsDistances)
import Coverage.Boustro (planBoustro)
import Coverage.Repair (AssignedDepot (..), BuildResult (..), PlannerEnv, buildPathWith, prepareDepot)
import Coverage.STC (planSTC)
import Domain.Cell (Cell (..))
import Domain.Depot (Depot (..))
import Domain.Drone (DroneSpec)
import Domain.Map (GridMap, freeNeighbors, isFree)
import Domain.Tour (Tour, mkTour)
import IO.Json (MapContext (..), readMapContext, readParams, readRegions, writeSkipped, writeTours)
import Partition.Common (RegionAssignment (..))

data CoverageAlgorithm = AlgoSTC | AlgoBoustro deriving (Eq, Show)

data Options = Options
  { optMap :: !FilePath
  , optParams :: !FilePath
  , optRegions :: !FilePath
  , optOutput :: !(Maybe FilePath)
  , optSkipped :: !(Maybe FilePath)
  , optAlgorithm :: !CoverageAlgorithm
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strOption (long "map" <> short 'm' <> metavar "FILE" <> help "Path to map.json")
    <*> strOption (long "params" <> short 'p' <> metavar "FILE" <> help "Path to params.json")
    <*> strOption (long "regions" <> short 'r' <> metavar "FILE" <> help "Path to regions.json")
    <*> optional (strOption (long "output" <> short 'o' <> metavar "FILE" <> help "Output tours_initial.json (defaults to stdout)"))
    <*> optional (strOption (long "skipped" <> metavar "FILE" <> help "Write cells skipped as out of battery range"))
    <*> option
      (eitherReader parseAlgo)
      ( long "coverage-algorithm"
          <> metavar "ALGO"
          <> value AlgoSTC
          <> showDefaultWith showAlgo
          <> help "Coverage pattern: stc or boustro"
      )
  where
    parseAlgo "stc" = Right AlgoSTC
    parseAlgo "boustro" = Right AlgoBoustro
    parseAlgo other = Left $ "Unsupported coverage algorithm: " <> other
    showAlgo AlgoSTC = "stc"
    showAlgo AlgoBoustro = "boustro"

main :: IO ()
main = do
  Options {..} <- execParser opts
  mapCtx <- either (die . ("map: " ++)) pure =<< readMapContext optMap
  params <- either (die . ("params: " ++)) pure =<< readParams optParams
  regions <- either (die . ("regions: " ++)) pure =<< readRegions optRegions
  let gm = mcMap mapCtx
      depots = filter (isFree gm . depotCell) (mcDepots mapCtx)
  unless (null (mcDepots mapCtx) || not (null depots)) $
    die "Every depot lies on a blocked cell"
  -- Label every free cell with the depot that reaches it first, so regions are
  -- always served by a depot that can actually fly to them.
  let depotIndex = bfsDistances (freeNeighbors gm) (fmap depotCell depots)
      nonEmpty = filter (not . null . raCells) regions
      unreachable = filter (not . (`HM.member` depotIndex) . raSeed) nonEmpty
  unless (null unreachable) $
    warn ("Skipping " ++ show (length unreachable) ++ " region(s) that no depot can reach")
  -- Building a depot's return field is the expensive part of planning, so it
  -- is done once per depot and shared by every region that depot serves.
  let assigned = mapMaybe (assignDepot depots depotIndex) nonEmpty
  envs <- buildEnvs gm params (fmap snd assigned)
  planned <- forM assigned $ \(region, depot) ->
    planRegion (envFor envs depot) optAlgorithm region
  let tours = fmap fst planned
      skipped = [(raDroneId r, cells) | ((_, cells), r) <- zip planned nonEmpty, not (null cells)]
  case optOutput of
    Nothing -> BL.putStrLn (encode tours)
    Just outPath -> writeTours outPath tours
  case optSkipped of
    Nothing -> pure ()
    Just skipPath -> writeSkipped skipPath skipped

warn :: String -> IO ()
warn = hPutStrLn stderr . ("coverage-planner: " ++)

-- | One planner environment per distinct depot cell.
buildEnvs :: GridMap -> DroneSpec -> [AssignedDepot] -> IO (M.Map Cell PlannerEnv)
buildEnvs gm params depots =
  fmap M.fromList . forM (nubOrd (fmap adCell depots)) $ \cell -> do
    let depot = head [d | d <- depots, adCell d == cell]
    case prepareDepot gm params depot of
      Left err -> die ("depot " ++ show cell ++ ": " ++ err)
      Right env -> pure (cell, env)
  where
    nubOrd = S.toList . S.fromList

envFor :: M.Map Cell PlannerEnv -> AssignedDepot -> PlannerEnv
envFor envs depot = envs M.! adCell depot

planRegion :: PlannerEnv -> CoverageAlgorithm -> RegionAssignment -> IO (Tour, [Cell])
planRegion env algo region =
  case buildPathWith env (selectAlgorithm algo region) of
    Left err -> die ("region " ++ show (raDroneId region) ++ ": " ++ err)
    Right BuildResult {..} -> do
      unless (null brSkipped) $
        warn
          ( "region " ++ show (raDroneId region) ++ ": " ++ show (length brSkipped)
              ++ " cell(s) exceed the battery round-trip range and were skipped"
          )
      pure (mkTour (raDroneId region) brPath brRechargeStops brCost, brSkipped)

selectAlgorithm :: CoverageAlgorithm -> RegionAssignment -> [Cell]
selectAlgorithm AlgoSTC = planSTC
selectAlgorithm AlgoBoustro = planBoustro

-- | Serve a region from the depot nearest (by grid distance) to its seed.
-- With no depots at all the seed doubles as a depot.
assignDepot :: [Depot] -> HM.HashMap Cell (Int, Int) -> RegionAssignment -> Maybe (RegionAssignment, AssignedDepot)
assignDepot [] _ region = Just (region, AssignedDepot (Depot 0 (raSeed region)) (raSeed region))
assignDepot depots depotIndex region = do
  (label, _) <- HM.lookup (raSeed region) depotIndex
  let depot = minimumBy (compare `on` depotId) [d | (i, d) <- zip [0 ..] depots, i == label]
  pure (region, AssignedDepot depot (depotCell depot))

opts :: ParserInfo Options
opts =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc "Generate per-drone coverage tours"
        <> header "coverage-planner"
    )
