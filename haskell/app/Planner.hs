module Main (main) where

import Control.Monad (forM, unless)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Function (on)
import qualified Data.HashMap.Strict as HM
import Data.List (minimumBy)
import Data.Maybe (mapMaybe)
import Options.Applicative
import System.Exit (die)
import System.IO (hPutStrLn, stderr)
import Algorithm.Flood (bfsDistances)
import Coverage.Boustro (planBoustro)
import Coverage.Repair (AssignedDepot (..), BuildResult (..), buildPath)
import Coverage.STC (planSTC)
import Domain.Cell (Cell (..))
import Domain.Depot (Depot (..))
import Domain.Drone (DroneSpec)
import Domain.Map (GridMap, freeNeighbors, isFree)
import Domain.Tour (Tour, mkTour)
import IO.Json (MapContext (..), readMapContext, readParams, readRegions, writeTours)
import Partition.Common (RegionAssignment (..))

data CoverageAlgorithm = AlgoSTC | AlgoBoustro deriving (Eq, Show)

data Options = Options
  { optMap :: !FilePath
  , optParams :: !FilePath
  , optRegions :: !FilePath
  , optOutput :: !(Maybe FilePath)
  , optAlgorithm :: !CoverageAlgorithm
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strOption (long "map" <> short 'm' <> metavar "FILE" <> help "Path to map.json")
    <*> strOption (long "params" <> short 'p' <> metavar "FILE" <> help "Path to params.json")
    <*> strOption (long "regions" <> short 'r' <> metavar "FILE" <> help "Path to regions.json")
    <*> optional (strOption (long "output" <> short 'o' <> metavar "FILE" <> help "Output tours_initial.json (defaults to stdout)"))
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
  tours <- forM (mapMaybe (assignDepot depots depotIndex) nonEmpty) $ \(region, depot) ->
    planRegion gm params optAlgorithm region depot
  case optOutput of
    Nothing -> BL.putStrLn (encode tours)
    Just outPath -> writeTours outPath tours

warn :: String -> IO ()
warn = hPutStrLn stderr . ("coverage-planner: " ++)

planRegion :: GridMap -> DroneSpec -> CoverageAlgorithm -> RegionAssignment -> AssignedDepot -> IO Tour
planRegion gm params algo region depot =
  case buildPath gm params depot (selectAlgorithm algo region) of
    Left err -> die ("region " ++ show (raDroneId region) ++ ": " ++ err)
    Right BuildResult {..} -> do
      unless (null brSkipped) $
        warn
          ( "region " ++ show (raDroneId region) ++ ": " ++ show (length brSkipped)
              ++ " cell(s) exceed the battery round-trip range and were skipped"
          )
      pure (mkTour (raDroneId region) brPath brRechargeStops brCost)

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
