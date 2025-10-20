module Main (main) where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Function (on)
import Data.List (minimumBy)
import Options.Applicative
import Algorithm.AStar (astarPath)
import Coverage.Boustro (planBoustro)
import Coverage.Repair (AssignedDepot (..), buildPath)
import Coverage.STC (planSTC)
import Domain.Cell (Cell (..), manhattan, neighbors4)
import Domain.Depot (Depot (..))
import Domain.Drone (DroneSpec)
import Domain.Map (isFree)
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
  mapCtxEither <- readMapContext optMap
  paramsEither <- readParams optParams
  regionsEither <- readRegions optRegions
  case (mapCtxEither, paramsEither, regionsEither) of
    (Left err, _, _) -> fail err
    (_, Left err, _) -> fail err
    (_, _, Left err) -> fail err
    (Right mapCtx, Right params, Right regions) -> do
      let reachableRegions = filter (isReachable mapCtx) regions
      tours <- mapM (planRegion mapCtx params optAlgorithm) reachableRegions
      case optOutput of
        Nothing -> BL.putStrLn (encode tours)
        Just outPath -> writeTours outPath tours

isReachable :: MapContext -> RegionAssignment -> Bool
isReachable mapCtx region =
  let depot = assignDepot (mcDepots mapCtx) region
      depotCell = adCell depot
      seedCell = raSeed region
      neighborsFn c = filter (isFree (mcMap mapCtx)) (neighbors4 c)
  in case astarPath neighborsFn seedCell depotCell of
       Nothing -> False
       Just _ -> True

planRegion :: MapContext -> DroneSpec -> CoverageAlgorithm -> RegionAssignment -> IO Tour
planRegion mapCtx params algo region =
  case buildPath (mcMap mapCtx) params depot route of
    Left err -> fail err
    Right (path, recharges, cost) -> pure (mkTour (raDroneId region) path recharges cost)
  where
    route = selectAlgorithm algo region
    depot = assignDepot (mcDepots mapCtx) region

selectAlgorithm :: CoverageAlgorithm -> RegionAssignment -> [Cell]
selectAlgorithm AlgoSTC = planSTC
selectAlgorithm AlgoBoustro = planBoustro

assignDepot :: [Depot] -> RegionAssignment -> AssignedDepot
assignDepot [] region =
  let fallback = Depot 0 (raSeed region)
   in AssignedDepot fallback (raSeed region)
assignDepot depots region = AssignedDepot chosen (depotCell chosen)
  where
    chosen = minimumBy (compare `on` dist) depots
    dist depot = manhattan (depotCell depot) (raSeed region)

opts :: ParserInfo Options
opts =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc "Generate per-drone coverage tours"
        <> header "coverage-planner"
    )
