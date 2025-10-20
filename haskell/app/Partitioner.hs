module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as BL
import Options.Applicative
import Domain.Drone (DroneSpec (..))
import Domain.Map (GridMap)
import Domain.Depot (Depot)
import IO.Json (MapContext (..), encodeRegionsLazy, readMapContext, readParams, writeRegions)
import Partition.Common (RegionAssignment)
import qualified Partition.RegionGrow as Region
import qualified Partition.Strip as Strip

data PartitionMethod = RegionGrowMethod | StripMethod deriving (Eq, Show)

data Options = Options
  { optMap :: !FilePath
  , optParams :: !FilePath
  , optOutput :: !(Maybe FilePath)
  , optMethod :: !PartitionMethod
  , optDrones :: !(Maybe Int)
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strOption (long "map" <> short 'm' <> metavar "FILE" <> help "Path to map.json")
    <*> strOption (long "params" <> short 'p' <> metavar "FILE" <> help "Path to params.json")
    <*> optional (strOption (long "output" <> short 'o' <> metavar "FILE" <> help "Output regions.json (defaults to stdout)"))
    <*> option
      (eitherReader parseMethod)
      ( long "method"
          <> metavar "METHOD"
          <> value RegionGrowMethod
          <> showDefaultWith showMethod
          <> help "Partition method: region or strip"
      )
    <*> optional (option auto (long "drones" <> metavar "INT" <> help "Override number of drones"))
  where
    parseMethod "region" = Right RegionGrowMethod
    parseMethod "strip" = Right StripMethod
    parseMethod other = Left $ "Unsupported method: " <> other
    showMethod RegionGrowMethod = "region"
    showMethod StripMethod = "strip"

main :: IO ()
main = do
  Options {..} <- execParser opts
  mapCtxEither <- readMapContext optMap
  paramsEither <- readParams optParams
  case (mapCtxEither, paramsEither) of
    (Left err, _) -> error err
    (_, Left err) -> error err
    (Right mapCtx, Right params) -> do
      let droneCount = max 1 $ maybe (dsMaxDrones params) id optDrones
          regions = selectMethod optMethod (mcMap mapCtx) (mcDepots mapCtx) droneCount
      case optOutput of
        Nothing -> BL.putStrLn (encodeRegionsLazy regions)
        Just outPath -> writeRegions outPath regions

selectMethod :: PartitionMethod -> GridMap -> [Depot] -> Int -> [RegionAssignment]
selectMethod RegionGrowMethod = Region.regionGrow
selectMethod StripMethod = Strip.stripPartition

opts :: ParserInfo Options
opts =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc "Partition the workspace among drones"
        <> header "coverage-partitioner"
    )
