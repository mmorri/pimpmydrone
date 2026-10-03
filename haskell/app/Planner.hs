module Main (main) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (evaluate)
import Control.Monad (forM, unless)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Function (on)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Either (rights)
import Data.List (minimumBy)
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Options.Applicative
import System.Exit (die)
import System.IO (hPutStrLn, stderr)
import Coverage.Boustro (planBoustro)
import Coverage.MegaSTC (planMegaSTC)
import Coverage.Repair
  ( AssignedDepot (..)
  , BuildResult (..)
  , PlannerEnv
  , buildPathWith
  , plannerEnvWithFields
  )
import Coverage.Shortcut (shortcutRoute)
import Coverage.STC (planSTC)
import Domain.Cell (Cell (..), neighbors4)
import qualified Data.HashSet as HS
import qualified Data.Sequence as Seq
import Domain.Depot (Depot (..))
import Domain.Drone (DroneSpec (..))
import Domain.Map (GridMap (..), freeNeighbors, isFree)
import Algorithm.Flood (bfsDistances, depotField)
import Domain.Tour (Tour, TourCost (..), mkTour)
import IO.Json (MapContext (..), readMapContext, readParams, readRegions, writeSkipped, writeTours)
import Partition.Common (RegionAssignment (..))

-- | Which coverage pattern to run over a region. Mega-STC (2x2 spanning tree)
-- is cheaper on open ground than fine-cell STC; boustrophedon often wins on
-- open sweeps too. Cluttered ground still favours a tree walk. 'AlgoAuto'
-- plans the candidates and keeps whichever tour is faster per region.
data CoverageAlgorithm = AlgoSTC | AlgoBoustro | AlgoAuto deriving (Eq, Show)

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
          <> value AlgoAuto
          <> showDefaultWith showAlgo
          <> help "Coverage pattern: stc (2x2 mega-cell), boustro, or auto (cheaper per region)"
      )
  where
    parseAlgo "stc" = Right AlgoSTC
    parseAlgo "boustro" = Right AlgoBoustro
    parseAlgo "auto" = Right AlgoAuto
    parseAlgo other = Left $ "Unsupported coverage algorithm: " <> other
    showAlgo AlgoSTC = "stc"
    showAlgo AlgoBoustro = "boustro"
    showAlgo AlgoAuto = "auto"

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
  envs <- buildEnvs gm params (fmap snd assigned) (fmap depotCell depots)
  -- Regions are independent once depot fields exist; plan them in parallel.
  planned <-
    mapConcurrently
      ( \(region, depot) -> planRegion (envFor envs depot) optAlgorithm region
      )
      assigned
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

-- | One planner environment per distinct home depot. Depot fields are built
-- once (in parallel) and shared so mid-tour recharges can divert to the nearest.
buildEnvs :: GridMap -> DroneSpec -> [AssignedDepot] -> [Cell] -> IO (M.Map Cell PlannerEnv)
buildEnvs gm params depots mapDepotCells = do
  let homes = nubOrd (fmap adCell depots)
      fieldCells = nubOrd (homes ++ mapDepotCells)
  fields <-
    mapConcurrently
      ( \cell ->
          evaluate
            ( cell
            , depotField
                (gmWidth gm)
                (gmHeight gm)
                (freeNeighbors gm)
                (dsSpeed params)
                cell
            )
      )
      fieldCells
  fmap M.fromList $ forM homes $ \cell -> do
    let depot = head [d | d <- depots, adCell d == cell]
    case plannerEnvWithFields gm params depot fields of
      Left err -> die ("depot " ++ show cell ++ ": " ++ err)
      Right env -> pure (cell, env)
  where
    nubOrd = S.toList . S.fromList

envFor :: M.Map Cell PlannerEnv -> AssignedDepot -> PlannerEnv
envFor envs depot = envs M.! adCell depot

planRegion :: PlannerEnv -> CoverageAlgorithm -> RegionAssignment -> IO (Tour, [Cell])
planRegion env algo region =
  case candidates of
    [] -> die ("region " ++ show (raDroneId region) ++ ": no coverage pattern succeeded")
    results -> emit (minimumBy (compare `on` (tcTime . brCost)) results)
  where
    candidates = rights [buildPathWith env (pattern region) | pattern <- patternsFor algo]
    emit BuildResult {..} = do
      unless (null brSkipped) $
        warn
          ( "region " ++ show (raDroneId region) ++ ": " ++ show (length brSkipped)
              ++ " cell(s) exceed the battery round-trip range and were skipped"
          )
      pure (mkTour (raDroneId region) brPath brRechargeStops brCost, brSkipped)

patternsFor :: CoverageAlgorithm -> [RegionAssignment -> [Cell]]
patternsFor AlgoSTC = [withShortcut planMegaSTC]
patternsFor AlgoBoustro = [withShortcut planBoustro]
patternsFor AlgoAuto = [withShortcut planMegaSTC, withShortcut planBoustro, withShortcut planSTC]

withShortcut :: (RegionAssignment -> [Cell]) -> RegionAssignment -> [Cell]
withShortcut planner region =
  let regionSet = HS.fromList (raSeed region : raCells region)
      route = shortcutRoute regionSet (planner region)
      missing =
        [ c
        | c <- HS.toList regionSet
        , not (HS.member c (HS.fromList route))
        ]
   in if null missing
        then route
        else
          let patch =
                planSTC
                  RegionAssignment
                    { raDroneId = raDroneId region
                    , raSeed = head missing
                    , raCells = missing
                    }
           in if null route
                then patch
                else route ++ drop 1 (shortestInRegion regionSet (last route) (head patch)) ++ drop 1 patch

shortestInRegion :: HS.HashSet Cell -> Cell -> Cell -> [Cell]
shortestInRegion region start end
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
