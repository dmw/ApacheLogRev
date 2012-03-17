----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Daniel Molina Wegener 2012
-- License     :  BSD 3 (see the LICENSE file)
-- Author      :  Daniel Molina Wegener <dmw@coder.cl>
-- Homepage    :  http://coder.cl/products/logrev/
-- Repository  :  https://github.com/dmw/ApacheLogRev
--
-- An Apache Access Log Statistics extractor
--
-- This is the initial commit of this project, please write
-- me directly if you want to contribute.
-----------------------------------------------------------------------------


module Main (main) where


import qualified Control.Monad as D
import qualified Data.ByteString.Char8 as B ()
import qualified Data.Map as M
import qualified Data.String.Utils as S

import Data.Char ()
import Data.GeoIP.GeoDB
import Data.Colour ()
import Data.Colour.Names ()
import Data.List
import Data.LogRev.LogStats
import Data.LogRev.Parser
import Data.LogRev.Processing
import Proc.LRS.Parser
import Data.Maybe
import Graphics.LogRev.Charts
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.IO.Unsafe ()
import System.Posix.Temp ()
import Text.JSON.Generic (encodeJSON)
import Text.Printf


progVersion :: String
progVersion = "ApacheLogRev 0.0.1"

emptyLogRevStats :: LogRevStats
emptyLogRevStats = LogRevStats { sTot   = 0
                               , sSzTot = 0
                               , sSz    = M.empty
                               , sMap   = M.empty
                               , sPer   = M.empty }

actionMap :: [LogRevStatsAction]
actionMap = [LogRevStatsAction {
                aHeader   = "HTTP Status"
                , aAction = statsHandlerStatus
                , aOutput = emptyLogRevStats
                , aPlot   = plotPngBarChart
                },
             LogRevStatsAction {
                aHeader   = "Connections From Countries"
                , aAction = statsHandlerCountry
                , aOutput = emptyLogRevStats
                , aPlot   = plotPngBarChart
                }]

startOptions :: LogRevOptions
startOptions = LogRevOptions {
  optVerbose    = False
  , optVersion  = False
  , optHelp     = False
  , inpFile     = "main.log"
  , outFile     = "report"
  , lrsFile     = "main.lrs"
  , geoHdl      = safeGeoDB
}


safeGeoDB :: Maybe GeoDB
safeGeoDB = let
  geo = bringGeoCityDB
  in if geo /= Nothing
        then geo
     else bringGeoCountryDB

progOptions :: [OptDescr (LogRevOptions -> IO LogRevOptions)]
progOptions =
  [ Option "i" ["input"]
    (ReqArg (\ x o -> return o { inpFile = x }) "FILE")
    "input file"
  , Option "o" ["output"]
    (ReqArg (\ x o -> return o { outFile = x }) "FILE")
    "output file"
  , Option "l" ["lrs"]
    (ReqArg (\ x o -> return o { lrsFile = x }) "FILE")
    "log reviser specification"
  , Option "v" ["verbose"]
    (NoArg (\ o -> return o { optVerbose = True }))
    "verbose output"
  , Option "V" ["version"]
    (NoArg (\ _ -> hPutStrLn stderr progVersion
                   >> exitWith ExitSuccess))
    "displays program version"
  , Option "h" ["help"]
    (NoArg (\ _ -> do prg <- getProgName
                      hPutStrLn stderr (usageInfo prg progOptions)
                      >> exitWith ExitSuccess))
    "displays this message"
  ]

logRevMakeStringStat :: LogRevStatsAction -> String
logRevMakeStringStat l = printf "%s:\n%s\n" (aHeader l)
                         $ S.join "\n"
                         $ fmap (\ x -> printf f
                                        x (r M.! x) (s M.! x)
                                        (logRevCntPer l x) (logRevSzPer l x)) m
                         where r = sMap o
                               s = sSz o
                               m = sort $ M.keys r
                               o = aOutput l
                               f = "%10.10s: %10.d %10.d %10.2f %10.2f"

applyAction :: LogRevOptions
               -> LogRevStatsAction
               -> LogLine
               -> LogRevStatsAction
applyAction o a l = a { aOutput = aAction a o (aOutput a) l }

procLogMachine :: LogRevOptions
                  -> [LogRevStatsAction]
                  -> Maybe LogLine
                  -> [LogRevStatsAction]
procLogMachine o m l = if l /= Nothing
                          then fmap (flip (applyAction o) $ fromJust l) m
                       else m

foldLogLines :: [LogRevStatsAction]
                -> LogRevOptions
                -> [String]
                -> [LogRevStatsAction]
foldLogLines [] _ [] = []
foldLogLines ms _ [] = ms
foldLogLines ms o (x : xs) = let
  lm :: Maybe LogLine
  ns :: [LogRevStatsAction]
  lm = x `seq` parseLogLine x
  ns = lm `seq` o `seq` procLogMachine o ms lm
  in foldLogLines ns o xs

procResults :: [LogRevStatsAction] -> LogRevOptions -> IO ()
procResults xs o = putStrLn (S.join "\n" $ fmap logRevMakeStringStat xs)
                   >> mapM_ (D.join (`aPlot` o)) xs

handlerIOError :: IOError -> IO ()
handlerIOError e = putStrLn (printf "IOError: %s" $ show e)
                   >> exitFailure

readLogFile :: [LogRevStatsAction] -> LogRevOptions -> IO ()
readLogFile a o = do
  fh <- openFile (inpFile o) ReadMode
  cont <- hGetContents fh
  procResults (foldLogLines a o $ lines cont) o
  hClose fh

processArgs :: IO ()
processArgs = do
    argv <- getArgs
    let (act, nopt, errs) = getOpt RequireOrder progOptions argv
    opts <- foldl (>>=) (return startOptions) act
    putStrLn $ printf "Processing: %s\n" (inpFile opts)
    readLogFile actionMap opts

main :: IO ()
main = processArgs `catch` handlerIOError

