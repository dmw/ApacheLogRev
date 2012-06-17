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


import qualified Data.Map as M
import qualified Data.String.Utils as S (join)

import Control.DeepSeq ()
import Data.GeoIP.GeoDB
import Data.List
import Data.LogRev.LogStats
import Data.LogRev.Parser
import Data.LogRev.Processing
import Graphics.LogRev.Charts
import Data.Maybe (fromJust, isJust)
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.IO.Unsafe ()
import System.Posix.Temp ()
import Text.Printf


progVersion :: String
progVersion = "ApacheLogRev 0.0.1"


emptyLogRevStats :: LogRevStats
emptyLogRevStats = LogRevStats {
  sTot   = 0
  , sSzTot = 0
  , sSz    = M.empty
  , sMap   = M.empty
  , sPer   = M.empty
  }


actionMap :: LogRevStatsMap
actionMap = M.fromList [
  (
    "http_status"
    , LogRevStatsAction
      {
        aHeader   = "HTTP Status"
      , aAction = statsHandlerStatus
      , aOutput = emptyLogRevStats
      , aPlot   = plotPngBarChart
      }
  )
  ,
  (
    "connections"
    , LogRevStatsAction
      {
        aHeader   = "Connections From Countries"
      , aAction = statsHandlerCountry
      , aOutput = emptyLogRevStats
      , aPlot   = plotPngBarChart
      }
  )
  ]


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
  in if isJust geo
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
                   >> exitSuccess))
    "displays program version"
  , Option "h" ["help"]
    (NoArg (\ _ -> do
                   prg <- getProgName
                   hPutStrLn stderr (usageInfo prg progOptions)
                   >> exitSuccess))
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
                  -> LogRevStatsMap
                  -> Maybe LogLine
                  -> LogRevStatsMap
procLogMachine o ~m l = if isJust l
                           then m `seq` fmap (flip (applyAction o)
                                              $ fromJust l) m
                        else m


foldLogLines :: Handle
                -> LogRevStatsMap
                -> LogRevOptions
                -> IO ()
foldLogLines h ~ms ~o = let foldLogLines' hs ~rs ~os = do
                              e <- hIsEOF hs
                              if e
                                then rs `seq` os `seq` procResults rs os
                                else do x <- hGetLine hs
                                        foldLogLines' hs (procLogMachine os rs
                                                          $ parseLogLine x) os
                        in foldLogLines' h ms o


procResults :: LogRevStatsMap -> LogRevOptions -> IO ()
procResults xs o = putStrLn report >> mapM_ mkpl mkeys
                   where report = S.join "\n" $ fmap mkrss mkeys
                         mkeys = M.keys xs
                         mkpl x = aPlot (xs M.! x) o (xs M.! x)
                         mkrss x = logRevMakeStringStat $ xs M.! x


handlerIOError :: IOError -> IO ()
handlerIOError e = putStrLn (printf "IOError: %s" $ show e)
                   >> exitFailure


processArgs :: IO ()
processArgs = do
    argv <- getArgs
    let (act, nopt, errs) = getOpt RequireOrder progOptions argv
    opts <- foldl (>>=) (return startOptions) act
    hndl <- openFile (inpFile opts) ReadMode
    putStrLn $ printf "Processing: %s\n" (inpFile opts)
    foldLogLines hndl actionMap opts


main :: IO ()
main = processArgs `catch` handlerIOError
