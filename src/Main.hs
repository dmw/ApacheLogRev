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

import Control.DeepSeq (deepseq)
import Data.GeoIP.GeoDB
import Data.IORef
import Data.List
import Data.LogRev.LogStats
import Data.LogRev.Parser
import Data.LogRev.Processing
import Data.Maybe (fromJust, isJust, isNothing)
import Graphics.LogRev.Charts
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
  geoc = bringGeoCountryDB
  in if isJust geo
        then geo
     else geoc


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
                  -> String
                  -> LogRevStatsMap
procLogMachine o m x = let
  ln = parseLogLine x
  in case ln of
          Left  e -> m
          Right l -> let r = fmap (flip (applyAction o) l) m
                     in r


foldLogLines :: LogRevOptions
                -> LogRevStatsMap
                -> [String]
                -> LogRevStatsMap
foldLogLines mo ms ~ml = foldLogLines' mo ms ml
  where foldLogLines' _ s [] = s
        foldLogLines' o s (x : ~xs) = let
          sm = procLogMachine o s x
          in o `seq` sm `seq` foldLogLines' o sm xs


procResults :: LogRevOptions
               -> LogRevStatsMap
               -> IO ()
procResults o xs = putStrLn report >> mapM_ mkpl mkeys
  where report = S.join "\n" $ fmap mkrss mkeys
        mkeys = M.keys xs
        mkpl x = aPlot (xs M.! x) o (xs M.! x)
        mkrss x = logRevMakeStringStat $ xs M.! x


handlerIOError :: IOError -> IO ()
handlerIOError e = putStrLn (printf "IOError: %s" $ show e)
                   >> exitFailure


defOpts :: LogRevOptions -> LogRevOptions
defOpts lo = fo `deepseq` fo
  where optVerbose_ = lo `deepseq` optVerbose lo
        optVersion_ = lo `deepseq` optVersion lo
        optHelp_    = lo `deepseq` optHelp lo
        inpFile_    = lo `deepseq` inpFile lo
        outFile_    = lo `deepseq` outFile lo
        lrsFile_    = lo `deepseq` lrsFile lo
        geoHdl_     = lo `deepseq` geoHdl lo
        fo          = LogRevOptions {
          optVerbose   = optVerbose_
          , optVersion = optVersion_
          , optHelp    = optHelp_
          , inpFile    = inpFile_
          , outFile    = outFile_
          , lrsFile    = lrsFile_
          , geoHdl     = geoHdl_
          }


processArgs :: IO ()
processArgs = do
    argv <- getArgs
    let (act, nopt, errs) = getOpt RequireOrder progOptions argv
    opts <- foldl (>>=) (return startOptions) act
    putStrLn $ printf "Processing: %s\n" (inpFile opts)
    inpData <- readFile (inpFile opts)
    procResults opts $ foldLogLines opts actionMap $ lines inpData


main :: IO ()
main = processArgs `catch` handlerIOError
