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


import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import qualified Data.String.Utils as S
import qualified Control.Monad as D
import Data.Char
import Data.Colour
import Data.Colour.Names
import Data.Geolocation.GeoIP
import Data.List
import Data.LogRev.LogStats
import Data.LogRev.Parser
import Data.LogRev.Processing
import Data.Maybe
import Graphics.LogRev.Charts
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.IO.Unsafe
import System.Posix.Temp
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
                aHeader   = "Status"
                , aAction = statsHandlerStatus
                , aOutput = emptyLogRevStats
                , aPlot   = plotPngBarChart
                },
             LogRevStatsAction {
                aHeader = "Country"
                , aAction = statsHandlerCountry
                , aOutput = emptyLogRevStats
                , aPlot   = plotPngBarChart
                }]

startOptions :: LogRevOptions
startOptions = LogRevOptions {
  optVerbose    = False
  , optVersion  = False
  , optHelp     = False
  , geoFile     = "/usr/share/GeoIP/GeoLiteCity.dat"
  , mapFile     = "/usr/share/doc/gnuplot-doc/examples/world.dat"
  , inpFile     = "main.log"
  , outFile     = "report.png"
  , geoHdl      = bringGeoDB "/usr/share/GeoIP/GeoLiteCity.dat"
}

bringGeoDB :: String -> GeoDB
bringGeoDB x = unsafePerformIO $ openGeoDB memory_cache x

progOptions :: [OptDescr (LogRevOptions -> IO LogRevOptions)]
progOptions =
  [ Option "m" ["map"] (ReqArg (\x o -> return o { mapFile = x }) "FILE") "map file"
  , Option "i" ["input"] (ReqArg (\x o -> return o { inpFile = x }) "FILE") "input file"
  , Option "o" ["output"] (ReqArg (\x o -> return o { outFile = x }) "FILE") "output file"
  , Option "g" ["geo"] (ReqArg (\x o -> return o { geoFile = x,
                                                   geoHdl = bringGeoDB x }) "FILE") "GeoIP database file"
  , Option "v" ["verbose"] (NoArg (\o -> return o { optVerbose = True })) "verbose output"
  , Option "V" ["version"] (NoArg (\_ -> hPutStrLn stderr progVersion
                                         >> exitWith ExitSuccess)) "displays program version"
  , Option "h" ["help"] (NoArg (\_ -> do prg <- getProgName
                                         hPutStrLn stderr (usageInfo prg progOptions)
                                         exitWith ExitSuccess)) "displays this message"
  ]

logRevMakeStringStat :: LogRevStatsAction -> String
logRevMakeStringStat l = printf "%s:\n%s\n" (aHeader l)
                         $ S.join "\n"
                         $ fmap (\x -> printf f
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
applyAction o a l = let action = aAction a
                        output = aOutput a
                        in a { aOutput = action o output l }

procLogMachine :: LogRevOptions
                  -> [LogRevStatsAction]
                  -> LogLine
                  -> [LogRevStatsAction]
procLogMachine o m l = fmap (flip (applyAction o) l) m

procLineString :: LogRevOptions
                  -> [LogRevStatsAction]
                  -> String
                  -> [LogRevStatsAction]
procLineString m s x = let r = parseLogLine x
                           in if r /= Nothing
                                 then procLogMachine m s (fromJust r)
                                 else s

processLogFileLoop :: [LogRevStatsAction] -> LogRevOptions -> Handle -> IO ()
processLogFileLoop a o fh = do x <- hIsEOF fh
                               if x
                                  then putStrLn (S.join "\n" $ fmap logRevMakeStringStat a)
                                       >> mapM_ (D.join (flip aPlot o)) a
                                  else do ins <- hGetLine fh
                                          processLogFileLoop (procLineString o a ins) o fh

readLogFile :: [LogRevStatsAction] -> LogRevOptions -> IO ()
readLogFile a o = do fh <- openFile (inpFile o) ReadMode
                     processLogFileLoop a o fh
                     hClose fh

main :: IO ()
main = do
    argv <- getArgs
    let (act, nopt, errs) = getOpt RequireOrder progOptions argv
    opts <- foldl (>>=) (return startOptions) act
    putStrLn $ printf "Processing: %s\n" (inpFile opts)
    readLogFile actionMap opts
