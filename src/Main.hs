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
import Data.Char
import Data.Colour
import Data.Colour.Names
import Data.Geolocation.GeoIP
import Data.List
import Data.LogRev.LogStats
import Data.Maybe
import Data.String.Utils
import Graphics.LogRev.Charts
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.IO.Unsafe
import System.Posix.Temp
import Text.ParserCombinators.Parsec
import Text.Printf


progVersion :: String
progVersion = "1.0"

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
                , aPlot   = plotPngPieChart
                },
             LogRevStatsAction {
                aHeader = "Bytes"
                , aAction = statsHandlerBytes
                , aOutput = emptyLogRevStats
                , aPlot   = plotPngPieChart
                },
             LogRevStatsAction {
                aHeader = "Country"
                , aAction = statsHandlerCountry
                , aOutput = emptyLogRevStats
                , aPlot   = plotPngPieChart
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

plainValue :: Parser String
plainValue = many1 (noneOf " \n")

bracketedValue :: Parser String
bracketedValue = do
  char '['
  content <- many (noneOf "]")
  char ']'
  return content

quotedValue :: Parser String
quotedValue = do
  char '"'
  content <- many (noneOf "\"")
  char '"'
  return content

dashChar :: Parser String
dashChar = do
  x <- char '-'
  return (show x)

logLine :: Parser LogLine
logLine = do
  vhost <- plainValue
  space
  ip <- plainValue
  space
  ident <- plainValue
  space
  user <- plainValue
  space
  date <- bracketedValue
  space
  req <- quotedValue
  space
  status <- plainValue
  space
  bytes <- plainValue
  space
  ref <- dashChar <|> quotedValue
  space
  ua <- dashChar <|> quotedValue
  return $ LogLine vhost ip ident user date req status bytes ref ua

progOptions :: [OptDescr (LogRevOptions -> IO LogRevOptions)]
progOptions =
  [ Option "m" ["map"] (ReqArg (\x o -> return o { mapFile = x }) "FILE") "map file"
  , Option "i" ["input"] (ReqArg (\x o -> return o { inpFile = x }) "FILE") "input file"
  , Option "o" ["output"] (ReqArg (\x o -> return o { outFile = x }) "FILE") "output file"
  , Option "g" ["geo"] (ReqArg (\x o -> return o { geoFile = x,
                                                   geoHdl = bringGeoDB x }) "FILE") "GeoIP database file"
  , Option "v" ["verbose"] (NoArg (\o -> return o { optVerbose = True })) "verbose output"
  , Option "V" ["version"] (NoArg (\_ -> hPutStrLn stderr "Version 0.01"
                                         >> exitWith ExitSuccess)) "displays program version"
  , Option "h" ["help"] (NoArg (\_ -> do prg <- getProgName
                                         hPutStrLn stderr (usageInfo prg progOptions)
                                         exitWith ExitSuccess)) "displays this message"
  ]

parseLogLine :: String -> Maybe LogLine
parseLogLine s = let r = parse logLine "[Invalid]" s
                         in case r of
                                 Left perr -> Nothing
                                 Right itm -> Just itm

addIntMapEntry :: String -> StringIntMap -> StringIntMap
addIntMapEntry loc m = if M.member loc m
                          then M.insert loc ((m M.! loc) + 1) m
                          else M.insert loc 1 m

addPIntMapEntry :: String
                   -> Int
                   -> StringIntMap
                   -> StringIntMap
addPIntMapEntry loc v m = if M.member loc m
                             then M.insert loc ((m M.! loc) + v) m
                             else M.insert loc 1 m

addFltMapEntry :: String -> StringDoubleMap -> StringDoubleMap
addFltMapEntry loc m = if M.member loc m
                          then M.insert loc ((m M.! loc) + 1.0) m
                          else M.insert loc 1 m

addPFltMapEntry :: String
                   -> Double
                   -> StringDoubleMap
                   -> StringDoubleMap
addPFltMapEntry loc v m = if M.member loc m
                             then M.insert loc ((m M.! loc) + v) m
                             else M.insert loc 1 m

logRevMakeStringStat :: LogRevStatsAction -> String
logRevMakeStringStat l = printf "%s:\n%s\n" (aHeader l)
                         $ join "\n"
                         $ fmap (\x -> printf f
                                       x (r M.! x) (s M.! x)
                                       (logRevCntPer l x) (logRevSzPer l x)) m
                         where r = sMap o
                               s = sSz o
                               m = sort $ M.keys r
                               o = aOutput l
                               f = "%10.10s: %10.d %10.d %10.2f %10.2f"

statsHandlerStatus :: LogRevOptions -> LogRevStats -> LogLine -> LogRevStats
statsHandlerStatus o s l = r
                           where r = s { sMap   = x
                                       , sSz    = z
                                       , sPer   = M.empty
                                       , sTot   = sTot s + 1
                                       , sSzTot = sSzTot s + getReqSz l
                                       }
                                 x = addIntMapEntry t (sMap s)
                                 z = addPIntMapEntry t (getReqSz l) (sSz s)
                                 t = getStatus l

statsHandlerCountry :: LogRevOptions -> LogRevStats -> LogLine -> LogRevStats
statsHandlerCountry o s l = r
                            where r = s { sMap   = x
                                        , sSz    = z
                                        , sPer   = M.empty
                                        , sTot   = sTot s + 1
                                        , sSzTot = sSzTot s + getReqSz l
                                        }
                                  y = geoLookupAddr o (getIP l)
                                  x = addIntMapEntry y (sMap s)
                                  z = addPIntMapEntry y (getReqSz l) (sSz s)

statsHandlerBytes :: LogRevOptions -> LogRevStats -> LogLine -> LogRevStats
statsHandlerBytes o s l = r
                          where r = s { sMap = x
                                      , sSz    = z
                                      , sPer = M.empty
                                      , sTot = sTot s + 1
                                      , sSzTot = sSzTot s + getReqSz l
                                      }
                                y = getBytes l
                                x = addIntMapEntry y (sMap s)
                                z = addPIntMapEntry y (getReqSz l) (sSz s)

geoLookupAddr :: LogRevOptions -> String -> String
geoLookupAddr o s = B.unpack r
                    where g = geoHdl o
                          a = geoLocateByIPAddress g (B.pack s)
                          r = geoCountryCode3 $ fromJust a

getReqSz :: LogLine -> Int
getReqSz l = read $ strip $ getBytes l

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
procLogMachine o m l = fmap (\ x -> applyAction o x l ) m

procLineString :: LogRevOptions
                  -> [LogRevStatsAction]
                  -> String
                  -> [LogRevStatsAction]
procLineString m s x = let r = parseLogLine x
                           in if r == Nothing
                                 then s
                                 else procLogMachine m s
                                      $ fromJust r

processLogFileLoop :: [LogRevStatsAction] -> LogRevOptions -> Handle -> IO ()
processLogFileLoop a o fh = do x <- hIsEOF fh
                               if x
                                  then putStrLn (join "\n" $ fmap logRevMakeStringStat a)
                                       >> mapM_ (\x -> aPlot x o x) a
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
