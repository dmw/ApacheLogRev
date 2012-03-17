----------------------------------------------------------------------------
-- |
-- Module      :  Data.LogRev.LogStats
-- Copyright   :  (c) Daniel Molina Wegener 2012
-- License     :  BSD 3 (see the LICENSE file)
-- Author      :  Daniel Molina Wegener <dmw@coder.cl>
-- Homepage    :  http://coder.cl/products/logrev/
-- Repository  :  https://github.com/dmw/ApacheLogRev
--
-- An Apache Access Log Statistics extractor modules
--
-- This is the initial commit of this project, please write
-- me directly if you want to contribute.
-----------------------------------------------------------------------------


module Data.LogRev.LogStats (
  StringIntMap
  , StringDoubleMap
  , LogRevStatsCol
  , LogLine (..)
  , LogRevOptions (..)
  , LogRevStats (..)
  , LogRevStatsAction (..)
  , addIntMapEntry
  , addPIntMapEntry
  , addFltMapEntry
  , addPFltMapEntry
  ) where


import qualified Data.Map as M
import qualified Data.GeoIP.GeoDB as G

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk()
import Text.Printf


type StringIntMap = M.Map String Int

type StringDoubleMap = M.Map String Double

type LogRevStatsCol = M.Map String LogRevStats

data LogLine = LogLine {
  getVhost      :: String
  , getIP       :: String
  , getIdent    :: String
  , getUser     :: String
  , getDate     :: String
  , getReq      :: String
  , getStatus   :: String
  , getBytes    :: String
  , getRef      :: String
  , getUA       :: String
} deriving (Show, Eq)

data LogRevOptions = LogRevOptions {
  optVerbose    :: Bool
  , optVersion  :: Bool
  , optHelp     :: Bool
  , inpFile     :: String
  , outFile     :: String
  , lrsFile     :: String
  , geoHdl      :: Maybe G.GeoDB
}

data LogRevStats = LogRevStats {
  sTot          :: Int
  , sSzTot      :: Int
  , sSz         :: StringIntMap
  , sMap        :: StringIntMap
  , sPer        :: StringDoubleMap
}

data LogRevStatsAction = LogRevStatsAction {
  aHeader       :: String
  , aAction     :: LogRevOptions
                   -> LogRevStats
                   -> LogLine
                   -> LogRevStats
  , aPlot       :: LogRevOptions
                   -> LogRevStatsAction
                   -> IO (PickFn ())
  , aOutput     :: LogRevStats
}

instance Show LogRevStats where
  show a = printf " %d " tot
           where tot = sTot a

instance Show LogRevStatsAction where
  show a = printf " (%s:%s) " hdr out
           where hdr = aHeader a
                 out = show $! aOutput a

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
