----------------------------------------------------------------------------
-- |
-- Module      :  Data.LogRev
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
  StringIntMap(..)
  , StringDoubleMap(..)
  , LogRevStatsCol(..)
  , LogLine(..)
  , LogRevOptions(..)
  , LogRevStats(..)
  , LogRevStatsAction(..)
  ) where


import qualified Data.Map as M
import Data.Geolocation.GeoIP
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
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
} deriving (Ord, Show, Eq)

data LogRevOptions = LogRevOptions {
  optVerbose    :: Bool
  , optVersion  :: Bool
  , optHelp     :: Bool
  , mapFile     :: String
  , geoFile     :: String
  , inpFile     :: String
  , outFile     :: String
  , geoHdl      :: GeoDB
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
  show a = printf " %d " (sTot a)

instance Show LogRevStatsAction where
  show a = printf " (%s:%s) " (aHeader a) (show (aOutput a))

