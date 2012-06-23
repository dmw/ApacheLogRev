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
  , LogRevStatsMap
  , LogLine (..)
  , LogError (..)
  , LogRevOptions (..)
  , LogRevStats (..)
  , LogRevStatsAction (..)
  , addIntMapEntry
  , addPIntMapEntry
  , addFltMapEntry
  , addPFltMapEntry
  ) where


import Control.DeepSeq

import qualified Data.ByteString.Char8 as S
import qualified Data.Map as M
import qualified Data.GeoIP.GeoDB as G

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk()
import Text.Printf


type StringIntMap = M.Map String Int

type StringDoubleMap = M.Map String Double

type LogRevStatsCol = M.Map String LogRevStats

type LogRevStatsMap = M.Map String LogRevStatsAction


data LogLine = LogLine {
  getVhost      :: S.ByteString
  , getIP       :: S.ByteString
  , getIdent    :: S.ByteString
  , getUser     :: S.ByteString
  , getDate     :: S.ByteString
  , getReq      :: S.ByteString
  , getStatus   :: S.ByteString
  , getBytes    :: S.ByteString
  , getRef      :: S.ByteString
  , getUA       :: S.ByteString
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


data LogError = LogError {
  err           :: String
  , group       :: [String]
} deriving (Show)


data LogRevStatsAction = LogRevStatsAction {
  aHeader       :: String
  , aAction     :: LogRevOptions -> LogRevStats -> LogLine -> LogRevStats
  , aPlot       :: LogRevOptions -> LogRevStatsAction -> IO (PickFn ())
  , aOutput     :: LogRevStats
}


instance Show LogRevStats where
  show a = printf " %d " tot
           where tot = sTot a


instance Show LogRevStatsAction where
  show a = printf " (%s:%s) " hdr out
           where hdr = aHeader a
                 out = show $! aOutput a


instance NFData LogLine where
  rnf a = a `seq` ()


instance NFData LogRevStatsAction where
  rnf a = a `seq` ()


instance NFData LogRevOptions where
  rnf a = a `seq` ()


instance NFData LogError where
  rnf a = a `seq` ()



addIntMapEntry :: String
                  -> StringIntMap
                  -> StringIntMap
addIntMapEntry loc m = res `seq` res
  where res = if M.member loc m
                 then let r = M.insert loc ((m M.! loc) + 1) m in r
              else let r = M.insert loc 1 m in r


addPIntMapEntry :: String
                   -> Int
                   -> StringIntMap
                   -> StringIntMap
addPIntMapEntry loc v m = res `seq` res
  where res = if M.member loc m
                 then let r = M.insert loc ((m M.! loc) + v) m in r
              else let r = M.insert loc 1 m in r


addFltMapEntry :: String -> StringDoubleMap -> StringDoubleMap
addFltMapEntry loc m = if M.member loc m
                          then let r = M.insert loc ((m M.! loc) + 1.0) m in r
                       else let r = M.insert loc 1 m in r


addPFltMapEntry :: String
                   -> Double
                   -> StringDoubleMap
                   -> StringDoubleMap
addPFltMapEntry loc v m = if M.member loc m
                             then let r = M.insert loc ((m M.! loc) + v) m in r
                          else let r = M.insert loc 1 m in r

