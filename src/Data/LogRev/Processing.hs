----------------------------------------------------------------------------
-- |
-- Module      :  Data.LogRev.Processing
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


module Data.LogRev.Processing (
  statsHandlerStatus
  , statsHandlerCountry
  , statsHandlerBytes
  , geoLookupAddr
  , getReqSz
  ) where


import qualified Data.ByteString.Char8 as B
import qualified Data.GeoIP.GeoDB as G
import qualified Data.String.Utils as S
import qualified Data.Map as M ()
import Data.LogRev.LogStats
import Data.Maybe


coreStatsArgs :: LogRevStats -> LogLine -> (StringIntMap, Int, StringIntMap)
coreStatsArgs s l = ssm `seq` req `seq` szz `seq` (ssm, req, szz)
                    where ssm = sMap s
                          req = getReqSz l
                          szz = sSz s


statsHandlerStatus :: LogRevOptions -> LogRevStats -> LogLine -> LogRevStats
statsHandlerStatus o s l = r
                           where r = s { sMap   = x
                                       , sSz    = z
                                       , sTot   = m
                                       , sSzTot = n
                                       }
                                 x = addIntMapEntry t ssm
                                 z = addPIntMapEntry t req szz
                                 t = B.unpack $ getStatus l
                                 m = sTot s + 1
                                 n = sSzTot s + getReqSz l
                                 (ssm, req, szz) = coreStatsArgs s l


statsHandlerCountry :: LogRevOptions -> LogRevStats -> LogLine -> LogRevStats
statsHandlerCountry o s l = r
                            where r = s { sMap   = x
                                        , sSz    = z
                                        , sTot   = m
                                        , sSzTot = n
                                        }
                                  y = geoLookupAddr o i
                                  x = addIntMapEntry y ssm
                                  z = addPIntMapEntry y req szz
                                  i = B.unpack $ getIP l
                                  m = sTot s + 1
                                  n = sSzTot s + getReqSz l
                                  (ssm, req, szz) = coreStatsArgs s l


statsHandlerBytes :: LogRevOptions -> LogRevStats -> LogLine -> LogRevStats
statsHandlerBytes o s l = r
                          where r = s { sMap   = x
                                      , sSz    = z
                                      , sTot   = m
                                      , sSzTot = m
                                      }
                                y = B.unpack $ getBytes l
                                x = addIntMapEntry y ssm
                                z = addPIntMapEntry y req szz
                                m = sTot s + 1
                                n = sSzTot s + getReqSz l
                                (ssm, req, szz) = coreStatsArgs s l


geoLookupAddr :: LogRevOptions -> String -> String
geoLookupAddr o s = if isNothing $ geoHdl o
                       then "X"
                    else c
  where g = fromJust $ geoHdl o
        a = G.geoLocateByIPAddress g $ B.pack s
        c = G.countryCode3 $ fromJust a


getReqSz :: LogLine -> Int
getReqSz l = let r = (read . S.strip . B.unpack) $ getBytes l
             in r

