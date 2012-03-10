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
import qualified Data.Map as M()
import Data.LogRev.LogStats
import Data.Maybe


coreStatsArgs :: LogRevStats -> LogLine -> (StringIntMap, Int, StringIntMap)
coreStatsArgs s l = (ssm, req, szz)
                    where ssm = seq s $ sMap s
                          req = seq l $ getReqSz l
                          szz = seq s $ sSz s

statsHandlerStatus :: LogRevOptions -> LogRevStats -> LogLine -> LogRevStats
statsHandlerStatus o s l = r
                           where r = s { sMap   = x
                                       , sSz    = z
                                       , sTot   = sTot s + 1
                                       , sSzTot = sSzTot s + getReqSz l
                                       }
                                 x = addIntMapEntry t ssm
                                 z = addPIntMapEntry t req szz
                                 t = seq l $ getStatus l
                                 (ssm, req, szz) = coreStatsArgs s l

statsHandlerCountry :: LogRevOptions -> LogRevStats -> LogLine -> LogRevStats
statsHandlerCountry o s l = r
                            where r = s { sMap   = x
                                        , sSz    = z
                                        , sTot   = sTot s + 1
                                        , sSzTot = sSzTot s + getReqSz l
                                        }
                                  y = geoLookupAddr o i
                                  x = addIntMapEntry y ssm
                                  z = addPIntMapEntry y req szz
                                  i = seq l $ getIP l
                                  (ssm, req, szz) = coreStatsArgs s l

statsHandlerBytes :: LogRevOptions -> LogRevStats -> LogLine -> LogRevStats
statsHandlerBytes o s l = r
                          where r = s { sMap   = x
                                      , sSz    = z
                                      , sTot   = sTot s + 1
                                      , sSzTot = sSzTot s + getReqSz l
                                      }
                                y = seq l $ getBytes l
                                x = addIntMapEntry y ssm
                                z = addPIntMapEntry y req szz
                                (ssm, req, szz) = coreStatsArgs s l

geoLookupAddr :: LogRevOptions -> String -> String
geoLookupAddr o s = G.countryCode3 $! fromJust a
                    where g = fromJust $! geoHdl o
                          a = G.geoLocateByIPAddress g
                              $! B.pack s

getReqSz :: LogLine -> Int
getReqSz l = (read . S.strip) $ getBytes l

