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
import qualified Data.Map as M
import Data.LogRev.LogStats
import Data.Maybe


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
                    where g = fromJust $ geoHdl o
                          a = G.geoLocateByIPAddress g (B.pack s)
                          r = G.geoCountryCode3 $ fromJust a

getReqSz :: LogLine -> Int
getReqSz l = read $ S.strip $ getBytes l




