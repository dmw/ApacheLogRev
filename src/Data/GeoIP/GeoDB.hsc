{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}


module Data.GeoIP.GeoDB (
  -- * Types
  GeoDB
  , GeoIPOption(..)
  , GeoIPDBTypes(..)
  , GeoIPRec(..)
  , geoCountryDB
    -- * Data Operations
  , combineOptions
  , availableGeoDB
  , bringGeoDB
  , bringGeoCityDB
  , bringGeoCountryDB
  , geoLocateByIPAddress
  , mkIpNum
  ) where


import Control.DeepSeq
import Control.Applicative

import Data.ByteString.Char8 (ByteString,
                              packCString,
                              split,
                              unpack)

import Data.Maybe

import Foreign.C.String()
import Foreign.C.Types
import Foreign.Ptr
import Foreign


#include "GeoIP.h"
#include "GeoIPCity.h"


data GeoIP

------------------------------------------------------------------------------
-- | Type representing an established connection to a GeoIPCity database
newtype GeoDB = GeoDB {
  unGeoDB :: ForeignPtr GeoIP
  } deriving (Eq, Show)


------------------------------------------------------------------------------
-- | Return data for a geolocation lookup (this should not be used)
data GeoIPRecord = GeoIPRecord
  { geoCountryCode    :: !ByteString
  , geoCountryCode3   :: !ByteString
  , geoCountryName    :: !ByteString
  , geoRegion         :: !ByteString
  , geoCity           :: !ByteString
  , geoPostalCode     :: !ByteString
  , geoLatitude       :: !Double
  , geoLongitude      :: !Double
  , geoAreaCode       :: !Int
  , geoContinentCode  :: !ByteString
  , geoAccuracyRadius :: !Int
  } deriving (Eq, Show)

------------------------------------------------------------------------------
-- | Return data for a geolocation lookup (this entry is public)
data GeoIPRec = GeoIPRec
  { countryCode       :: !String
  , countryCode3      :: !String
  , countryName       :: !String
  , region            :: !String
  , city              :: !String
  , postalCode        :: !String
  , latitude          :: !Double
  , longitude         :: !Double
  , areaCode          :: !Int
  , continentCode     :: !String
  , accuracyRadius    :: !Int
  } deriving (Eq, Show)


instance NFData GeoIPRecord where
  rnf a = a `seq` ()


peekGeoIPRecord :: Ptr GeoIPRecord -> IO (Maybe GeoIPRecord)
peekGeoIPRecord p =
  case nullPtr == p of
    True -> return Nothing
    False -> fmap Just r
  where
    !r = GeoIPRecord
          <$> peekBS (#{peek GeoIPRecord, country_code})
          <*> peekBS (#{peek GeoIPRecord, country_code3})
          <*> peekBS (#{peek GeoIPRecord, country_name})
          <*> peekBS (#{peek GeoIPRecord, region})
          <*> peekBS (#{peek GeoIPRecord, city})
          <*> peekBS (#{peek GeoIPRecord, postal_code})
          <*> fmap tofloat (#{peek GeoIPRecord, latitude} p)
          <*> fmap tofloat (#{peek GeoIPRecord, longitude} p)
          <*> fmap toInt (#{peek GeoIPRecord, area_code} p)
          <*> peekBS (#{peek GeoIPRecord, continent_code})
          <*> fmap toInt (#{peek GeoIPRecord, accuracy_radius} p)
    peekBS f = do
      !sptr <- f p
      case nullPtr == sptr of
        True -> return ""
        False -> let x = packCString sptr in x `seq` x
    tofloat :: CFloat -> Double
    tofloat = realToFrac
    toInt :: CInt -> Int
    toInt = fromIntegral


newtype GeoIPOption = GeoIPOption {
  unGeoIPOpt     :: CInt
  } deriving (Eq, Show)

newtype GeoIPDBTypes = GeoIPDBTypes {
  unGeoIPDBType  :: CInt
  } deriving (Eq, Show)

#{enum GeoIPOption, GeoIPOption
 , geoip_standard                             = GEOIP_STANDARD
 , geoip_memory_cache                         = GEOIP_MEMORY_CACHE
 , geoip_check_cache                          = GEOIP_CHECK_CACHE
 , geoip_index_cache                          = GEOIP_INDEX_CACHE
 , geoip_mmap_cache                           = GEOIP_MMAP_CACHE
 }

#{enum GeoIPDBTypes, GeoIPDBTypes
 , geoip_country_edition                      = GEOIP_COUNTRY_EDITION
 , geoip_region_edition_rev0                  = GEOIP_REGION_EDITION_REV0
 , geoip_city_edition_rev0                    = GEOIP_CITY_EDITION_REV0
 , geoip_org_edition                          = GEOIP_ORG_EDITION
 , geoip_isp_edition                          = GEOIP_ISP_EDITION
 , geoip_city_edition_rev1                    = GEOIP_CITY_EDITION_REV1
 , geoip_region_edition_rev1                  = GEOIP_REGION_EDITION_REV1
 , geoip_proxy_edition                        = GEOIP_PROXY_EDITION
 , geoip_asnum_edition                        = GEOIP_ASNUM_EDITION
 , geoip_netspeed_edition                     = GEOIP_NETSPEED_EDITION
 , geoip_domain_edition                       = GEOIP_DOMAIN_EDITION
 , geoip_country_edition_v6                   = GEOIP_COUNTRY_EDITION_V6
 , geoip_locationa_edition                    = GEOIP_LOCATIONA_EDITION
 , geoip_accuracyradius_edition               = GEOIP_ACCURACYRADIUS_EDITION
 , geoip_cityconfidence_edition               = GEOIP_CITYCONFIDENCE_EDITION
 , geoip_cityconfidencedist_edition           = GEOIP_CITYCONFIDENCEDIST_EDITION
 , geoip_large_country_edition                = GEOIP_LARGE_COUNTRY_EDITION
 , geoip_large_country_edition_v6             = GEOIP_LARGE_COUNTRY_EDITION_V6
 , geoip_cityconfidencedist_isp_org_edition   = GEOIP_CITYCONFIDENCEDIST_ISP_ORG_EDITION
 , geoip_ccm_country_edition                  = GEOIP_CCM_COUNTRY_EDITION
 , geoip_asnum_edition_v6                     = GEOIP_ASNUM_EDITION_V6
 , geoip_isp_edition_v6                       = GEOIP_ISP_EDITION_V6
 , geoip_org_edition_v6                       = GEOIP_ORG_EDITION_V6
 , geoip_domain_edition_v6                    = GEOIP_DOMAIN_EDITION_V6
 , geoip_locationa_edition_v6                 = GEOIP_LOCATIONA_EDITION_V6
 , geoip_registrar_edition                    = GEOIP_REGISTRAR_EDITION
 , geoip_registrar_edition_v6                 = GEOIP_REGISTRAR_EDITION_V6
 , geoip_usertype_edition                     = GEOIP_USERTYPE_EDITION
 , geoip_usertype_edition_v6                  = GEOIP_USERTYPE_EDITION_V6
 , geoip_city_edition_rev1_v6                 = GEOIP_CITY_EDITION_REV1_V6
 , geoip_city_edition_rev0_v6                 = GEOIP_CITY_EDITION_REV0_V6
 , geoip_netspeed_edition_rev1                = GEOIP_NETSPEED_EDITION_REV1
 , geoip_netspeed_edition_rev1_v6             = GEOIP_NETSPEED_EDITION_REV1_V6
 , geoip_notfound_database                    = 0
 }


geoCountryDB :: [GeoIPDBTypes]
geoCountryDB = [geoip_city_edition_rev1_v6
               , geoip_city_edition_rev0_v6
               , geoip_city_edition_rev1
               , geoip_city_edition_rev0
               , geoip_country_edition_v6
               , geoip_country_edition]


geoCityDB :: [GeoIPDBTypes]
geoCityDB = [geoip_city_edition_rev1_v6
            , geoip_city_edition_rev0_v6
            , geoip_city_edition_rev1
            , geoip_city_edition_rev0]


------------------------------------------------------------------------------
-- | Collapse & combine multiple 'GeoIPOption's into one
combineOptions :: [GeoIPOption] -> GeoIPOption
combineOptions = GeoIPOption . foldr ((.|.) . unGeoIPOpt) 0


------------------------------------------------------------------------------
-- | Standard options for common operating systems
geoDBMainOptions :: GeoIPOption
geoDBMainOptions = combineOptions [geoip_standard
                                  , geoip_memory_cache
                                  , geoip_mmap_cache]


------------------------------------------------------------------------------
-- | Returns the first available database for the system wide Country Editions
bringGeoCountryDB :: Maybe GeoDB
bringGeoCountryDB = bringGeoDB geoCountryDB

------------------------------------------------------------------------------
-- | Returns the first available database for the system wide Country Editions
bringGeoCityDB :: Maybe GeoDB
bringGeoCityDB = bringGeoDB geoCityDB

------------------------------------------------------------------------------
-- | Returns the first available database for the given list of types.

bringGeoDB :: [GeoIPDBTypes] -> Maybe GeoDB
bringGeoDB [] = Nothing
bringGeoDB (x:xs) = if availableGeoDB x
                       then Just
                            $! unsafePerformIO
                            $! openGeoDB x geoDBMainOptions
                       else bringGeoDB xs

------------------------------------------------------------------------------
-- Utils


------------------------------------------------------------------------------
-- | Convert a string IP adress to IPNum
mkIpNum :: ByteString -> Maybe Integer
mkIpNum x = case valid of
  False -> Nothing
  True -> Just $! a * 16777216 + b * 65536 + 256 * c + d
  where
    valid = length parts == 4 && foldr (\r acc -> acc && r <= 255) True [a,b,c,d]
    a : b : c : d : _ = map (read . unpack) parts
    parts = split '.' x


------------------------------------------------------------------------------
-- Higher level GeoIP ops
--


------------------------------------------------------------------------------
-- | Checks if the given GeoDB database type is available on the System.
--
-- This would check a system wide database for the City Database Rev0
--
-- > availableGeoDB geoip_city_edition_rev0
--
availableGeoDB :: GeoIPDBTypes -> Bool
availableGeoDB t = let x = c_GeoIP_db_avail t
                       in if x == 0
                             then False
                          else True


------------------------------------------------------------------------------
-- | Open a system wide GeoIP database based on its type (GeoIPDBTypes)
--
-- This would open a system wide database for the City Database Rev0
--
-- > openGeoDB geoip_city_edition_rev0
--
openGeoDB :: GeoIPDBTypes -> GeoIPOption -> IO GeoDB
openGeoDB t o = do x <- c_GeoIP_open_type t o
                   GeoDB <$> newForeignPtr c_GeoIP_delete x


------------------------------------------------------------------------------
-- | Geo-locate by given IP Adress
--
-- > geoLocateByIPAddress db "123.123.123.123"
geoLocateByIPAddress :: GeoDB -> ByteString -> Maybe GeoIPRec
geoLocateByIPAddress db ip = seq r $! toIPRec r
                             where r = mkIpNum ip >>= geoLocateByIPNum db


------------------------------------------------------------------------------
-- | Converts a GeoIPRecord entry to GeoIPRec entry.
--
--
-- > toIPRec r
toIPRec :: Maybe GeoIPRecord -> Maybe GeoIPRec
toIPRec r = case r of
                 Nothing -> Nothing
                 _       -> nr
                            where nr = Just $ GeoIPRec {
                                    countryCode = unpack (geoCountryCode jr)
                                    , countryCode3 = unpack (geoCountryCode3 jr)
                                    , countryName = unpack (geoCountryName jr)
                                    , region = unpack (geoRegion jr)
                                    , city = unpack (geoCity jr)
                                    , postalCode = unpack (geoPostalCode jr)
                                    , latitude = geoLatitude jr
                                    , longitude = geoLongitude jr
                                    , areaCode = geoAreaCode jr
                                    , continentCode = unpack (geoContinentCode jr)
                                    , accuracyRadius = geoAccuracyRadius jr
                                    }
                                  jr = fromJust r

------------------------------------------------------------------------------
-- | Geo-locate by given IP number. Call 'mkIpNum' on a 'String' ip address to
-- convert to IP number.
--
-- > geoLocateByIPNum db 12336939327338
geoLocateByIPNum :: GeoDB -> Integer -> Maybe GeoIPRecord
geoLocateByIPNum (GeoDB db) ip = unsafePerformIO $! do
  withForeignPtr db $! \db' -> do
    ptr <- c_GeoIP_record_by_ipnum db' (fromIntegral ip)
    rec <- peekGeoIPRecord ptr
    return $! rec `deepseq` ()
    case ptr == nullPtr of
      True -> return ()
      False -> c_GeoIPRecord_delete ptr
    return rec


------------------------------------------------------------------------------
-- Low level calls into the C library

foreign import ccall safe "GeoIP.h GeoIP_new"
  c_GeoIP_new :: GeoIPOption -> IO (Ptr GeoIP)

foreign import ccall safe "GeoIP.h &GeoIP_delete"
  c_GeoIP_delete :: FunPtr (Ptr GeoIP -> IO ())

foreign import ccall safe "GeoIPCity.h GeoIP_record_by_ipnum"
  c_GeoIP_record_by_ipnum :: Ptr GeoIP -> CULong -> IO (Ptr GeoIPRecord)

foreign import ccall safe "GeoIPCity.h GeoIPRecord_delete"
  c_GeoIPRecord_delete :: Ptr GeoIPRecord -> IO ()

foreign import ccall safe "GeoIPCity.h GeoIP_db_avail"
  c_GeoIP_db_avail :: GeoIPDBTypes -> CInt

foreign import ccall safe "GeoIPCity.h GeoIP_open_type"
  c_GeoIP_open_type :: GeoIPDBTypes -> GeoIPOption -> IO (Ptr GeoIP)

