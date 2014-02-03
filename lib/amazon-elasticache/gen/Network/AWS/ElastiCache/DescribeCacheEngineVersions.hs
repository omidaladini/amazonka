{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.DescribeCacheEngineVersions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeCacheEngineVersions operation returns a list of the available
-- cache engines and their versions.
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=DescribeCacheEngineVersions &MaxRecords=100 &Version=2013-06-15
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-05-23T07%3A34%3A17.435Z &AWSAccessKeyId= &Signature=
-- memcached1.4 memcached memcached version 1.4.14 memcached 1.4.14
-- memcached1.4 memcached memcached version 1.4.5 memcached 1.4.5
-- a6ac9ad2-f8a4-11e1-a4d1-a345e5370093.
module Network.AWS.ElastiCache.DescribeCacheEngineVersions where

import           Control.Monad
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import           Text.XML.Generic

import Network.AWS.ElastiCache.Service
import Network.AWS.ElastiCache.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
describeCacheEngineVersions :: DescribeCacheEngineVersions
describeCacheEngineVersions = DescribeCacheEngineVersions
    { dcevmCacheParameterGroupFamily = Nothing
    , dcevmDefaultOnly = Nothing
    , dcevmEngine = Nothing
    , dcevmEngineVersion = Nothing
    , dcevmMarker = Nothing
    , dcevmMaxRecords = Nothing
    }

data DescribeCacheEngineVersions = DescribeCacheEngineVersions
    { dcevmCacheParameterGroupFamily :: Maybe Text
      -- ^ The name of a specific cache parameter group family to return details for.
      -- Constraints: Must be 1 to 255 alphanumeric characters First character must
      -- be a letter Cannot end with a hyphen or contain two consecutive hyphens.
    , dcevmDefaultOnly :: Maybe Bool
      -- ^ If true, specifies that only the default version of the specified engine or
      -- engine and major version combination is to be returned.
    , dcevmEngine :: Maybe Text
      -- ^ The cache engine to return. Valid values: memcached | redis.
    , dcevmEngineVersion :: Maybe Text
      -- ^ The cache engine version to return. Example: 1.4.14.
    , dcevmMarker :: Maybe Text
      -- ^ An optional marker returned from a prior request. Use this marker for
      -- pagination of results from this operation. If this parameter is specified,
      -- the response includes only records beyond the marker, up to the value
      -- specified by MaxRecords.
    , dcevmMaxRecords :: Maybe Int
      -- ^ The maximum number of records to include in the response. If more records
      -- exist than the specified MaxRecords value, a marker is included in the
      -- response so that the remaining results can be retrieved. Default:
      -- 100Constraints: minimum 20; maximum 100.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeCacheEngineVersions

instance AWSRequest DescribeCacheEngineVersions where
    type Er DescribeCacheEngineVersions = ElastiCacheError
    type Rs DescribeCacheEngineVersions = DescribeCacheEngineVersionsResponse
    request = getQuery service "DescribeCacheEngineVersions"

instance AWSPager DescribeCacheEngineVersions where
    next rq rs
        | Just x <- dcevmrsMarker rs = Just $ rq { dcevmMarker = Just x }
        | otherwise = Nothing

data DescribeCacheEngineVersionsResponse = DescribeCacheEngineVersionsResponse
    { dcevmrsCacheEngineVersions :: [CacheEngineVersion]
      -- ^ A list of cache engine version details. Each element in the list contains
      -- detailed information about once cache engine version.
    , dcevmrsMarker :: Maybe Text
      -- ^ Provides an identifier to allow retrieval of paginated results.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeCacheEngineVersionsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeCacheEngineVersionsResponse"
        :| ["DescribeCacheEngineVersionsResult"]
