{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.DescribeCacheParameters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeCacheParameters operation returns the detailed parameter list
-- for a particular cache parameter group. Some of the output has been omitted
-- for brevity. https://elasticache.us-east-1.amazonaws.com/
-- ?Action=DescribeCacheParameters
-- &CacheParameterGroupName=default.memcached1.4 &MaxRecords=100
-- &Version=2013-06-15 &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-07-27T03%3A07%3A23.039Z &AWSAccessKeyId=YOUR-ACCESS-KEY
-- &Signature=YOUR-SIGNATURE 1-07-15/"> cache.c1.xlarge 6000 (...output
-- omitted...) integer system false The maximum configurable amount of memory
-- to use to store items, in megabytes. 1-100000 max_cache_memory 1.4.5
-- (...output omitted...) 1024 integer system false The backlog queue limit.
-- 1-10000 backlog_queue_limit 1.4.5 (...output omitted...)
-- 0c507368-b7fe-11e0-9326-b7275b9d4a6c.
module Network.AWS.ElastiCache.DescribeCacheParameters where

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

data DescribeCacheParameters = DescribeCacheParameters
    { dcpmCacheParameterGroupName :: !Text
      -- ^ The name of a specific cache parameter group to return details for.
    , dcpmMarker :: Maybe Text
      -- ^ An optional marker returned from a prior request. Use this marker for
      -- pagination of results from this operation. If this parameter is specified,
      -- the response includes only records beyond the marker, up to the value
      -- specified by MaxRecords.
    , dcpmMaxRecords :: Maybe Int
      -- ^ The maximum number of records to include in the response. If more records
      -- exist than the specified MaxRecords value, a marker is included in the
      -- response so that the remaining results can be retrieved. Default:
      -- 100Constraints: minimum 20; maximum 100.
    , dcpmSource :: Maybe Text
      -- ^ The parameter types to return. Valid values: user | system |
      -- engine-default.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeCacheParameters

instance AWSRequest DescribeCacheParameters where
    type Er DescribeCacheParameters = ElastiCacheError
    type Rs DescribeCacheParameters = DescribeCacheParametersResponse
    request = getQuery service "DescribeCacheParameters"

instance AWSPager DescribeCacheParameters where
    next rq rs
        | Just x <- dcpmrsMarker rs = Just $ rq { dcpmMarker = Just x }
        | otherwise = Nothing

data DescribeCacheParametersResponse = DescribeCacheParametersResponse
    { dcpmrsCacheNodeTypeSpecificParameters :: [CacheNodeTypeSpecificParameter]
      -- ^ A list of parameters specific to a particular cache node type. Each element
      -- in the list contains detailed information about one parameter.
    , dcpmrsMarker :: Maybe Text
      -- ^ Provides an identifier to allow retrieval of paginated results.
    , dcpmrsParameters :: [Parameter]
      -- ^ A list of Parameter instances.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeCacheParametersResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeCacheParametersResponse"
        :| ["DescribeCacheParametersResult"]
