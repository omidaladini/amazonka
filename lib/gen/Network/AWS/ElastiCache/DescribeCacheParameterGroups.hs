{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.DescribeCacheParameterGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeCacheParameterGroups operation returns a list of cache
-- parameter group descriptions. If a cache parameter group name is specified,
-- the list will contain only the descriptions for that group.
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=DescribeCacheParameterGroups &MaxRecords=100 &Version=2013-06-15
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-07-27T01%3A34%3A31.045Z &AWSAccessKeyId=YOUR-ACCESS-KEY
-- &Signature=YOUR-SIGNATURE default.memcached1.4 memcached1.4 Default
-- parameter group for memcached1.4 mycacheparametergroup memcached1.4 My
-- cache parameter group mycacheparametergroup1 memcached1.4 My first cache
-- parameter group mycacheparametergroup3 memcached1.4 My first cache
-- parameter group 7193fbb8-b7fc-11e0-9b0b-a9261be2b354.
module Network.AWS.ElastiCache.DescribeCacheParameterGroups where

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

-- | Convenience method utilising default fields where applicable.
describeCacheParameterGroups :: AWS (Either ElastiCacheError DescribeCacheParameterGroupsResponse)
describeCacheParameterGroups = undefined $ DescribeCacheParameterGroups
    { dcpgmCacheParameterGroupName = Nothing
    , dcpgmMarker = Nothing
    , dcpgmMaxRecords = Nothing
    }

data DescribeCacheParameterGroups = DescribeCacheParameterGroups
    { dcpgmCacheParameterGroupName :: Maybe Text
      -- ^ The name of a specific cache parameter group to return details for.
    , dcpgmMarker :: Maybe Text
      -- ^ An optional marker returned from a prior request. Use this marker for
      -- pagination of results from this operation. If this parameter is specified,
      -- the response includes only records beyond the marker, up to the value
      -- specified by MaxRecords.
    , dcpgmMaxRecords :: Maybe Int
      -- ^ The maximum number of records to include in the response. If more records
      -- exist than the specified MaxRecords value, a marker is included in the
      -- response so that the remaining results can be retrieved. Default:
      -- 100Constraints: minimum 20; maximum 100.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeCacheParameterGroups

instance AWSRequest DescribeCacheParameterGroups where
    type Er DescribeCacheParameterGroups = ElastiCacheError
    type Rs DescribeCacheParameterGroups = DescribeCacheParameterGroupsResponse
    request = getQuery service "DescribeCacheParameterGroups"

instance AWSPager DescribeCacheParameterGroups where
    next rq rs
        | Just x <- dcpgmrsMarker rs = Just $ rq { dcpgmMarker = Just x }
        | otherwise = Nothing

data DescribeCacheParameterGroupsResponse = DescribeCacheParameterGroupsResponse
    { dcpgmrsCacheParameterGroups :: [CacheParameterGroup]
      -- ^ A list of cache parameter groups. Each element in the list contains
      -- detailed information about one cache parameter group.
    , dcpgmrsMarker :: Maybe Text
      -- ^ Provides an identifier to allow retrieval of paginated results.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeCacheParameterGroupsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeCacheParameterGroupsResponse"
        :| ["DescribeCacheParameterGroupsResult"]
