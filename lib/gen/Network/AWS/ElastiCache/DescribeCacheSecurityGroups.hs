{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.DescribeCacheSecurityGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeCacheSecurityGroups operation returns a list of cache security
-- group descriptions. If a cache security group name is specified, the list
-- will contain only the description of that group.
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=DescribeCacheSecurityGroups &Version=2013-06-15 &MaxRecords=100
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-07-27T01%3A22%3A48.381Z &AWSAccessKeyId=YOUR-ACCESS-KEY
-- &Signature=YOUR-SIGNATURE default 123456789012 default mycachesecuritygroup
-- 123456789012 My Security Group a95360ae-b7fc-11e0-9326-b7275b9d4a6c.
module Network.AWS.ElastiCache.DescribeCacheSecurityGroups where

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

data DescribeCacheSecurityGroups = DescribeCacheSecurityGroups
    { dcsgoCacheSecurityGroupName :: Maybe Text
      -- ^ The name of the cache security group to return details for.
    , dcsgoMarker :: Maybe Text
      -- ^ An optional marker returned from a prior request. Use this marker for
      -- pagination of results from this operation. If this parameter is specified,
      -- the response includes only records beyond the marker, up to the value
      -- specified by MaxRecords.
    , dcsgoMaxRecords :: Maybe Int
      -- ^ The maximum number of records to include in the response. If more records
      -- exist than the specified MaxRecords value, a marker is included in the
      -- response so that the remaining results can be retrieved. Default:
      -- 100Constraints: minimum 20; maximum 100.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeCacheSecurityGroups

instance AWSRequest DescribeCacheSecurityGroups where
    type Er DescribeCacheSecurityGroups = ElastiCacheError
    type Rs DescribeCacheSecurityGroups = DescribeCacheSecurityGroupsResponse
    request = getQuery service "DescribeCacheSecurityGroups"

instance AWSPager DescribeCacheSecurityGroups where
    next rq rs
        | Just x <- dcsgorsMarker rs = Just $ rq { dcsgoMarker = Just x }
        | otherwise = Nothing

data DescribeCacheSecurityGroupsResponse = DescribeCacheSecurityGroupsResponse
    { dcsgorsCacheSecurityGroups :: [CacheSecurityGroup]
      -- ^ A list of cache security groups. Each element in the list contains detailed
      -- information about one group.
    , dcsgorsMarker :: Maybe Text
      -- ^ Provides an identifier to allow retrieval of paginated results.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeCacheSecurityGroupsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeCacheSecurityGroupsResponse"
        :| ["DescribeCacheSecurityGroupsResult"]
