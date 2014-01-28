{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.DescribeCacheSubnetGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeCacheSubnetGroups operation returns a list of cache subnet
-- group descriptions. If a subnet group name is specified, the list will
-- contain only the description of that group. Some of the output has been
-- omitted for brevity. https://elasticache.amazonaws.com/
-- ?Action=DescribeCacheSubnetGroups &Version=2013-06-15 &MaxRecords=100
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-02-15T19%3A40%3A19.926Z &AWSAccessKeyId= &Signature=
-- 990524496922 description subnet_grp1 Active subnet-7c5b4115 us-east-1c
-- Active subnet-7b5b4112 us-east-1b Active subnet-3ea6bd57 us-east-1d
-- (...output omitted...) 31d0faee-229b-11e1-81f1-df3a2a803dad.
module Network.AWS.ElastiCache.DescribeCacheSubnetGroups where

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

data DescribeCacheSubnetGroups = DescribeCacheSubnetGroups
    { dcsgnCacheSubnetGroupName :: Maybe Text
      -- ^ The name of the cache subnet group to return details for.
    , dcsgnMarker :: Maybe Text
      -- ^ An optional marker returned from a prior request. Use this marker for
      -- pagination of results from this operation. If this parameter is specified,
      -- the response includes only records beyond the marker, up to the value
      -- specified by MaxRecords.
    , dcsgnMaxRecords :: Maybe Int
      -- ^ The maximum number of records to include in the response. If more records
      -- exist than the specified MaxRecords value, a marker is included in the
      -- response so that the remaining results can be retrieved. Default:
      -- 100Constraints: minimum 20; maximum 100.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeCacheSubnetGroups

instance AWSRequest DescribeCacheSubnetGroups where
    type Er DescribeCacheSubnetGroups = ElastiCacheError
    type Rs DescribeCacheSubnetGroups = DescribeCacheSubnetGroupsResponse
    request = getQuery service "DescribeCacheSubnetGroups"

instance AWSPager DescribeCacheSubnetGroups where
    next rq rs
        | Just x <- dcsgnrsMarker rs = Just $ rq { dcsgnMarker = Just x }
        | otherwise = Nothing

data DescribeCacheSubnetGroupsResponse = DescribeCacheSubnetGroupsResponse
    { dcsgnrsCacheSubnetGroups :: [CacheSubnetGroup]
      -- ^ A list of cache subnet groups. Each element in the list contains detailed
      -- information about one group.
    , dcsgnrsMarker :: Maybe Text
      -- ^ Provides an identifier to allow retrieval of paginated results.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeCacheSubnetGroupsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeCacheSubnetGroupsResponse"
        :| ["DescribeCacheSubnetGroupsResult"]
