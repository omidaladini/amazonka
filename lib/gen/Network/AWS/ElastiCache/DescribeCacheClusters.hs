{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.DescribeCacheClusters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeCacheClusters operation returns information about all
-- provisioned cache clusters if no cache cluster identifier is specified, or
-- about a specific cache cluster if a cache cluster identifier is supplied.
-- By default, abbreviated information about the cache clusters(s) will be
-- returned. You can use the optional ShowDetails flag to retrieve detailed
-- information about the cache nodes associated with the cache clusters. These
-- details include the DNS address and port for the cache node endpoint. If
-- the cluster is in the CREATING state, only cluster level information will
-- be displayed until all of the nodes are successfully provisioned. If the
-- cluster is in the DELETING state, only cluster level information will be
-- displayed. If cache nodes are currently being added to the cache cluster,
-- node endpoint information and creation time for the additional nodes will
-- not be displayed until they are completely provisioned. When the cache
-- cluster state is available, the cluster is ready for use. If cache nodes
-- are currently being removed from the cache cluster, no endpoint information
-- for the removed nodes is displayed.
-- https://elasticache.us-east-1.amazonaws.com/ ?Action=DescribeCacheClusters
-- &MaxRecords=100 &Version=2013-06-15 &ShowCacheNodeInfo=false
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-07-27T02%3A55%3A54.654Z &AWSAccessKeyId=YOUR-ACCESS-KEY
-- &Signature=YOUR-SIGNATURE in-sync default.memcached1.4 simcoprod42
-- available 11211 simcoprod42.m2st2p.cfg.cache.amazonaws.com
-- https://console.aws.amazon.com/elasticache/home#client-download:
-- cache.m1.large memcached us-east-1d 2011-07-26T01:21:46.607Z 1.4.5 true
-- fri:08:30-fri:09:00 default active active
-- arn:aws:sns:us-east-1:123456789012:ElastiCacheNotifications 6
-- f270d58f-b7fb-11e0-9326-b7275b9d4a6c.
module Network.AWS.ElastiCache.DescribeCacheClusters where

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

data DescribeCacheClusters = DescribeCacheClusters
    { dccnCacheClusterId :: Maybe Text
      -- ^ The user-supplied cluster identifier. If this parameter is specified, only
      -- information about that specific cache cluster is returned. This parameter
      -- isn't case sensitive.
    , dccnMarker :: Maybe Text
      -- ^ An optional marker returned from a prior request. Use this marker for
      -- pagination of results from this operation. If this parameter is specified,
      -- the response includes only records beyond the marker, up to the value
      -- specified by MaxRecords.
    , dccnMaxRecords :: Maybe Int
      -- ^ The maximum number of records to include in the response. If more records
      -- exist than the specified MaxRecords value, a marker is included in the
      -- response so that the remaining results can be retrieved. Default:
      -- 100Constraints: minimum 20; maximum 100.
    , dccnShowCacheNodeInfo :: Maybe Bool
      -- ^ An optional flag that can be included in the DescribeCacheCluster request
      -- to retrieve information about the individual cache nodes.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeCacheClusters

instance AWSRequest DescribeCacheClusters where
    type Er DescribeCacheClusters = ElastiCacheError
    type Rs DescribeCacheClusters = DescribeCacheClustersResponse
    request = getQuery service "DescribeCacheClusters"

instance AWSPager DescribeCacheClusters where
    next rq rs
        | Just x <- dccnrsMarker rs = Just $ rq { dccnMarker = Just x }
        | otherwise = Nothing

data DescribeCacheClustersResponse = DescribeCacheClustersResponse
    { dccnrsCacheClusters :: [CacheCluster]
      -- ^ A list of cache clusters. Each item in the list contains detailed
      -- information about one cache cluster.
    , dccnrsMarker :: Maybe Text
      -- ^ Provides an identifier to allow retrieval of paginated results.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeCacheClustersResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeCacheClustersResponse"
        :| ["DescribeCacheClustersResult"]
