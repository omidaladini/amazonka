{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.DescribeReservedCacheNodes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeReservedCacheNodes operation returns information about reserved
-- cache nodes for this account, or about a specified reserved cache node.
-- https://elasticache.amazonaws.com/ ?Action=DescribeReservedCacheNodes
-- &ReservedCacheNodeId=customerSpecifiedID &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2012-12-18T18%3A31%3A36.118Z
-- &AWSAccessKeyId= &Signature= Medium Utilization memcached
-- 649fd0c8-cf6d-47a0-bfa6-060f8e75e95f payment-failed myreservationid 1
-- 2010-12-15T00:25:14.131Z 31536000 227.5 0.046 cache.m1.small
-- c695119b-2961-11e1-bd06-6fe008f046c3.
module Network.AWS.ElastiCache.DescribeReservedCacheNodes where

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
describeReservedCacheNodes :: DescribeReservedCacheNodes
describeReservedCacheNodes = DescribeReservedCacheNodes
    { drcnmCacheNodeType = Nothing
    , drcnmDuration = Nothing
    , drcnmMarker = Nothing
    , drcnmMaxRecords = Nothing
    , drcnmOfferingType = Nothing
    , drcnmProductDescription = Nothing
    , drcnmReservedCacheNodeId = Nothing
    , drcnmReservedCacheNodesOfferingId = Nothing
    }

data DescribeReservedCacheNodes = DescribeReservedCacheNodes
    { drcnmCacheNodeType :: Maybe Text
      -- ^ The cache node type filter value. Use this parameter to show only those
      -- reservations matching the specified cache node type.
    , drcnmDuration :: Maybe Text
      -- ^ The duration filter value, specified in years or seconds. Use this
      -- parameter to show only reservations for this duration. Valid Values: 1 | 3
      -- | 31536000 | 94608000.
    , drcnmMarker :: Maybe Text
      -- ^ An optional marker returned from a prior request. Use this marker for
      -- pagination of results from this operation. If this parameter is specified,
      -- the response includes only records beyond the marker, up to the value
      -- specified by MaxRecords.
    , drcnmMaxRecords :: Maybe Int
      -- ^ The maximum number of records to include in the response. If more records
      -- exist than the specified MaxRecords value, a marker is included in the
      -- response so that the remaining results can be retrieved. Default:
      -- 100Constraints: minimum 20; maximum 100.
    , drcnmOfferingType :: Maybe Text
      -- ^ The offering type filter value. Use this parameter to show only the
      -- available offerings matching the specified offering type. Valid values:
      -- "Light Utilization" | "Medium Utilization" | "Heavy Utilization".
    , drcnmProductDescription :: Maybe Text
      -- ^ The product description filter value. Use this parameter to show only those
      -- reservations matching the specified product description.
    , drcnmReservedCacheNodeId :: Maybe Text
      -- ^ The reserved cache node identifier filter value. Use this parameter to show
      -- only the reservation that matches the specified reservation ID.
    , drcnmReservedCacheNodesOfferingId :: Maybe Text
      -- ^ The offering identifier filter value. Use this parameter to show only
      -- purchased reservations matching the specified offering identifier.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeReservedCacheNodes

instance AWSRequest DescribeReservedCacheNodes where
    type Er DescribeReservedCacheNodes = ElastiCacheError
    type Rs DescribeReservedCacheNodes = DescribeReservedCacheNodesResponse
    request = getQuery service "DescribeReservedCacheNodes"

instance AWSPager DescribeReservedCacheNodes where
    next rq rs
        | Just x <- drcnmrsMarker rs = Just $ rq { drcnmMarker = Just x }
        | otherwise = Nothing

data DescribeReservedCacheNodesResponse = DescribeReservedCacheNodesResponse
    { drcnmrsMarker :: Maybe Text
      -- ^ Provides an identifier to allow retrieval of paginated results.
    , drcnmrsReservedCacheNodes :: [ReservedCacheNode]
      -- ^ A list of reserved cache nodes. Each element in the list contains detailed
      -- information about one node.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeReservedCacheNodesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeReservedCacheNodesResponse"
        :| ["DescribeReservedCacheNodesResult"]
