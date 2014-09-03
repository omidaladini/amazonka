{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.DescribeReservedCacheNodes
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
-- &ReservedCacheNodeId=customerSpecifiedID &Version=2014-03-24
-- &SignatureVersion=4 &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z
-- &X-Amz-Credential= Medium Utilization memcached
-- 649fd0c8-cf6d-47a0-bfa6-060f8e75e95f payment-failed myreservationid 1
-- 2010-12-15T00:25:14.131Z 31536000 227.5 0.046 cache.m1.small
-- c695119b-2961-11e1-bd06-6fe008f046c3.
module Network.AWS.ElastiCache.V2014_07_15.DescribeReservedCacheNodes
    (
    -- * Request
      DescribeReservedCacheNodes
    -- ** Request constructor
    , describeReservedCacheNodes
    -- ** Request lenses
    , drcnmMaxRecords
    , drcnmReservedCacheNodeId
    , drcnmReservedCacheNodesOfferingId
    , drcnmCacheNodeType
    , drcnmDuration
    , drcnmProductDescription
    , drcnmOfferingType
    , drcnmMarker

    -- * Response
    , DescribeReservedCacheNodesResponse
    -- ** Response lenses
    , rcnmReservedCacheNodes
    , rcnmMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeReservedCacheNodes' request.
describeReservedCacheNodes :: DescribeReservedCacheNodes
describeReservedCacheNodes = DescribeReservedCacheNodes
    { _drcnmMaxRecords = Nothing
    , _drcnmReservedCacheNodeId = Nothing
    , _drcnmReservedCacheNodesOfferingId = Nothing
    , _drcnmCacheNodeType = Nothing
    , _drcnmDuration = Nothing
    , _drcnmProductDescription = Nothing
    , _drcnmOfferingType = Nothing
    , _drcnmMarker = Nothing
    }

data DescribeReservedCacheNodes = DescribeReservedCacheNodes
    { _drcnmMaxRecords :: Maybe Integer
      -- ^ The maximum number of records to include in the response. If more
      -- records exist than the specified MaxRecords value, a marker is
      -- included in the response so that the remaining results can be
      -- retrieved. Default: 100 Constraints: minimum 20; maximum 100.
    , _drcnmReservedCacheNodeId :: Maybe Text
      -- ^ The reserved cache node identifier filter value. Use this
      -- parameter to show only the reservation that matches the specified
      -- reservation ID.
    , _drcnmReservedCacheNodesOfferingId :: Maybe Text
      -- ^ The offering identifier filter value. Use this parameter to show
      -- only purchased reservations matching the specified offering
      -- identifier.
    , _drcnmCacheNodeType :: Maybe Text
      -- ^ The cache node type filter value. Use this parameter to show only
      -- those reservations matching the specified cache node type.
    , _drcnmDuration :: Maybe Text
      -- ^ The duration filter value, specified in years or seconds. Use
      -- this parameter to show only reservations for this duration. Valid
      -- Values: 1 | 3 | 31536000 | 94608000.
    , _drcnmProductDescription :: Maybe Text
      -- ^ The product description filter value. Use this parameter to show
      -- only those reservations matching the specified product
      -- description.
    , _drcnmOfferingType :: Maybe Text
      -- ^ The offering type filter value. Use this parameter to show only
      -- the available offerings matching the specified offering type.
      -- Valid values: "Light Utilization" | "Medium Utilization" | "Heavy
      -- Utilization".
    , _drcnmMarker :: Maybe Text
      -- ^ An optional marker returned from a prior request. Use this marker
      -- for pagination of results from this operation. If this parameter
      -- is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords.
    } deriving (Show, Generic)

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
drcnmMaxRecords
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeReservedCacheNodes
    -> f DescribeReservedCacheNodes
drcnmMaxRecords f x =
    (\y -> x { _drcnmMaxRecords = y })
       <$> f (_drcnmMaxRecords x)
{-# INLINE drcnmMaxRecords #-}

-- | The reserved cache node identifier filter value. Use this parameter to show
-- only the reservation that matches the specified reservation ID.
drcnmReservedCacheNodeId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeReservedCacheNodes
    -> f DescribeReservedCacheNodes
drcnmReservedCacheNodeId f x =
    (\y -> x { _drcnmReservedCacheNodeId = y })
       <$> f (_drcnmReservedCacheNodeId x)
{-# INLINE drcnmReservedCacheNodeId #-}

-- | The offering identifier filter value. Use this parameter to show only
-- purchased reservations matching the specified offering identifier.
drcnmReservedCacheNodesOfferingId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeReservedCacheNodes
    -> f DescribeReservedCacheNodes
drcnmReservedCacheNodesOfferingId f x =
    (\y -> x { _drcnmReservedCacheNodesOfferingId = y })
       <$> f (_drcnmReservedCacheNodesOfferingId x)
{-# INLINE drcnmReservedCacheNodesOfferingId #-}

-- | The cache node type filter value. Use this parameter to show only those
-- reservations matching the specified cache node type.
drcnmCacheNodeType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeReservedCacheNodes
    -> f DescribeReservedCacheNodes
drcnmCacheNodeType f x =
    (\y -> x { _drcnmCacheNodeType = y })
       <$> f (_drcnmCacheNodeType x)
{-# INLINE drcnmCacheNodeType #-}

-- | The duration filter value, specified in years or seconds. Use this
-- parameter to show only reservations for this duration. Valid Values: 1 | 3
-- | 31536000 | 94608000.
drcnmDuration
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeReservedCacheNodes
    -> f DescribeReservedCacheNodes
drcnmDuration f x =
    (\y -> x { _drcnmDuration = y })
       <$> f (_drcnmDuration x)
{-# INLINE drcnmDuration #-}

-- | The product description filter value. Use this parameter to show only those
-- reservations matching the specified product description.
drcnmProductDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeReservedCacheNodes
    -> f DescribeReservedCacheNodes
drcnmProductDescription f x =
    (\y -> x { _drcnmProductDescription = y })
       <$> f (_drcnmProductDescription x)
{-# INLINE drcnmProductDescription #-}

-- | The offering type filter value. Use this parameter to show only the
-- available offerings matching the specified offering type. Valid values:
-- "Light Utilization" | "Medium Utilization" | "Heavy Utilization".
drcnmOfferingType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeReservedCacheNodes
    -> f DescribeReservedCacheNodes
drcnmOfferingType f x =
    (\y -> x { _drcnmOfferingType = y })
       <$> f (_drcnmOfferingType x)
{-# INLINE drcnmOfferingType #-}

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
drcnmMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeReservedCacheNodes
    -> f DescribeReservedCacheNodes
drcnmMarker f x =
    (\y -> x { _drcnmMarker = y })
       <$> f (_drcnmMarker x)
{-# INLINE drcnmMarker #-}

instance ToQuery DescribeReservedCacheNodes where
    toQuery = genericQuery def

data DescribeReservedCacheNodesResponse = DescribeReservedCacheNodesResponse
    { _rcnmReservedCacheNodes :: [ReservedCacheNode]
      -- ^ A list of reserved cache nodes. Each element in the list contains
      -- detailed information about one node.
    , _rcnmMarker :: Maybe Text
      -- ^ Provides an identifier to allow retrieval of paginated results.
    } deriving (Show, Generic)

-- | A list of reserved cache nodes. Each element in the list contains detailed
-- information about one node.
rcnmReservedCacheNodes
    :: Functor f
    => ([ReservedCacheNode]
    -> f ([ReservedCacheNode]))
    -> DescribeReservedCacheNodesResponse
    -> f DescribeReservedCacheNodesResponse
rcnmReservedCacheNodes f x =
    (\y -> x { _rcnmReservedCacheNodes = y })
       <$> f (_rcnmReservedCacheNodes x)
{-# INLINE rcnmReservedCacheNodes #-}

-- | Provides an identifier to allow retrieval of paginated results.
rcnmMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeReservedCacheNodesResponse
    -> f DescribeReservedCacheNodesResponse
rcnmMarker f x =
    (\y -> x { _rcnmMarker = y })
       <$> f (_rcnmMarker x)
{-# INLINE rcnmMarker #-}

instance FromXML DescribeReservedCacheNodesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeReservedCacheNodes where
    type Sv DescribeReservedCacheNodes = ElastiCache
    type Rs DescribeReservedCacheNodes = DescribeReservedCacheNodesResponse

    request = post "DescribeReservedCacheNodes"
    response _ = xmlResponse

instance AWSPager DescribeReservedCacheNodes where
    next rq rs = (\x -> rq { _drcnmMarker = Just x })
        <$> (_rcnmMarker rs)
