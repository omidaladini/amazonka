{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
-- &ReservedCacheNodeId=customerSpecifiedID &Version=2014-03-24
-- &SignatureVersion=4 &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z
-- &X-Amz-Credential= Medium Utilization memcached
-- 649fd0c8-cf6d-47a0-bfa6-060f8e75e95f payment-failed myreservationid 1
-- 2010-12-15T00:25:14.131Z 31536000 227.5 0.046 cache.m1.small
-- c695119b-2961-11e1-bd06-6fe008f046c3.
module Network.AWS.ElastiCache.DescribeReservedCacheNodes
    (
    -- * Request
      DescribeReservedCacheNodes
    -- ** Request constructor
    , mkDescribeReservedCacheNodes
    -- ** Request lenses
    , drcnReservedCacheNodeId
    , drcnReservedCacheNodesOfferingId
    , drcnCacheNodeType
    , drcnDuration
    , drcnProductDescription
    , drcnOfferingType
    , drcnMaxRecords
    , drcnMarker

    -- * Response
    , DescribeReservedCacheNodesResponse
    -- ** Response constructor
    , mkDescribeReservedCacheNodesResponse
    -- ** Response lenses
    , drcnrMarker
    , drcnrReservedCacheNodes
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import Network.AWS.Prelude

-- | Represents the input of a DescribeReservedCacheNodes operation.
data DescribeReservedCacheNodes = DescribeReservedCacheNodes
    { _drcnReservedCacheNodeId :: !(Maybe Text)
    , _drcnReservedCacheNodesOfferingId :: !(Maybe Text)
    , _drcnCacheNodeType :: !(Maybe Text)
    , _drcnDuration :: !(Maybe Text)
    , _drcnProductDescription :: !(Maybe Text)
    , _drcnOfferingType :: !(Maybe Text)
    , _drcnMaxRecords :: !(Maybe Integer)
    , _drcnMarker :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeReservedCacheNodes' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ReservedCacheNodeId ::@ @Maybe Text@
--
-- * @ReservedCacheNodesOfferingId ::@ @Maybe Text@
--
-- * @CacheNodeType ::@ @Maybe Text@
--
-- * @Duration ::@ @Maybe Text@
--
-- * @ProductDescription ::@ @Maybe Text@
--
-- * @OfferingType ::@ @Maybe Text@
--
-- * @MaxRecords ::@ @Maybe Integer@
--
-- * @Marker ::@ @Maybe Text@
--
mkDescribeReservedCacheNodes :: DescribeReservedCacheNodes
mkDescribeReservedCacheNodes = DescribeReservedCacheNodes
    { _drcnReservedCacheNodeId = Nothing
    , _drcnReservedCacheNodesOfferingId = Nothing
    , _drcnCacheNodeType = Nothing
    , _drcnDuration = Nothing
    , _drcnProductDescription = Nothing
    , _drcnOfferingType = Nothing
    , _drcnMaxRecords = Nothing
    , _drcnMarker = Nothing
    }

-- | The reserved cache node identifier filter value. Use this parameter to show
-- only the reservation that matches the specified reservation ID.
drcnReservedCacheNodeId :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnReservedCacheNodeId =
    lens _drcnReservedCacheNodeId
         (\s a -> s { _drcnReservedCacheNodeId = a })

-- | The offering identifier filter value. Use this parameter to show only
-- purchased reservations matching the specified offering identifier.
drcnReservedCacheNodesOfferingId :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnReservedCacheNodesOfferingId =
    lens _drcnReservedCacheNodesOfferingId
         (\s a -> s { _drcnReservedCacheNodesOfferingId = a })

-- | The cache node type filter value. Use this parameter to show only those
-- reservations matching the specified cache node type.
drcnCacheNodeType :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnCacheNodeType =
    lens _drcnCacheNodeType (\s a -> s { _drcnCacheNodeType = a })

-- | The duration filter value, specified in years or seconds. Use this
-- parameter to show only reservations for this duration. Valid Values: 1 | 3
-- | 31536000 | 94608000.
drcnDuration :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnDuration = lens _drcnDuration (\s a -> s { _drcnDuration = a })

-- | The product description filter value. Use this parameter to show only those
-- reservations matching the specified product description.
drcnProductDescription :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnProductDescription =
    lens _drcnProductDescription (\s a -> s { _drcnProductDescription = a })

-- | The offering type filter value. Use this parameter to show only the
-- available offerings matching the specified offering type. Valid values:
-- "Light Utilization" | "Medium Utilization" | "Heavy Utilization".
drcnOfferingType :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnOfferingType =
    lens _drcnOfferingType (\s a -> s { _drcnOfferingType = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
drcnMaxRecords :: Lens' DescribeReservedCacheNodes (Maybe Integer)
drcnMaxRecords = lens _drcnMaxRecords (\s a -> s { _drcnMaxRecords = a })

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by MaxRecords.
drcnMarker :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnMarker = lens _drcnMarker (\s a -> s { _drcnMarker = a })

instance ToQuery DescribeReservedCacheNodes where
    toQuery = genericQuery def

-- | Represents the output of a DescribeReservedCacheNodes operation.
data DescribeReservedCacheNodesResponse = DescribeReservedCacheNodesResponse
    { _drcnrMarker :: !(Maybe Text)
    , _drcnrReservedCacheNodes :: [ReservedCacheNode]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeReservedCacheNodesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Marker ::@ @Maybe Text@
--
-- * @ReservedCacheNodes ::@ @[ReservedCacheNode]@
--
mkDescribeReservedCacheNodesResponse :: DescribeReservedCacheNodesResponse
mkDescribeReservedCacheNodesResponse = DescribeReservedCacheNodesResponse
    { _drcnrMarker = Nothing
    , _drcnrReservedCacheNodes = mempty
    }

-- | Provides an identifier to allow retrieval of paginated results.
drcnrMarker :: Lens' DescribeReservedCacheNodesResponse (Maybe Text)
drcnrMarker = lens _drcnrMarker (\s a -> s { _drcnrMarker = a })

-- | A list of reserved cache nodes. Each element in the list contains detailed
-- information about one node.
drcnrReservedCacheNodes :: Lens' DescribeReservedCacheNodesResponse [ReservedCacheNode]
drcnrReservedCacheNodes =
    lens _drcnrReservedCacheNodes
         (\s a -> s { _drcnrReservedCacheNodes = a })

instance FromXML DescribeReservedCacheNodesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeReservedCacheNodes where
    type Sv DescribeReservedCacheNodes = ElastiCache
    type Rs DescribeReservedCacheNodes = DescribeReservedCacheNodesResponse

    request = post "DescribeReservedCacheNodes"
    response _ = xmlResponse

instance AWSPager DescribeReservedCacheNodes where
    next rq rs = (\x -> rq & drcnMarker ?~ x)
        <$> (rs ^. drcnrMarker)