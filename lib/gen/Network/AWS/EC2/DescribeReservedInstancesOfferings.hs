{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeReservedInstancesOfferings
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeReservedInstancesOfferings operation describes Reserved
-- Instance offerings that are available for purchase. With Amazon EC2
-- Reserved Instances, you purchase the right to launch Amazon EC2 instances
-- for a period of time (without getting insufficient capacity errors) and pay
-- a lower usage rate for the actual time used.
module Network.AWS.EC2.DescribeReservedInstancesOfferings where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeReservedInstancesOfferings = DescribeReservedInstancesOfferings
    { drioAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone in which the Reserved Instance can be used.
    , drioDryRun :: Maybe Bool
    , drioFilters :: [Filter]
      -- ^ A list of filters used to match properties for ReservedInstancesOfferings.
      -- For a complete reference to the available filter keys for this operation,
      -- see the Amazon EC2 API reference.
    , drioIncludeMarketplace :: Maybe Bool
      -- ^ Include Marketplace offerings in the response.
    , drioInstanceTenancy :: Maybe Tenancy
      -- ^ The tenancy of the Reserved Instance offering. A Reserved Instance with
      -- tenancy of dedicated will run on single-tenant hardware and can only be
      -- launched within a VPC.
    , drioInstanceType :: Maybe InstanceType
      -- ^ The instance type on which the Reserved Instance can be used.
    , drioMaxDuration :: Maybe Integer
      -- ^ Maximum duration (in seconds) to filter when searching for offerings.
    , drioMaxInstanceCount :: Maybe Int
    , drioMaxResults :: Maybe Int
    , drioMinDuration :: Maybe Integer
      -- ^ Minimum duration (in seconds) to filter when searching for offerings.
    , drioNextToken :: Maybe Text
    , drioOfferingType :: Maybe OfferingTypeValues
      -- ^ The Reserved Instance offering type.
    , drioProductDescription :: Maybe RIProductDescription
      -- ^ The Reserved Instance product description.
    , drioReservedInstancesOfferingIds :: [Text]
      -- ^ An optional list of the unique IDs of the Reserved Instance offerings to
      -- describe.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeReservedInstancesOfferings

instance AWSRequest DescribeReservedInstancesOfferings where
    type Er DescribeReservedInstancesOfferings = EC2Error
    type Rs DescribeReservedInstancesOfferings = DescribeReservedInstancesOfferingsResponse
    request  = postQuery service "DescribeReservedInstancesOfferings"
    response = responseXML

instance AWSPager DescribeReservedInstancesOfferings where
    next rq rs
        | Just x <- driorNextToken rs = Just $ rq { drioNextToken = Just x }
        | otherwise = Nothing

data DescribeReservedInstancesOfferingsResponse = DescribeReservedInstancesOfferingsResponse
    { driorNextToken :: Maybe Text
    , driorReservedInstancesOfferingsSet :: [ReservedInstancesOffering]
      -- ^ The list of described Reserved Instance offerings.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeReservedInstancesOfferingsResponse where
    fromXMLOptions = xmlOptions
