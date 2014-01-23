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

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeReservedInstancesOfferings = DescribeReservedInstancesOfferings
    { driorAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone in which the Reserved Instance can be used.
    , driorDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , driorFilters :: [Filter]
      -- ^ A list of filters used to match properties for ReservedInstancesOfferings.
      -- For a complete reference to the available filter keys for this operation,
      -- see the Amazon EC2 API reference.
    , driorIncludeMarketplace :: Maybe Bool
      -- ^ Include Marketplace offerings in the response.
    , driorInstanceTenancy :: Maybe Tenancy
      -- ^ The tenancy of the Reserved Instance offering. A Reserved Instance with
      -- tenancy of dedicated will run on single-tenant hardware and can only be
      -- launched within a VPC.
    , driorInstanceType :: Maybe InstanceType
      -- ^ The instance type on which the Reserved Instance can be used.
    , driorMaxDuration :: Maybe Integer
      -- ^ Maximum duration (in seconds) to filter when searching for offerings.
    , driorMaxInstanceCount :: Maybe Int
      -- ^ FIXME: Missing documentation
    , driorMaxResults :: Maybe Int
      -- ^ FIXME: Missing documentation
    , driorMinDuration :: Maybe Integer
      -- ^ Minimum duration (in seconds) to filter when searching for offerings.
    , driorNextToken :: Maybe Text
      -- ^ FIXME: Missing documentation
    , driorOfferingType :: Maybe OfferingTypeValues
      -- ^ The Reserved Instance offering type.
    , driorProductDescription :: Maybe RIProductDescription
      -- ^ The Reserved Instance product description.
    , driorReservedInstancesOfferingIds :: [Text]
      -- ^ An optional list of the unique IDs of the Reserved Instance offerings to
      -- describe.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeReservedInstancesOfferings

instance AWSRequest DescribeReservedInstancesOfferings where
    type Er DescribeReservedInstancesOfferings = EC2Error
    type Rs DescribeReservedInstancesOfferings = DescribeReservedInstancesOfferingsResponse
    request = v2Query service GET "DescribeReservedInstancesOfferings"

instance AWSPager DescribeReservedInstancesOfferings where
    next rq rs
        | Just x <- driorrsNextToken rs = Just $ rq { driorNextToken = Just x }
        | otherwise = Nothing

data DescribeReservedInstancesOfferingsResponse = DescribeReservedInstancesOfferingsResponse
    { driorrsNextToken :: Maybe Text
      -- ^ FIXME: Missing documentation
    , driorrsReservedInstancesOfferings :: [ReservedInstancesOffering]
      -- ^ The list of described Reserved Instance offerings.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeReservedInstancesOfferingsResponse where
    fromXMLOptions = xmlOptions
