{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeReservedInstancesListings
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for DescribeReservedInstancesListings
module Network.AWS.EC2.DescribeReservedInstancesListings where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeReservedInstancesListings = DescribeReservedInstancesListings
    { drilFilters :: [Filter]
    , drilReservedInstancesId :: Maybe Text
    , drilReservedInstancesListingId :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeReservedInstancesListings

instance AWSRequest DescribeReservedInstancesListings where
    type Er DescribeReservedInstancesListings = EC2Error
    type Rs DescribeReservedInstancesListings = DescribeReservedInstancesListingsResponse
    request  = postQuery service "DescribeReservedInstancesListings"
    response = responseXML

data DescribeReservedInstancesListingsResponse = DescribeReservedInstancesListingsResponse
    { drilrReservedInstancesListings :: [ReservedInstancesListing]
    } deriving (Eq, Show, Generic)

instance FromXML DescribeReservedInstancesListingsResponse where
    fromXMLOptions = xmlOptions
