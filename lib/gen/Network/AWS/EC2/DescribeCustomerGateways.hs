{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeCustomerGateways
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gives you information about your customer gateways. You can filter the
-- results to return information only about customer gateways that match
-- criteria you specify. For example, you could ask to get information about a
-- particular customer gateway (or all) only if the gateway's state is pending
-- or available. You can specify multiple filters (e.g., the customer gateway
-- has a particular IP address for the Internet-routable external interface,
-- and the gateway's state is pending or available). The result includes
-- information for a particular customer gateway only if the gateway matches
-- all your filters. If there's no match, no special message is returned; the
-- response is simply empty. The following table shows the available filters.
module Network.AWS.EC2.DescribeCustomerGateways where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeCustomerGateways = DescribeCustomerGateways
    { dcgCustomerGatewayIds :: [Text]
      -- ^ A set of one or more customer gateway IDs.
    , dcgDryRun :: Maybe Bool
    , dcgFilters :: [Filter]
      -- ^ A list of filters used to match properties for Customer Gateways. For a
      -- complete reference to the available filter keys for this operation, see the
      -- Amazon EC2 API reference.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeCustomerGateways

instance AWSRequest DescribeCustomerGateways where
    type Er DescribeCustomerGateways = EC2Error
    type Rs DescribeCustomerGateways = DescribeCustomerGatewaysResponse
    request  = postQuery service "DescribeCustomerGateways"
    response = responseXML

data DescribeCustomerGatewaysResponse = DescribeCustomerGatewaysResponse
    { dcgrCustomerGateways :: [CustomerGateway]
    } deriving (Eq, Show, Generic)

instance FromXML DescribeCustomerGatewaysResponse where
    fromXMLOptions = xmlOptions
