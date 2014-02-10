{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeSubnets
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gives you information about your subnets. You can filter the results to
-- return information only about subnets that match criteria you specify. For
-- example, you could ask to get information about a particular subnet (or
-- all) only if the subnet's state is available. You can specify multiple
-- filters (e.g., the subnet is in a particular VPC, and the subnet's state is
-- available). The result includes information for a particular subnet only if
-- the subnet matches all your filters. If there's no match, no special
-- message is returned; the response is simply empty. The following table
-- shows the available filters.
module Network.AWS.EC2.DescribeSubnets where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeSubnets = DescribeSubnets
    { dsDryRun :: Maybe Bool
    , dsFilters :: [Filter]
      -- ^ A list of filters used to match properties for Subnets. For a complete
      -- reference to the available filter keys for this operation, see the Amazon
      -- EC2 API reference.
    , dsSubnetIds :: [Text]
      -- ^ A set of one or more subnet IDs.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeSubnets

instance AWSRequest DescribeSubnets where
    type Er DescribeSubnets = EC2Error
    type Rs DescribeSubnets = DescribeSubnetsResponse
    request  = postQuery service "DescribeSubnets"
    response = responseXML

data DescribeSubnetsResponse = DescribeSubnetsResponse
    { dsrSubnetSet :: [Subnet]
      -- ^ Contains a set of one or more Subnet instances.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeSubnetsResponse where
    fromXMLOptions = xmlOptions
