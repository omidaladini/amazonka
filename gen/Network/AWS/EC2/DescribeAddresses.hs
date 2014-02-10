{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeAddresses
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeAddresses operation lists elastic IP addresses assigned to your
-- account.
module Network.AWS.EC2.DescribeAddresses where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeAddresses = DescribeAddresses
    { daAllocationIds :: [Text]
    , daDryRun :: Maybe Bool
    , daFilters :: [Filter]
      -- ^ A list of filters used to match properties for Addresses. For a complete
      -- reference to the available filter keys for this operation, see the Amazon
      -- EC2 API reference.
    , daPublicIps :: [Text]
      -- ^ The optional list of Elastic IP addresses to describe.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeAddresses

instance AWSRequest DescribeAddresses where
    type Er DescribeAddresses = EC2Error
    type Rs DescribeAddresses = DescribeAddressesResponse
    request  = postQuery service "DescribeAddresses"
    response = responseXML

data DescribeAddressesResponse = DescribeAddressesResponse
    { darAddressesSet :: [Address]
      -- ^ The list of Elastic IPs.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeAddressesResponse where
    fromXMLOptions = xmlOptions
