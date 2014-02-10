{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateSubnet
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a subnet in an existing VPC. You can create up to 20 subnets in a
-- VPC. If you add more than one subnet to a VPC, they're set up in a star
-- topology with a logical router in the middle. When you create each subnet,
-- you provide the VPC ID and the CIDR block you want for the subnet. Once you
-- create a subnet, you can't change its CIDR block. The subnet's CIDR block
-- can be the same as the VPC's CIDR block (assuming you want only a single
-- subnet in the VPC), or a subset of the VPC's CIDR block. If you create more
-- than one subnet in a VPC, the subnets' CIDR blocks must not overlap. The
-- smallest subnet (and VPC) you can create uses a /28 netmask (16 IP
-- addresses), and the largest uses a /18 netmask (16,384 IP addresses). AWS
-- reserves both the first four and the last IP address in each subnet's CIDR
-- block. They're not available for use.
module Network.AWS.EC2.CreateSubnet where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
createSubnet :: Text
             -- ^ The CIDR block the subnet is to cover.
             -> Text
             -- ^ The ID of the VPC to create the subnet in.
             -> CreateSubnet
createSubnet p1 p2 = CreateSubnet
    { csdCidrBlock = p1
    , csdVpcId = p2
    , csdAvailabilityZone = Nothing
    , csdDryRun = Nothing
    }

data CreateSubnet = CreateSubnet
    { csdAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone to create the subnet in.
    , csdCidrBlock :: !Text
      -- ^ The CIDR block the subnet is to cover.
    , csdDryRun :: Maybe Bool
    , csdVpcId :: !Text
      -- ^ The ID of the VPC to create the subnet in.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateSubnet

instance AWSRequest CreateSubnet where
    type Er CreateSubnet = EC2Error
    type Rs CreateSubnet = CreateSubnetResponse
    request  = postQuery service "CreateSubnet"
    response = responseXML

data CreateSubnetResponse = CreateSubnetResponse
    { csdrSubnet :: Maybe Subnet
    } deriving (Eq, Show, Generic)

instance FromXML CreateSubnetResponse where
    fromXMLOptions = xmlOptions