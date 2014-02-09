{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateVpc
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a VPC with the CIDR block you specify. The smallest VPC you can
-- create uses a /28 netmask (16 IP addresses), and the largest uses a /18
-- netmask (16,384 IP addresses). To help you decide how big to make your VPC,
-- go to the topic about creating VPCs in the Amazon Virtual Private Cloud
-- Developer Guide. By default, each instance you launch in the VPC has the
-- default DHCP options (the standard EC2 host name, no domain name, no DNS
-- server, no NTP server, and no NetBIOS server or node type).
module Network.AWS.EC2.CreateVpc where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
createVpc :: Text
          -- ^ A valid CIDR block.
          -> CreateVpc
createVpc p1 = CreateVpc
    { cvrCidrBlock = p1
    , cvrDryRun = Nothing
    , cvrInstanceTenancy = Nothing
    }

data CreateVpc = CreateVpc
    { cvrCidrBlock :: !Text
      -- ^ A valid CIDR block.
    , cvrDryRun :: Maybe Bool
    , cvrInstanceTenancy :: Maybe Tenancy
      -- ^ The allowed tenancy of instances launched into the VPC. A value of default
      -- means instances can be launched with any tenancy; a value of dedicated
      -- means instances must be launched with tenancy as dedicated.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateVpc

instance AWSRequest CreateVpc where
    type Er CreateVpc = EC2Error
    type Rs CreateVpc = CreateVpcResponse
    request = getQuery service "CreateVpc"

data CreateVpcResponse = CreateVpcResponse
    { cvrrVpc :: Maybe Vpc
      -- ^ Information about the VPC.
    } deriving (Eq, Show, Generic)

instance FromXML CreateVpcResponse where
    fromXMLOptions = xmlOptions
