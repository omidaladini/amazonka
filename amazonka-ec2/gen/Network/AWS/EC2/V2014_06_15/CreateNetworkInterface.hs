{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.CreateNetworkInterface
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a network interface in the specified subnet. For more information
-- about network interfaces, see Elastic Network Interfaces in the Amazon
-- Elastic Compute Cloud User Guide. Example 1 This example creates a network
-- interface in the specified subnet with a primary IP address that is
-- automatically selected by Amazon EC2.
-- https://ec2.amazonaws.com/?Action=CreateNetworkInterface
-- &amp;SubnetId=subnet-b2a249da &amp;AUTHPARAMS
-- &lt;CreateNetworkInterfaceResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;8dbe591e-5a22-48cb-b948-dd0aadd55adf&lt;/requestId&gt;
-- &lt;networkInterface&gt;
-- &lt;networkInterfaceId&gt;eni-cfca76a6&lt;/networkInterfaceId&gt;
-- &lt;subnetId&gt;subnet-b2a249da&lt;/subnetId&gt;
-- &lt;vpcId&gt;vpc-c31dafaa&lt;/vpcId&gt;
-- &lt;availabilityZone&gt;ap-southeast-1b&lt;/availabilityZone&gt;
-- &lt;description/&gt; &lt;ownerId&gt;251839141158&lt;/ownerId&gt;
-- &lt;requesterManaged&gt;false&lt;/requesterManaged&gt;
-- &lt;status&gt;available&lt;/status&gt;
-- &lt;macAddress&gt;02:74:b0:72:79:61&lt;/macAddress&gt;
-- &lt;privateIpAddress&gt;10.0.2.157&lt;/privateIpAddress&gt;
-- &lt;sourceDestCheck&gt;true&lt;/sourceDestCheck&gt; &lt;groupSet&gt;
-- &lt;item&gt; &lt;groupId&gt;sg-1a2b3c4d&lt;/groupId&gt;
-- &lt;groupName&gt;default&lt;/groupName&gt; &lt;/item&gt; &lt;/groupSet&gt;
-- &lt;tagSet/&gt; &lt;privateIpAddressesSet&gt; &lt;item&gt;
-- &lt;privateIpAddress&gt;10.0.2.157&lt;/privateIpAddress&gt;
-- &lt;primary&gt;true&lt;/primary&gt; &lt;/item&gt;
-- &lt;/privateIpAddressesSet&gt; &lt;/networkInterface&gt;
-- &lt;/CreateNetworkInterfaceResponse&gt; Example 2 This example creates a
-- network interface in the specified subnet with a primary IP address of
-- 10.0.2.140 and four secondary private IP addresses that are automatically
-- selected by Amazon EC2.
-- https://ec2.amazonaws.com/?Action=CreateNetworkInterface
-- &amp;PrivateIpAddresses.0.Primary=true
-- &amp;PrivateIpAddresses.0.PrivateIpAddress=10.0.2.140
-- &amp;SecondaryPrivateIpAddressCount=4 &amp;SubnetId=subnet-a61dafcf
-- &amp;AUTHPARAMS &lt;CreateNetworkInterfaceResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;bd78c839-0895-4fac-a17f-98b559b6b630&lt;/requestId&gt;
-- &lt;networkInterface&gt;
-- &lt;networkInterfaceId&gt;eni-1bcb7772&lt;/networkInterfaceId&gt;
-- &lt;subnetId&gt;subnet-a61dafcf&lt;/subnetId&gt;
-- &lt;vpcId&gt;vpc-c31dafaa&lt;/vpcId&gt;
-- &lt;availabilityZone&gt;ap-southeast-1b&lt;/availabilityZone&gt;
-- &lt;description/&gt; &lt;ownerId&gt;251839141158&lt;/ownerId&gt;
-- &lt;requesterManaged&gt;false&lt;/requesterManaged&gt;
-- &lt;status&gt;pending&lt;/status&gt;
-- &lt;macAddress&gt;02:74:b0:70:7f:1a&lt;/macAddress&gt;
-- &lt;privateIpAddress&gt;10.0.2.140&lt;/privateIpAddress&gt;
-- &lt;sourceDestCheck&gt;true&lt;/sourceDestCheck&gt; &lt;groupSet&gt;
-- &lt;item&gt; &lt;groupId&gt;sg-1a2b3c4d&lt;/groupId&gt;
-- &lt;groupName&gt;default&lt;/groupName&gt; &lt;/item&gt; &lt;/groupSet&gt;
-- &lt;tagSet/&gt; &lt;privateIpAddressesSet&gt; &lt;item&gt;
-- &lt;privateIpAddress&gt;10.0.2.140&lt;/privateIpAddress&gt;
-- &lt;primary&gt;true&lt;/primary&gt; &lt;/item&gt; &lt;item&gt;
-- &lt;privateIpAddress&gt;10.0.2.172&lt;/privateIpAddress&gt;
-- &lt;primary&gt;false&lt;/primary&gt; &lt;/item&gt; &lt;item&gt;
-- &lt;privateIpAddress&gt;10.0.2.169&lt;/privateIpAddress&gt;
-- &lt;primary&gt;false&lt;/primary&gt; &lt;/item&gt; &lt;item&gt;
-- &lt;privateIpAddress&gt;10.0.2.170&lt;/privateIpAddress&gt;
-- &lt;primary&gt;false&lt;/primary&gt; &lt;/item&gt; &lt;item&gt;
-- &lt;privateIpAddress&gt;10.0.2.171&lt;/privateIpAddress&gt;
-- &lt;primary&gt;false&lt;/primary&gt; &lt;/item&gt;
-- &lt;/privateIpAddressesSet&gt; &lt;/networkInterface&gt;
-- &lt;/CreateNetworkInterfaceResponse&gt; Example 3 This example creates a
-- network interface with a primary private IP address of 10.0.2.130 and two
-- secondary IP addresses of 10.0.2.132 and 10.0.2.133.
-- https://ec2.amazonaws.com/?Action=CreateNetworkInterface
-- &amp;PrivateIpAddresses.0.Primary=true
-- &amp;PrivateIpAddresses.0.PrivateIpAddress=10.0.2.130
-- &amp;PrivateIpAddresses.1.Primary=false
-- &amp;PrivateIpAddresses.1.PrivateIpAddress=10.0.2.132
-- &amp;PrivateIpAddresses.2.Primary=false
-- &amp;PrivateIpAddresses.2.PrivateIpAddress=10.0.2.133
-- &amp;SubnetId=subnet-a61dafcf &amp;AUTHPARAMS
-- &lt;CreateNetworkInterfaceResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;a9565f4c-f928-4113-859b-905886d11658&lt;/requestId&gt;
-- &lt;networkInterface&gt;
-- &lt;networkInterfaceId&gt;eni-41c47828&lt;/networkInterfaceId&gt;
-- &lt;subnetId&gt;subnet-a61dafcf&lt;/subnetId&gt;
-- &lt;vpcId&gt;vpc-c31dafaa&lt;/vpcId&gt;
-- &lt;availabilityZone&gt;ap-southeast-1b&lt;/availabilityZone&gt;
-- &lt;description/&gt; &lt;ownerId&gt;251839141158&lt;/ownerId&gt;
-- &lt;requesterManaged&gt;false&lt;/requesterManaged&gt;
-- &lt;status&gt;pending&lt;/status&gt;
-- &lt;macAddress&gt;02:74:b0:78:bf:ab&lt;/macAddress&gt;
-- &lt;privateIpAddress&gt;10.0.2.130&lt;/privateIpAddress&gt;
-- &lt;sourceDestCheck&gt;true&lt;/sourceDestCheck&gt; &lt;groupSet&gt;
-- &lt;item&gt; &lt;groupId&gt;sg-188d9f74&lt;/groupId&gt;
-- &lt;groupName&gt;default&lt;/groupName&gt; &lt;/item&gt; &lt;/groupSet&gt;
-- &lt;tagSet/&gt; &lt;privateIpAddressesSet&gt; &lt;item&gt;
-- &lt;privateIpAddress&gt;10.0.2.130&lt;/privateIpAddress&gt;
-- &lt;primary&gt;true&lt;/primary&gt; &lt;/item&gt; &lt;item&gt;
-- &lt;privateIpAddress&gt;10.0.2.133&lt;/privateIpAddress&gt;
-- &lt;primary&gt;false&lt;/primary&gt; &lt;/item&gt; &lt;item&gt;
-- &lt;privateIpAddress&gt;10.0.2.132&lt;/privateIpAddress&gt;
-- &lt;primary&gt;false&lt;/primary&gt; &lt;/item&gt;
-- &lt;/privateIpAddressesSet&gt; &lt;/networkInterface&gt;
-- &lt;/CreateNetworkInterfaceResponse&gt;.
module Network.AWS.EC2.V2014_06_15.CreateNetworkInterface where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateNetworkInterface' request.
createNetworkInterface :: Text -- ^ '_cnirSubnetId'
                       -> CreateNetworkInterface
createNetworkInterface p1 = CreateNetworkInterface
    { _cnirSubnetId = p1
    , _cnirDryRun = Nothing
    , _cnirSecondaryPrivateIpAddressCount = Nothing
    , _cnirPrivateIpAddresses = mempty
    , _cnirGroups = mempty
    , _cnirPrivateIpAddress = Nothing
    , _cnirDescription = Nothing
    }

data CreateNetworkInterface = CreateNetworkInterface
    { _cnirSubnetId :: Text
      -- ^ The ID of the subnet to associate with the network interface.
    , _cnirDryRun :: Maybe Bool
      -- ^ 
    , _cnirSecondaryPrivateIpAddressCount :: Maybe Integer
      -- ^ The number of secondary private IP addresses to assign to a
      -- network interface. When you specify a number of secondary IP
      -- addresses, Amazon EC2 selects these IP addresses within the
      -- subnet range. The number of IP addresses you can assign to a
      -- network interface varies by instance type. For more information,
      -- see Private IP Addresses Per ENI Per Instance Type in the Amazon
      -- Elastic Compute Cloud User Guide.
    , _cnirPrivateIpAddresses :: [PrivateIpAddressSpecification]
      -- ^ One or more private IP addresses.
    , _cnirGroups :: [Text]
      -- ^ The IDs of one or more security groups.
    , _cnirPrivateIpAddress :: Maybe Text
      -- ^ The primary private IP address of the network interface. If you
      -- don't specify an IP address, Amazon EC2 selects one for you from
      -- the subnet range.
    , _cnirDescription :: Maybe Text
      -- ^ A description for the network interface.
    } deriving (Show, Generic)

makeLenses ''CreateNetworkInterface

instance ToQuery CreateNetworkInterface where
    toQuery = genericQuery def

data CreateNetworkInterfaceResponse = CreateNetworkInterfaceResponse
    { _cnisNetworkInterface :: Maybe NetworkInterface
      -- ^ Information about the network interface.
    } deriving (Show, Generic)

makeLenses ''CreateNetworkInterfaceResponse

instance FromXML CreateNetworkInterfaceResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateNetworkInterface where
    type Sv CreateNetworkInterface = EC2
    type Rs CreateNetworkInterface = CreateNetworkInterfaceResponse

    request = post "CreateNetworkInterface"
    response _ = xmlResponse