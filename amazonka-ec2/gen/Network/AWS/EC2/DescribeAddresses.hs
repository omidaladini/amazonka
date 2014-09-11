{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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

-- | Describes one or more of your Elastic IP addresses. An Elastic IP address
-- is for use in either the EC2-Classic platform or in a VPC. For more
-- information, see Elastic IP Addresses in the Amazon Elastic Compute Cloud
-- User Guide. Example for EC2-Classic This example request describes two
-- specific Elastic IP addresses allocated to your account. Both addresses
-- were created for instances in EC2-Classic, so you must specify them using
-- their IP addresses. The address 192.0.2.1 is assigned to instance
-- i-f15ebb98, and 198.51.100.2 isn't assigned to an instance.
-- https://ec2.amazonaws.com/?Action=DescribeAddresses
-- &amp;PublicIp.1=192.0.2.1 &amp;PublicIp.2=198.51.100.2 &amp;AUTHPARAMS
-- &lt;DescribeAddressesResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;addressesSet&gt; &lt;item&gt;
-- &lt;publicIp&gt;192.0.2.1&lt;/publicIp&gt;
-- &lt;domain&gt;standard&lt;/domain&gt;
-- &lt;instanceId&gt;i-f15ebb98&lt;/instanceId&gt; &lt;/item&gt; &lt;item&gt;
-- &lt;publicIp&gt;198.51.100.2&lt;/publicIp&gt;
-- &lt;domain&gt;standard&lt;/domain&gt; &lt;instanceId/&gt; &lt;/item&gt;
-- &lt;/addressesSet&gt; &lt;/DescribeAddressesResponse&gt; Example 1 for
-- EC2-VPC This example request describes a specific Elastic IP address
-- allocated to your account. This address was created for instances in
-- EC2-VPC, so you must use the allocation ID to specify the address.
-- https://ec2.amazonaws.com/?Action=DescribeAddresses &amp;AllocationId.1=
-- eipalloc-08229861 &amp;AUTHPARAMS &lt;DescribeAddressesResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;f7de5e98-491a-4c19-a92d-908d6EXAMPLE&lt;/requestId&gt;
-- &lt;addressesSet&gt; &lt;item&gt;
-- &lt;publicIp&gt;203.0.113.41&lt;/publicIp&gt;
-- &lt;allocationId&gt;eipalloc-08229861&lt;/allocationId&gt;
-- &lt;domain&gt;vpc&lt;/domain&gt;
-- &lt;instanceId&gt;i-64600030&lt;/instanceId&gt;
-- &lt;associationId&gt;eipassoc-f0229899&lt;/associationId&gt;
-- &lt;networkInterfaceId&gt;eni-ef229886&lt;/networkInterfaceId&gt;
-- &lt;networkInterfaceOwnerId&gt;053230519467&lt;/networkInterfaceOwnerId&gt;
-- &lt;privateIpAddress&gt;10.0.0.228&lt;/privateIpAddress&gt; &lt;/item&gt;
-- &lt;/addressesSet&gt; &lt;/DescribeAddressesResponse&gt; Example 2 for
-- EC2-VPC This example describes your Elastic IP addresses for EC2-VPC only.
-- https://ec2.amazonaws.com/?Action=DescribeAddresses
-- &amp;Filter.1.Name=domain &amp;Filter.1.Value.1=vpc &amp;AUTHPARAMS.
module Network.AWS.EC2.DescribeAddresses
    (
    -- * Request
      DescribeAddresses
    -- ** Request constructor
    , mkDescribeAddresses
    -- ** Request lenses
    , daPublicIps
    , daFilters
    , daAllocationIds

    -- * Response
    , DescribeAddressesResponse
    -- ** Response constructor
    , mkDescribeAddressesResponse
    -- ** Response lenses
    , darAddresses
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data DescribeAddresses = DescribeAddresses
    { _daPublicIps :: [Text]
    , _daFilters :: [Filter]
    , _daAllocationIds :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeAddresses' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @PublicIps ::@ @[Text]@
--
-- * @Filters ::@ @[Filter]@
--
-- * @AllocationIds ::@ @[Text]@
--
mkDescribeAddresses :: DescribeAddresses
mkDescribeAddresses = DescribeAddresses
    { _daPublicIps = mempty
    , _daFilters = mempty
    , _daAllocationIds = mempty
    }

-- | [EC2-Classic] One or more Elastic IP addresses. Default: Describes all your
-- Elastic IP addresses.
daPublicIps :: Lens' DescribeAddresses [Text]
daPublicIps = lens _daPublicIps (\s a -> s { _daPublicIps = a })

-- | One or more filters. allocation-id - [EC2-VPC] The allocation ID for the
-- address. association-id - [EC2-VPC] The association ID for the address.
-- domain - Indicates whether the address is for use in EC2-Classic (standard)
-- or in a VPC (vpc). instance-id - The ID of the instance the address is
-- associated with, if any. network-interface-id - [EC2-VPC] The ID of the
-- network interface that the address is associated with, if any.
-- network-interface-owner-id - The AWS account ID of the owner.
-- private-ip-address - [EC2-VPC] The private IP address associated with the
-- Elastic IP address. public-ip - The Elastic IP address.
daFilters :: Lens' DescribeAddresses [Filter]
daFilters = lens _daFilters (\s a -> s { _daFilters = a })

-- | [EC2-VPC] One or more allocation IDs. Default: Describes all your Elastic
-- IP addresses.
daAllocationIds :: Lens' DescribeAddresses [Text]
daAllocationIds = lens _daAllocationIds (\s a -> s { _daAllocationIds = a })

instance ToQuery DescribeAddresses where
    toQuery = genericQuery def

newtype DescribeAddressesResponse = DescribeAddressesResponse
    { _darAddresses :: [Address]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeAddressesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Addresses ::@ @[Address]@
--
mkDescribeAddressesResponse :: DescribeAddressesResponse
mkDescribeAddressesResponse = DescribeAddressesResponse
    { _darAddresses = mempty
    }

-- | Information about one or more Elastic IP addresses.
darAddresses :: Lens' DescribeAddressesResponse [Address]
darAddresses = lens _darAddresses (\s a -> s { _darAddresses = a })

instance FromXML DescribeAddressesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeAddresses where
    type Sv DescribeAddresses = EC2
    type Rs DescribeAddresses = DescribeAddressesResponse

    request = post "DescribeAddresses"
    response _ = xmlResponse