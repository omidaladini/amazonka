{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.AssignPrivateIpAddresses
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Assigns one or more secondary private IP addresses to the specified network
-- interface. You can specify one or more specific secondary IP addresses, or
-- you can specify the number of secondary IP addresses to be automatically
-- assigned within the subnet's CIDR block range. The number of secondary IP
-- addresses that you can assign to an instance varies by instance type. For
-- information about instance types, see Instance Types in the Amazon Elastic
-- Compute Cloud User Guide. For more information about Elastic IP addresses,
-- see Elastic IP Addresses in the Amazon Elastic Compute Cloud User Guide.
-- AssignPrivateIpAddresses is available only in EC2-VPC. Example 1 This
-- example assigns two secondary private IP addresses (10.0.2.1 and 10.0.2.11)
-- to the specified network interface.
-- https://ec2.amazonaws.com/?Action=AssignPrivateIpAddresses
-- &amp;NetworkInterfaceId=eni-d83388b1 &amp;PrivateIpAddress.0=10.0.2.1
-- &amp;PrivateIpAddress.1=10.0.2.11 &amp;AUTHPARAMS
-- &lt;AssignPrivateIpAddresses
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/AssignPrivateIpAddresses&gt; Example
-- 2 This example assigns two secondary private IP addresses to the specified
-- network interface. Amazon EC2 automatically assigns these IP addresses from
-- the available IP addresses within the subnet's CIDR block range.
-- https://ec2.amazonaws.com/?Action=AssignPrivateIpAddresses
-- &amp;NetworkInterfaceId=eni-d83388b1 &amp;SecondaryPrivateIpAddressCount=2
-- &amp;AUTHPARAMS &lt;AssignPrivateIpAddresses
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/AssignPrivateIpAddresses&gt;.
module Network.AWS.EC2.AssignPrivateIpAddresses
    (
    -- * Request
      AssignPrivateIpAddresses
    -- ** Request constructor
    , mkAssignPrivateIpAddresses
    -- ** Request lenses
    , apiaNetworkInterfaceId
    , apiaPrivateIpAddresses
    , apiaSecondaryPrivateIpAddressCount
    , apiaAllowReassignment

    -- * Response
    , AssignPrivateIpAddressesResponse
    -- ** Response constructor
    , mkAssignPrivateIpAddressesResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data AssignPrivateIpAddresses = AssignPrivateIpAddresses
    { _apiaNetworkInterfaceId :: !Text
    , _apiaPrivateIpAddresses :: [Text]
    , _apiaSecondaryPrivateIpAddressCount :: !(Maybe Integer)
    , _apiaAllowReassignment :: !(Maybe Bool)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AssignPrivateIpAddresses' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @NetworkInterfaceId ::@ @Text@
--
-- * @PrivateIpAddresses ::@ @[Text]@
--
-- * @SecondaryPrivateIpAddressCount ::@ @Maybe Integer@
--
-- * @AllowReassignment ::@ @Maybe Bool@
--
mkAssignPrivateIpAddresses :: Text -- ^ 'apiaNetworkInterfaceId'
                           -> AssignPrivateIpAddresses
mkAssignPrivateIpAddresses p1 = AssignPrivateIpAddresses
    { _apiaNetworkInterfaceId = p1
    , _apiaPrivateIpAddresses = mempty
    , _apiaSecondaryPrivateIpAddressCount = Nothing
    , _apiaAllowReassignment = Nothing
    }

-- | The ID of the network interface.
apiaNetworkInterfaceId :: Lens' AssignPrivateIpAddresses Text
apiaNetworkInterfaceId =
    lens _apiaNetworkInterfaceId (\s a -> s { _apiaNetworkInterfaceId = a })

-- | One or more IP addresses to be assigned as a secondary private IP address
-- to the network interface. If you don't specify an IP address, Amazon EC2
-- automatically selects an IP address within the subnet range.
apiaPrivateIpAddresses :: Lens' AssignPrivateIpAddresses [Text]
apiaPrivateIpAddresses =
    lens _apiaPrivateIpAddresses (\s a -> s { _apiaPrivateIpAddresses = a })

-- | The number of secondary IP addresses to assign to the network interface.
apiaSecondaryPrivateIpAddressCount :: Lens' AssignPrivateIpAddresses (Maybe Integer)
apiaSecondaryPrivateIpAddressCount =
    lens _apiaSecondaryPrivateIpAddressCount
         (\s a -> s { _apiaSecondaryPrivateIpAddressCount = a })

-- | Indicates whether to allow an IP address that is already assigned to
-- another network interface or instance to be reassigned to the specified
-- network interface.
apiaAllowReassignment :: Lens' AssignPrivateIpAddresses (Maybe Bool)
apiaAllowReassignment =
    lens _apiaAllowReassignment (\s a -> s { _apiaAllowReassignment = a })

instance ToQuery AssignPrivateIpAddresses where
    toQuery = genericQuery def

data AssignPrivateIpAddressesResponse = AssignPrivateIpAddressesResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AssignPrivateIpAddressesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkAssignPrivateIpAddressesResponse :: AssignPrivateIpAddressesResponse
mkAssignPrivateIpAddressesResponse = AssignPrivateIpAddressesResponse

instance AWSRequest AssignPrivateIpAddresses where
    type Sv AssignPrivateIpAddresses = EC2
    type Rs AssignPrivateIpAddresses = AssignPrivateIpAddressesResponse

    request = post "AssignPrivateIpAddresses"
    response _ = nullaryResponse AssignPrivateIpAddressesResponse