{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateNetworkInterface
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for CreateNetworkInterface
module Network.AWS.EC2.CreateNetworkInterface where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
createNetworkInterface :: Text
                       -> CreateNetworkInterface
createNetworkInterface p1 = CreateNetworkInterface
    { cniSubnetId = p1
    , cniDescription = Nothing
    , cniDryRun = Nothing
    , cniGroups = []
    , cniPrivateIpAddress = Nothing
    , cniPrivateIpAddresses = []
    , cniSecondaryPrivateIpAddressCount = Nothing
    }

data CreateNetworkInterface = CreateNetworkInterface
    { cniDescription :: Maybe Text
    , cniDryRun :: Maybe Bool
    , cniGroups :: [Text]
    , cniPrivateIpAddress :: Maybe Text
    , cniPrivateIpAddresses :: [PrivateIpAddressSpecification]
    , cniSecondaryPrivateIpAddressCount :: Maybe Int
    , cniSubnetId :: !Text
    } deriving (Eq, Show, Generic)

instance ToQuery CreateNetworkInterface

instance AWSRequest CreateNetworkInterface where
    type Er CreateNetworkInterface = EC2Error
    type Rs CreateNetworkInterface = CreateNetworkInterfaceResponse
    request = getQuery service "CreateNetworkInterface"

data CreateNetworkInterfaceResponse = CreateNetworkInterfaceResponse
    { cnirNetworkInterface :: Maybe NetworkInterface
      -- ^ Specifies the characteristics of a network interface.
    } deriving (Eq, Show, Generic)

instance FromXML CreateNetworkInterfaceResponse where
    fromXMLOptions = xmlOptions
