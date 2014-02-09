{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.UnassignPrivateIpAddresses
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for UnassignPrivateIpAddresses
module Network.AWS.EC2.UnassignPrivateIpAddresses where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
unassignPrivateIpAddresses :: Text
                           -> [Text]
                           -> UnassignPrivateIpAddresses
unassignPrivateIpAddresses p1 p2 = UnassignPrivateIpAddresses
    { upiaNetworkInterfaceId = p1
    , upiaPrivateIpAddresses = p2
    }

data UnassignPrivateIpAddresses = UnassignPrivateIpAddresses
    { upiaNetworkInterfaceId :: !Text
    , upiaPrivateIpAddresses :: [Text]
    } deriving (Eq, Show, Generic)

instance ToQuery UnassignPrivateIpAddresses

instance AWSRequest UnassignPrivateIpAddresses where
    type Er UnassignPrivateIpAddresses = EC2Error
    type Rs UnassignPrivateIpAddresses = UnassignPrivateIpAddressesResponse
    request = getQuery service "UnassignPrivateIpAddresses"

data UnassignPrivateIpAddressesResponse = UnassignPrivateIpAddressesResponse
    deriving (Eq, Show, Generic)

instance FromXML UnassignPrivateIpAddressesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "UnassignPrivateIpAddressesResponse"
