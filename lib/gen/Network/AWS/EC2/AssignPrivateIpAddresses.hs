{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
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

-- FIXME: Operation documentation for AssignPrivateIpAddresses
module Network.AWS.EC2.AssignPrivateIpAddresses where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
assignPrivateIpAddresses :: Text
                         -> AssignPrivateIpAddresses
assignPrivateIpAddresses p1 = AssignPrivateIpAddresses
    { apiarNetworkInterfaceId = p1
    , apiarAllowReassignment = Nothing
    , apiarPrivateIpAddresses = []
    , apiarSecondaryPrivateIpAddressCount = Nothing
    }

data AssignPrivateIpAddresses = AssignPrivateIpAddresses
    { apiarAllowReassignment :: Maybe Bool
    , apiarNetworkInterfaceId :: !Text
    , apiarPrivateIpAddresses :: [Text]
    , apiarSecondaryPrivateIpAddressCount :: Maybe Int
    } deriving (Eq, Show, Generic)

instance ToQuery AssignPrivateIpAddresses

instance AWSRequest AssignPrivateIpAddresses where
    type Er AssignPrivateIpAddresses = EC2Error
    type Rs AssignPrivateIpAddresses = AssignPrivateIpAddressesResponse
    request = getQuery service "AssignPrivateIpAddresses"

data AssignPrivateIpAddressesResponse = AssignPrivateIpAddressesResponse
    deriving (Eq, Show, Generic)

instance FromXML AssignPrivateIpAddressesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AssignPrivateIpAddressesResponse"
