{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ResetNetworkInterfaceAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for ResetNetworkInterfaceAttribute
module Network.AWS.EC2.ResetNetworkInterfaceAttribute where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
resetNetworkInterfaceAttribute :: Text
                               -> ResetNetworkInterfaceAttribute
resetNetworkInterfaceAttribute p1 = ResetNetworkInterfaceAttribute
    { rniaNetworkInterfaceId = p1
    , rniaDryRun = Nothing
    , rniaSourceDestCheck = Nothing
    }

data ResetNetworkInterfaceAttribute = ResetNetworkInterfaceAttribute
    { rniaDryRun :: Maybe Bool
    , rniaNetworkInterfaceId :: !Text
    , rniaSourceDestCheck :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery ResetNetworkInterfaceAttribute

instance AWSRequest ResetNetworkInterfaceAttribute where
    type Er ResetNetworkInterfaceAttribute = EC2Error
    type Rs ResetNetworkInterfaceAttribute = ResetNetworkInterfaceAttributeResponse
    request  = postQuery service "ResetNetworkInterfaceAttribute"
    response = responseXML

data ResetNetworkInterfaceAttributeResponse = ResetNetworkInterfaceAttributeResponse
    deriving (Eq, Show, Generic)

instance FromXML ResetNetworkInterfaceAttributeResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ResetNetworkInterfaceAttributeResponse"
