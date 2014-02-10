{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateVpnConnectionRoute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for CreateVpnConnectionRoute
module Network.AWS.EC2.CreateVpnConnectionRoute where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data CreateVpnConnectionRoute = CreateVpnConnectionRoute
    { cvcrdDestinationCidrBlock :: !Text
    , cvcrdVpnConnectionId :: !Text
    } deriving (Eq, Show, Generic)

instance ToQuery CreateVpnConnectionRoute

instance AWSRequest CreateVpnConnectionRoute where
    type Er CreateVpnConnectionRoute = EC2Error
    type Rs CreateVpnConnectionRoute = CreateVpnConnectionRouteResponse
    request  = postQuery service "CreateVpnConnectionRoute"
    response = responseXML

data CreateVpnConnectionRouteResponse = CreateVpnConnectionRouteResponse
    deriving (Eq, Show, Generic)

instance FromXML CreateVpnConnectionRouteResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateVpnConnectionRouteResponse"