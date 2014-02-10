{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DetachVpnGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Detaches a VPN gateway from a VPC. You do this if you're planning to turn
-- off the VPC and not use it anymore. You can confirm a VPN gateway has been
-- completely detached from a VPC by describing the VPN gateway (any
-- attachments to the VPN gateway are also described). You must wait for the
-- attachment's state to switch to detached before you can delete the VPC or
-- attach a different VPC to the VPN gateway.
module Network.AWS.EC2.DetachVpnGateway where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
detachVpnGateway :: Text
                 -- ^ The ID of the VPC to detach the VPN gateway from.
                 -> Text
                 -- ^ The ID of the VPN gateway to detach from the VPC.
                 -> DetachVpnGateway
detachVpnGateway p1 p2 = DetachVpnGateway
    { dvgdVpcId = p1
    , dvgdVpnGatewayId = p2
    , dvgdDryRun = Nothing
    }

data DetachVpnGateway = DetachVpnGateway
    { dvgdDryRun :: Maybe Bool
    , dvgdVpcId :: !Text
      -- ^ The ID of the VPC to detach the VPN gateway from.
    , dvgdVpnGatewayId :: !Text
      -- ^ The ID of the VPN gateway to detach from the VPC.
    } deriving (Eq, Show, Generic)

instance ToQuery DetachVpnGateway

instance AWSRequest DetachVpnGateway where
    type Er DetachVpnGateway = EC2Error
    type Rs DetachVpnGateway = DetachVpnGatewayResponse
    request  = postQuery service "DetachVpnGateway"
    response = responseXML

data DetachVpnGatewayResponse = DetachVpnGatewayResponse
    deriving (Eq, Show, Generic)

instance FromXML DetachVpnGatewayResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DetachVpnGatewayResponse"