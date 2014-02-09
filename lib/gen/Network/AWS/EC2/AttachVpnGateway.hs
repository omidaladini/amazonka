{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.AttachVpnGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Attaches a VPN gateway to a VPC. This is the last step required to get your
-- VPC fully connected to your data center before launching instances in it.
-- For more information, go to Process for Using Amazon VPC in the Amazon
-- Virtual Private Cloud Developer Guide.
module Network.AWS.EC2.AttachVpnGateway where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
attachVpnGateway :: Text
                 -- ^ The ID of the VPC to attach to the VPN gateway.
                 -> Text
                 -- ^ The ID of the VPN gateway to attach to the VPC.
                 -> AttachVpnGateway
attachVpnGateway p1 p2 = AttachVpnGateway
    { avgVpcId = p1
    , avgVpnGatewayId = p2
    , avgDryRun = Nothing
    }

data AttachVpnGateway = AttachVpnGateway
    { avgDryRun :: Maybe Bool
    , avgVpcId :: !Text
      -- ^ The ID of the VPC to attach to the VPN gateway.
    , avgVpnGatewayId :: !Text
      -- ^ The ID of the VPN gateway to attach to the VPC.
    } deriving (Eq, Show, Generic)

instance ToQuery AttachVpnGateway

instance AWSRequest AttachVpnGateway where
    type Er AttachVpnGateway = EC2Error
    type Rs AttachVpnGateway = AttachVpnGatewayResponse
    request  = postQuery service "AttachVpnGateway"
    response = responseXML

data AttachVpnGatewayResponse = AttachVpnGatewayResponse
    { avgrVpcAttachment :: Maybe VpcAttachment
    } deriving (Eq, Show, Generic)

instance FromXML AttachVpnGatewayResponse where
    fromXMLOptions = xmlOptions
