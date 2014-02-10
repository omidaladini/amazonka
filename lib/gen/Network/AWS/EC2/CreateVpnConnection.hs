{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateVpnConnection
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new VPN connection between an existing VPN gateway and customer
-- gateway. The only supported connection type is ipsec.1. The response
-- includes information that you need to configure your customer gateway, in
-- XML format. We recommend you use the command line version of this operation
-- (ec2-create-vpn-connection), which takes an -f option (for format) and
-- returns configuration information formatted as expected by the vendor you
-- specified, or in a generic, human readable format. For information about
-- the command, go to ec2-create-vpn-connection in the Amazon Virtual Private
-- Cloud Command Line Reference. We strongly recommend you use HTTPS when
-- calling this operation because the response contains sensitive
-- cryptographic information for configuring your customer gateway. If you
-- decide to shut down your VPN connection for any reason and then create a
-- new one, you must re-configure your customer gateway with the new
-- information returned from this call.
module Network.AWS.EC2.CreateVpnConnection where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
createVpnConnection :: Text
                    -- ^ The ID of the customer gateway.
                    -> Text
                    -- ^ The type of VPN connection.
                    -> Text
                    -- ^ The ID of the VPN gateway.
                    -> CreateVpnConnection
createVpnConnection p1 p2 p3 = CreateVpnConnection
    { cvcCustomerGatewayId = p1
    , cvcType = p2
    , cvcVpnGatewayId = p3
    , cvcDryRun = Nothing
    , cvcOptions = Nothing
    }

data CreateVpnConnection = CreateVpnConnection
    { cvcCustomerGatewayId :: !Text
      -- ^ The ID of the customer gateway.
    , cvcDryRun :: Maybe Bool
    , cvcOptions :: Maybe VpnConnectionOptionsSpecification
    , cvcType :: !Text
      -- ^ The type of VPN connection.
    , cvcVpnGatewayId :: !Text
      -- ^ The ID of the VPN gateway.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateVpnConnection

instance AWSRequest CreateVpnConnection where
    type Er CreateVpnConnection = EC2Error
    type Rs CreateVpnConnection = CreateVpnConnectionResponse
    request  = postQuery service "CreateVpnConnection"
    response = responseXML

data CreateVpnConnectionResponse = CreateVpnConnectionResponse
    { cvcrVpnConnection :: Maybe VpnConnection
    } deriving (Eq, Show, Generic)

instance FromXML CreateVpnConnectionResponse where
    fromXMLOptions = xmlOptions