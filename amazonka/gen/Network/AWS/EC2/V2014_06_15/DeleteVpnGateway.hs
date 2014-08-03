{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DeleteVpnGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified virtual private gateway. We recommend that before you
-- delete a virtual private gateway, you detach it from the VPC and delete the
-- VPN connection. Note that you don't need to delete the virtual private
-- gateway if you plan to delete and recreate the VPN connection between your
-- VPC and your network. Example This example deletes the specified virtual
-- private gateway. https://ec2.amazonaws.com/?Action=DeleteVpnGateway
-- &amp;vpnGatewayId=vgw-8db04f81 &amp;AUTHPARAMS &lt;DeleteVpnGatewayResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteVpnGatewayResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DeleteVpnGateway where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteVpnGateway' request.
deleteVpnGateway :: Text -- ^ '_dvgrVpnGatewayId'
                 -> DeleteVpnGateway
deleteVpnGateway p1 = DeleteVpnGateway
    { _dvgrVpnGatewayId = p1
    , _dvgrDryRun = Nothing
    }

data DeleteVpnGateway = DeleteVpnGateway
    { _dvgrVpnGatewayId :: Text
      -- ^ The ID of the virtual private gateway.
    , _dvgrDryRun :: Maybe Bool
      -- ^ 
    } deriving (Generic)

makeLenses ''DeleteVpnGateway

instance ToQuery DeleteVpnGateway where
    toQuery = genericToQuery def

data DeleteVpnGatewayResponse = DeleteVpnGatewayResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteVpnGatewayResponse

instance AWSRequest DeleteVpnGateway where
    type Sv DeleteVpnGateway = EC2
    type Rs DeleteVpnGateway = DeleteVpnGatewayResponse

    request = post "DeleteVpnGateway"
    response _ _ = return (Right DeleteVpnGatewayResponse)