{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteVpnConnectionRoute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for DeleteVpnConnectionRoute
module Network.AWS.EC2.DeleteVpnConnectionRoute where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteVpnConnectionRoute :: Text
                         -> Text
                         -> DeleteVpnConnectionRoute
deleteVpnConnectionRoute p1 p2 = DeleteVpnConnectionRoute
    { dvcrDestinationCidrBlock = p1
    , dvcrVpnConnectionId = p2
    }

data DeleteVpnConnectionRoute = DeleteVpnConnectionRoute
    { dvcrDestinationCidrBlock :: !Text
    , dvcrVpnConnectionId :: !Text
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteVpnConnectionRoute

instance AWSRequest DeleteVpnConnectionRoute where
    type Er DeleteVpnConnectionRoute = EC2Error
    type Rs DeleteVpnConnectionRoute = DeleteVpnConnectionRouteResponse
    request = getQuery service "DeleteVpnConnectionRoute"

data DeleteVpnConnectionRouteResponse = DeleteVpnConnectionRouteResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteVpnConnectionRouteResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteVpnConnectionRouteResponse"
