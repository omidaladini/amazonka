{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DeleteVpnConnectionRoute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified static route associated with a VPN connection between
-- an existing virtual private gateway and a VPN customer gateway. The static
-- route allows traffic to be routed from the virtual private gateway to the
-- VPN customer gateway. Example This example deletes a static route to the
-- destination CIDR block 11.12.0.0/16 associated with the VPN connection with
-- the ID vpn-83ad48ea. Note that when using the Query API, the "/" is denoted
-- as "%2F". https://ec2.amazonaws.com/?Action=DeleteVpnConnectionRoute
-- &amp;DestinationCidrBlock=11.12.0.0%2F16 &amp;VpnConnectionId=vpn-83ad48ea
-- &amp;AUTHPARAMS &lt;DeleteVpnConnectionRouteResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;4f35a1b2-c2c3-4093-b51f-abb9d7311990&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;/DeleteVpnConnectionRouteResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DeleteVpnConnectionRoute
    (
    -- * Request
      DeleteVpnConnectionRoute
    -- ** Request constructor
    , deleteVpnConnectionRoute
    -- ** Request lenses
    , dvcrrVpnConnectionId
    , dvcrrDestinationCidrBlock

    -- * Response
    , DeleteVpnConnectionRouteResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteVpnConnectionRoute' request.
deleteVpnConnectionRoute :: Text -- ^ 'dvcrrVpnConnectionId'
                         -> Text -- ^ 'dvcrrDestinationCidrBlock'
                         -> DeleteVpnConnectionRoute
deleteVpnConnectionRoute p1 p2 = DeleteVpnConnectionRoute
    { _dvcrrVpnConnectionId = p1
    , _dvcrrDestinationCidrBlock = p2
    }

data DeleteVpnConnectionRoute = DeleteVpnConnectionRoute
    { _dvcrrVpnConnectionId :: Text
      -- ^ The ID of the VPN connection.
    , _dvcrrDestinationCidrBlock :: Text
      -- ^ The CIDR block associated with the local subnet of the customer
      -- network.
    } deriving (Show, Generic)

-- | The ID of the VPN connection.
dvcrrVpnConnectionId
    :: Functor f
    => (Text
    -> f (Text))
    -> DeleteVpnConnectionRoute
    -> f DeleteVpnConnectionRoute
dvcrrVpnConnectionId f x =
    (\y -> x { _dvcrrVpnConnectionId = y })
       <$> f (_dvcrrVpnConnectionId x)
{-# INLINE dvcrrVpnConnectionId #-}

-- | The CIDR block associated with the local subnet of the customer network.
dvcrrDestinationCidrBlock
    :: Functor f
    => (Text
    -> f (Text))
    -> DeleteVpnConnectionRoute
    -> f DeleteVpnConnectionRoute
dvcrrDestinationCidrBlock f x =
    (\y -> x { _dvcrrDestinationCidrBlock = y })
       <$> f (_dvcrrDestinationCidrBlock x)
{-# INLINE dvcrrDestinationCidrBlock #-}

instance ToQuery DeleteVpnConnectionRoute where
    toQuery = genericQuery def

data DeleteVpnConnectionRouteResponse = DeleteVpnConnectionRouteResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteVpnConnectionRoute where
    type Sv DeleteVpnConnectionRoute = EC2
    type Rs DeleteVpnConnectionRoute = DeleteVpnConnectionRouteResponse

    request = post "DeleteVpnConnectionRoute"
    response _ = nullaryResponse DeleteVpnConnectionRouteResponse
