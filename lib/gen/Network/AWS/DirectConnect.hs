-- Module      : Network.AWS.DirectConnect
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.DirectConnect
    (
    -- * Operations
    -- ** DescribeInterconnects
      module Network.AWS.DirectConnect.DescribeInterconnects
    -- ** DeleteConnection
    , module Network.AWS.DirectConnect.DeleteConnection
    -- ** CreateConnection
    , module Network.AWS.DirectConnect.CreateConnection
    -- ** DescribeConnections
    , module Network.AWS.DirectConnect.DescribeConnections
    -- ** DescribeConnectionsOnInterconnect
    , module Network.AWS.DirectConnect.DescribeConnectionsOnInterconnect
    -- ** DeleteInterconnect
    , module Network.AWS.DirectConnect.DeleteInterconnect
    -- ** ConfirmPrivateVirtualInterface
    , module Network.AWS.DirectConnect.ConfirmPrivateVirtualInterface
    -- ** DescribeLocations
    , module Network.AWS.DirectConnect.DescribeLocations
    -- ** CreatePublicVirtualInterface
    , module Network.AWS.DirectConnect.CreatePublicVirtualInterface
    -- ** AllocatePrivateVirtualInterface
    , module Network.AWS.DirectConnect.AllocatePrivateVirtualInterface
    -- ** ConfirmConnection
    , module Network.AWS.DirectConnect.ConfirmConnection
    -- ** ConfirmPublicVirtualInterface
    , module Network.AWS.DirectConnect.ConfirmPublicVirtualInterface
    -- ** DescribeVirtualGateways
    , module Network.AWS.DirectConnect.DescribeVirtualGateways
    -- ** DescribeVirtualInterfaces
    , module Network.AWS.DirectConnect.DescribeVirtualInterfaces
    -- ** DeleteVirtualInterface
    , module Network.AWS.DirectConnect.DeleteVirtualInterface
    -- ** CreatePrivateVirtualInterface
    , module Network.AWS.DirectConnect.CreatePrivateVirtualInterface
    -- ** AllocatePublicVirtualInterface
    , module Network.AWS.DirectConnect.AllocatePublicVirtualInterface
    -- ** AllocateConnectionOnInterconnect
    , module Network.AWS.DirectConnect.AllocateConnectionOnInterconnect
    -- ** CreateInterconnect
    , module Network.AWS.DirectConnect.CreateInterconnect

    -- * Types
    -- ** VirtualInterface
    , VirtualInterface (..)
    -- ** VirtualGateway
    , VirtualGateway (..)
    -- ** RouteFilterPrefix
    , RouteFilterPrefix (..)
    -- ** NewPublicVirtualInterfaceAllocation
    , NewPublicVirtualInterfaceAllocation (..)
    -- ** NewPublicVirtualInterface
    , NewPublicVirtualInterface (..)
    -- ** NewPrivateVirtualInterfaceAllocation
    , NewPrivateVirtualInterfaceAllocation (..)
    -- ** NewPrivateVirtualInterface
    , NewPrivateVirtualInterface (..)
    -- ** Location
    , Location (..)
    -- ** Interconnect
    , Interconnect (..)
    -- ** Connection
    , Connection (..)
    -- ** VirtualInterfaceState
    , VirtualInterfaceState (..)
    -- ** InterconnectState
    , InterconnectState (..)
    -- ** ConnectionState
    , ConnectionState (..)

    -- * Errors
    , DirectConnectError (..)
    ) where

import Network.AWS.DirectConnect.Service
import Network.AWS.DirectConnect.Types

import Network.AWS.DirectConnect.DescribeInterconnects
import Network.AWS.DirectConnect.DeleteConnection
import Network.AWS.DirectConnect.CreateConnection
import Network.AWS.DirectConnect.DescribeConnections
import Network.AWS.DirectConnect.DescribeConnectionsOnInterconnect
import Network.AWS.DirectConnect.DeleteInterconnect
import Network.AWS.DirectConnect.ConfirmPrivateVirtualInterface
import Network.AWS.DirectConnect.DescribeLocations
import Network.AWS.DirectConnect.CreatePublicVirtualInterface
import Network.AWS.DirectConnect.AllocatePrivateVirtualInterface
import Network.AWS.DirectConnect.ConfirmConnection
import Network.AWS.DirectConnect.ConfirmPublicVirtualInterface
import Network.AWS.DirectConnect.DescribeVirtualGateways
import Network.AWS.DirectConnect.DescribeVirtualInterfaces
import Network.AWS.DirectConnect.DeleteVirtualInterface
import Network.AWS.DirectConnect.CreatePrivateVirtualInterface
import Network.AWS.DirectConnect.AllocatePublicVirtualInterface
import Network.AWS.DirectConnect.AllocateConnectionOnInterconnect
import Network.AWS.DirectConnect.CreateInterconnect
