{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.CreatePrivateVirtualInterface
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new private virtual interface. A virtual interface is the VLAN
-- that transports AWS Direct Connect traffic. A private virtual interface
-- supports sending traffic to a single virtual private cloud (VPC).
module Network.AWS.DirectConnect.CreatePrivateVirtualInterface where

import           Control.Monad
import           Data.Aeson
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.DirectConnect.Service
import Network.AWS.DirectConnect.Types

-- | Convenience method utilising default fields where applicable.
createPrivateVirtualInterface :: Text
                              -> NewPrivateVirtualInterface
                              -> AWS (Either DirectConnectError CreatePrivateVirtualInterfaceResponse)
createPrivateVirtualInterface p1 p2 = undefined $ CreatePrivateVirtualInterface
    { cpviuConnectionId = p1
    , cpviuNewPrivateVirtualInterface = p2
    }

data CreatePrivateVirtualInterface = CreatePrivateVirtualInterface
    { cpviuConnectionId :: !Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    , cpviuNewPrivateVirtualInterface :: NewPrivateVirtualInterface
      -- ^ Detailed information for the private virtual interface to be created.
      -- Default: None.
    } deriving (Eq, Show, Generic)

instance ToJSON CreatePrivateVirtualInterface

instance AWSRequest CreatePrivateVirtualInterface where
    type Er CreatePrivateVirtualInterface = DirectConnectError
    type Rs CreatePrivateVirtualInterface = CreatePrivateVirtualInterfaceResponse
    request  = getJSON service
    response = responseJSON

data CreatePrivateVirtualInterfaceResponse = CreatePrivateVirtualInterfaceResponse
    { cpviursAmazonAddress :: Maybe Text
      -- ^ IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
    , cpviursAsn :: Maybe Int
      -- ^ Autonomous system (AS) number for Border Gateway Protocol (BGP)
      -- configuration. Example: 65000.
    , cpviursAuthKey :: Maybe Text
      -- ^ Authentication key for BGP configuration. Example: asdf34example.
    , cpviursConnectionId :: Maybe Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    , cpviursCustomerAddress :: Maybe Text
      -- ^ IP address assigned to the customer interface. Example: 192.168.1.2/30.
    , cpviursCustomerRouterConfig :: Maybe Text
      -- ^ Information for generating the customer router configuration.
    , cpviursLocation :: Maybe Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    , cpviursOwnerAccount :: Maybe Text
    , cpviursRouteFilterPrefixes :: [RouteFilterPrefix]
      -- ^ A list of routes to be advertised to the AWS network in this region (public
      -- virtual interface) or your VPC (private virtual interface).
    , cpviursVirtualGatewayId :: Maybe Text
      -- ^ The ID of the virtual private gateway to a VPC. This only applies to
      -- private virtual interfaces. Example: vgw-123er56.
    , cpviursVirtualInterfaceId :: Maybe Text
      -- ^ ID of the virtual interface. Example: dxvif-123dfg56 Default: None.
    , cpviursVirtualInterfaceName :: Maybe Text
      -- ^ The name of the virtual interface assigned by the customer. Example: "My
      -- VPC".
    , cpviursVirtualInterfaceState :: Maybe VirtualInterfaceState
      -- ^ State of the virtual interface. Confirming: The creation of the virtual
      -- interface is pending confirmation from the virtual interface owner. If the
      -- owner of the virtual interface is different from the owner of the
      -- connection on which it is provisioned, then the virtual interface will
      -- remain in this state until it is confirmed by the virtual interface owner.
      -- Verifying: This state only applies to public virtual interfaces. Each
      -- public virtual interface needs validation before the virtual interface can
      -- be created. Pending: A virtual interface is in this state from the time
      -- that it is created until the virtual interface is ready to forward traffic.
      -- Available: A virtual interface that is able to forward traffic. Deleting: A
      -- virtual interface is in this state immediately after calling
      -- DeleteVirtualInterface until it can no longer forward traffic. Deleted: A
      -- virtual interface that cannot forward traffic. Rejected: The virtual
      -- interface owner has declined creation of the virtual interface. If a
      -- virtual interface in the 'Confirming' state is deleted by the virtual
      -- interface owner, the virtual interface will enter the 'Rejected' state.
    , cpviursVirtualInterfaceType :: Maybe Text
      -- ^ The type of virtual interface. Example: private (Amazon VPC) or public
      -- (Amazon S3, Amazon DynamoDB, and so on.).
    , cpviursVlan :: Maybe Int
      -- ^ The VLAN ID. Example: 101.
    } deriving (Eq, Show, Generic)

instance FromJSON CreatePrivateVirtualInterfaceResponse
