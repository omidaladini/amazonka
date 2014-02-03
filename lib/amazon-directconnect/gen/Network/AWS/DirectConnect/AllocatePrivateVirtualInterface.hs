{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.AllocatePrivateVirtualInterface
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Provisions a private virtual interface to be owned by a different customer.
-- The owner of a connection calls this function to provision a private
-- virtual interface which will be owned by another AWS customer. Virtual
-- interfaces created using this function must be confirmed by the virtual
-- interface owner by calling ConfirmPrivateVirtualInterface. Until this step
-- has been completed, the virtual interface will be in 'Confirming' state,
-- and will not be available for handling traffic.
module Network.AWS.DirectConnect.AllocatePrivateVirtualInterface where

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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
allocatePrivateVirtualInterface :: Text
                                -> NewPrivateVirtualInterfaceAllocation
                                -> Text
                                -> AllocatePrivateVirtualInterface
allocatePrivateVirtualInterface p1 p2 p3 = AllocatePrivateVirtualInterface
    { apvirConnectionId = p1
    , apvirNewPrivateVirtualInterfaceAllocation = p2
    , apvirOwnerAccount = p3
    }

data AllocatePrivateVirtualInterface = AllocatePrivateVirtualInterface
    { apvirConnectionId :: !Text
      -- ^ The connection ID on which the private virtual interface is provisioned.
      -- Default: None.
    , apvirNewPrivateVirtualInterfaceAllocation :: NewPrivateVirtualInterfaceAllocation
      -- ^ Detailed information for the private virtual interface to be provisioned.
      -- Default: None.
    , apvirOwnerAccount :: !Text
      -- ^ The AWS account that will own the new private virtual interface. Default:
      -- None.
    } deriving (Eq, Show, Generic)

instance ToJSON AllocatePrivateVirtualInterface where
    toJSON = genericToJSON jsonOptions

instance AWSRequest AllocatePrivateVirtualInterface where
    type Er AllocatePrivateVirtualInterface = DirectConnectError
    type Rs AllocatePrivateVirtualInterface = AllocatePrivateVirtualInterfaceResponse
    request  = getJSON service
    response = responseJSON

data AllocatePrivateVirtualInterfaceResponse = AllocatePrivateVirtualInterfaceResponse
    { apvirrsAmazonAddress :: Maybe Text
      -- ^ IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
    , apvirrsAsn :: Maybe Int
      -- ^ Autonomous system (AS) number for Border Gateway Protocol (BGP)
      -- configuration. Example: 65000.
    , apvirrsAuthKey :: Maybe Text
      -- ^ Authentication key for BGP configuration. Example: asdf34example.
    , apvirrsConnectionId :: Maybe Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    , apvirrsCustomerAddress :: Maybe Text
      -- ^ IP address assigned to the customer interface. Example: 192.168.1.2/30.
    , apvirrsCustomerRouterConfig :: Maybe Text
      -- ^ Information for generating the customer router configuration.
    , apvirrsLocation :: Maybe Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    , apvirrsOwnerAccount :: Maybe Text
    , apvirrsRouteFilterPrefixes :: [RouteFilterPrefix]
      -- ^ A list of routes to be advertised to the AWS network in this region (public
      -- virtual interface) or your VPC (private virtual interface).
    , apvirrsVirtualGatewayId :: Maybe Text
      -- ^ The ID of the virtual private gateway to a VPC. This only applies to
      -- private virtual interfaces. Example: vgw-123er56.
    , apvirrsVirtualInterfaceId :: Maybe Text
      -- ^ ID of the virtual interface. Example: dxvif-123dfg56 Default: None.
    , apvirrsVirtualInterfaceName :: Maybe Text
      -- ^ The name of the virtual interface assigned by the customer. Example: "My
      -- VPC".
    , apvirrsVirtualInterfaceState :: Maybe VirtualInterfaceState
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
    , apvirrsVirtualInterfaceType :: Maybe Text
      -- ^ The type of virtual interface. Example: private (Amazon VPC) or public
      -- (Amazon S3, Amazon DynamoDB, and so on.).
    , apvirrsVlan :: Maybe Int
      -- ^ The VLAN ID. Example: 101.
    } deriving (Eq, Show, Generic)

instance FromJSON AllocatePrivateVirtualInterfaceResponse where
    fromJSON = genericFromJSON jsonOptions

