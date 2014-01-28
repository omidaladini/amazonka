{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.CreatePublicVirtualInterface
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new public virtual interface. A virtual interface is the VLAN
-- that transports AWS Direct Connect traffic. A public virtual interface
-- supports sending traffic to public services of AWS such as Amazon Simple
-- Storage Service (Amazon S3).
module Network.AWS.DirectConnect.CreatePublicVirtualInterface where

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

data CreatePublicVirtualInterface = CreatePublicVirtualInterface
    { cpvisconnectionId :: !Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    , cpvisnewPublicVirtualInterface :: NewPublicVirtualInterface
      -- ^ Detailed information for the public virtual interface to be created.
      -- Default: None.
    } deriving (Eq, Show, Generic)

instance ToJSON CreatePublicVirtualInterface

instance AWSRequest CreatePublicVirtualInterface where
    type Er CreatePublicVirtualInterface = DirectConnectError
    type Rs CreatePublicVirtualInterface = CreatePublicVirtualInterfaceResponse
    request  = getJSON service
    response = responseJSON

data CreatePublicVirtualInterfaceResponse = CreatePublicVirtualInterfaceResponse
    { cpvisrsamazonAddress :: Maybe Text
      -- ^ IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
    , cpvisrsasn :: Maybe Int
      -- ^ Autonomous system (AS) number for Border Gateway Protocol (BGP)
      -- configuration. Example: 65000.
    , cpvisrsauthKey :: Maybe Text
      -- ^ Authentication key for BGP configuration. Example: asdf34example.
    , cpvisrsconnectionId :: Maybe Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    , cpvisrscustomerAddress :: Maybe Text
      -- ^ IP address assigned to the customer interface. Example: 192.168.1.2/30.
    , cpvisrscustomerRouterConfig :: Maybe Text
      -- ^ Information for generating the customer router configuration.
    , cpvisrslocation :: Maybe Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    , cpvisrsownerAccount :: Maybe Text
    , cpvisrsrouteFilterPrefixes :: [RouteFilterPrefix]
      -- ^ A list of routes to be advertised to the AWS network in this region (public
      -- virtual interface) or your VPC (private virtual interface).
    , cpvisrsvirtualGatewayId :: Maybe Text
      -- ^ The ID of the virtual private gateway to a VPC. This only applies to
      -- private virtual interfaces. Example: vgw-123er56.
    , cpvisrsvirtualInterfaceId :: Maybe Text
      -- ^ ID of the virtual interface. Example: dxvif-123dfg56 Default: None.
    , cpvisrsvirtualInterfaceName :: Maybe Text
      -- ^ The name of the virtual interface assigned by the customer. Example: "My
      -- VPC".
    , cpvisrsvirtualInterfaceState :: Maybe VirtualInterfaceState
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
    , cpvisrsvirtualInterfaceType :: Maybe Text
      -- ^ The type of virtual interface. Example: private (Amazon VPC) or public
      -- (Amazon S3, Amazon DynamoDB, and so on.).
    , cpvisrsvlan :: Maybe Int
      -- ^ The VLAN ID. Example: 101.
    } deriving (Eq, Show, Generic)

instance FromJSON CreatePublicVirtualInterfaceResponse
