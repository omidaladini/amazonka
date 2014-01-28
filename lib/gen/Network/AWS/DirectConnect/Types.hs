{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.DirectConnect.Types where

import Data.Aeson
import Data.ByteString      (ByteString)
import Data.Hashable
import Data.Monoid
import Data.List.NonEmpty
import Data.Text            (Text)
import Data.HashMap.Strict  (HashMap)
import Data.Time
import GHC.Generics         (Generic)
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.DirectConnect.Service

-- | A virtual interface (VLAN) transmits the traffic between the AWS Direct
-- Connect location and the customer.
data VirtualInterface = VirtualInterface
    { viamazonAddress :: Maybe Text
      -- ^ IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
    , viasn :: Maybe Int
      -- ^ Autonomous system (AS) number for Border Gateway Protocol (BGP)
      -- configuration. Example: 65000.
    , viauthKey :: Maybe Text
      -- ^ Authentication key for BGP configuration. Example: asdf34example.
    , viconnectionId :: Maybe Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    , vicustomerAddress :: Maybe Text
      -- ^ IP address assigned to the customer interface. Example: 192.168.1.2/30.
    , vicustomerRouterConfig :: Maybe Text
      -- ^ Information for generating the customer router configuration.
    , vilocation :: Maybe Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    , viownerAccount :: Maybe Text
    , virouteFilterPrefixes :: [RouteFilterPrefix]
      -- ^ A list of routes to be advertised to the AWS network in this region (public
      -- virtual interface) or your VPC (private virtual interface).
    , vivirtualGatewayId :: Maybe Text
      -- ^ The ID of the virtual private gateway to a VPC. This only applies to
      -- private virtual interfaces. Example: vgw-123er56.
    , vivirtualInterfaceId :: Maybe Text
      -- ^ ID of the virtual interface. Example: dxvif-123dfg56 Default: None.
    , vivirtualInterfaceName :: Maybe Text
      -- ^ The name of the virtual interface assigned by the customer. Example: "My
      -- VPC".
    , vivirtualInterfaceState :: Maybe VirtualInterfaceState
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
    , vivirtualInterfaceType :: Maybe Text
      -- ^ The type of virtual interface. Example: private (Amazon VPC) or public
      -- (Amazon S3, Amazon DynamoDB, and so on.).
    , vivlan :: Maybe Int
      -- ^ The VLAN ID. Example: 101.
    } deriving (Eq, Show, Generic)

instance FromJSON VirtualInterface
instance ToJSON VirtualInterface

-- | You can create one or more AWS Direct Connect private virtual interfaces
-- linking to your virtual private gateway. Virtual private gateways can be
-- managed using the Amazon Virtual Private Cloud (Amazon VPC) console or the
-- Amazon EC2 CreateVpnGateway action.
data VirtualGateway = VirtualGateway
    { vhvirtualGatewayId :: Maybe Text
      -- ^ The ID of the virtual private gateway to a VPC. This only applies to
      -- private virtual interfaces. Example: vgw-123er56.
    , vhvirtualGatewayState :: Maybe Text
      -- ^ State of the virtual private gateway. Pending: This is the initial state
      -- after calling CreateVpnGateway. Available: Ready for use by a private
      -- virtual interface. Deleting: This is the initial state after calling
      -- DeleteVpnGateway. Deleted: In this state, a private virtual interface is
      -- unable to send traffic over this gateway.
    } deriving (Eq, Show, Generic)

instance FromJSON VirtualGateway
instance ToJSON VirtualGateway

-- | A route filter prefix that the customer can advertise through Border
-- Gateway Protocol (BGP) over a public virtual interface.
newtype RouteFilterPrefix = RouteFilterPrefix
    { rfpcidr :: Maybe Text
      -- ^ CIDR notation for the advertised route. Multiple routes are separated by
      -- commas. Example: 10.10.10.0/24,10.10.11.0/24.
    } deriving (Eq, Show, Generic)

instance FromJSON RouteFilterPrefix
instance ToJSON RouteFilterPrefix

-- | Detailed information for the public virtual interface to be provisioned.
-- Default: None.
data NewPublicVirtualInterfaceAllocation = NewPublicVirtualInterfaceAllocation
    { npvibamazonAddress :: !Text
      -- ^ IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
    , npvibasn :: !Int
      -- ^ Autonomous system (AS) number for Border Gateway Protocol (BGP)
      -- configuration. Example: 65000.
    , npvibauthKey :: Maybe Text
      -- ^ Authentication key for BGP configuration. Example: asdf34example.
    , npvibcustomerAddress :: !Text
      -- ^ IP address assigned to the customer interface. Example: 192.168.1.2/30.
    , npvibrouteFilterPrefixes :: [RouteFilterPrefix]
      -- ^ A list of routes to be advertised to the AWS network in this region (public
      -- virtual interface) or your VPC (private virtual interface).
    , npvibvirtualInterfaceName :: !Text
      -- ^ The name of the virtual interface assigned by the customer. Example: "My
      -- VPC".
    , npvibvlan :: !Int
      -- ^ The VLAN ID. Example: 101.
    } deriving (Eq, Show, Generic)

instance FromJSON NewPublicVirtualInterfaceAllocation
instance ToJSON NewPublicVirtualInterfaceAllocation

-- | Detailed information for the public virtual interface to be created.
-- Default: None.
data NewPublicVirtualInterface = NewPublicVirtualInterface
    { npvjamazonAddress :: !Text
      -- ^ IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
    , npvjasn :: !Int
      -- ^ Autonomous system (AS) number for Border Gateway Protocol (BGP)
      -- configuration. Example: 65000.
    , npvjauthKey :: Maybe Text
      -- ^ Authentication key for BGP configuration. Example: asdf34example.
    , npvjcustomerAddress :: !Text
      -- ^ IP address assigned to the customer interface. Example: 192.168.1.2/30.
    , npvjrouteFilterPrefixes :: [RouteFilterPrefix]
      -- ^ A list of routes to be advertised to the AWS network in this region (public
      -- virtual interface) or your VPC (private virtual interface).
    , npvjvirtualInterfaceName :: !Text
      -- ^ The name of the virtual interface assigned by the customer. Example: "My
      -- VPC".
    , npvjvlan :: !Int
      -- ^ The VLAN ID. Example: 101.
    } deriving (Eq, Show, Generic)

instance FromJSON NewPublicVirtualInterface
instance ToJSON NewPublicVirtualInterface

-- | Detailed information for the private virtual interface to be provisioned.
-- Default: None.
data NewPrivateVirtualInterfaceAllocation = NewPrivateVirtualInterfaceAllocation
    { npviaamazonAddress :: Maybe Text
      -- ^ IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
    , npviaasn :: !Int
      -- ^ Autonomous system (AS) number for Border Gateway Protocol (BGP)
      -- configuration. Example: 65000.
    , npviaauthKey :: Maybe Text
      -- ^ Authentication key for BGP configuration. Example: asdf34example.
    , npviacustomerAddress :: Maybe Text
      -- ^ IP address assigned to the customer interface. Example: 192.168.1.2/30.
    , npviavirtualInterfaceName :: !Text
      -- ^ The name of the virtual interface assigned by the customer. Example: "My
      -- VPC".
    , npviavlan :: !Int
      -- ^ The VLAN ID. Example: 101.
    } deriving (Eq, Show, Generic)

instance FromJSON NewPrivateVirtualInterfaceAllocation
instance ToJSON NewPrivateVirtualInterfaceAllocation

-- | Detailed information for the private virtual interface to be created.
-- Default: None.
data NewPrivateVirtualInterface = NewPrivateVirtualInterface
    { npviamazonAddress :: Maybe Text
      -- ^ IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
    , npviasn :: !Int
      -- ^ Autonomous system (AS) number for Border Gateway Protocol (BGP)
      -- configuration. Example: 65000.
    , npviauthKey :: Maybe Text
      -- ^ Authentication key for BGP configuration. Example: asdf34example.
    , npvicustomerAddress :: Maybe Text
      -- ^ IP address assigned to the customer interface. Example: 192.168.1.2/30.
    , npvivirtualGatewayId :: !Text
      -- ^ The ID of the virtual private gateway to a VPC. This only applies to
      -- private virtual interfaces. Example: vgw-123er56.
    , npvivirtualInterfaceName :: !Text
      -- ^ The name of the virtual interface assigned by the customer. Example: "My
      -- VPC".
    , npvivlan :: !Int
      -- ^ The VLAN ID. Example: 101.
    } deriving (Eq, Show, Generic)

instance FromJSON NewPrivateVirtualInterface
instance ToJSON NewPrivateVirtualInterface

-- | An AWS Direct Connect location where connections and interconnects can be
-- requested.
data Location = Location
    { mlocationCode :: Maybe Text
      -- ^ The code used to indicate the AWS Direct Connect location.
    , mlocationName :: Maybe Text
      -- ^ The name of the AWS Direct Connect location. The name includes the
      -- colocation partner name and the physical site of the lit building.
    } deriving (Eq, Show, Generic)

instance FromJSON Location
instance ToJSON Location

-- | An interconnect is a connection that can host other connections. Like a
-- standard AWS Direct Connect connection, an interconnect represents the
-- physical connection between an AWS Direct Connect partner's network and a
-- specific Direct Connect location. An AWS Direct Connect partner who owns an
-- interconnect can provision hosted connections on the interconnect for their
-- end customers, thereby providing the end customers with connectivity to AWS
-- services. The resources of the interconnect, including bandwidth and VLAN
-- numbers, are shared by all of the hosted connections on the interconnect,
-- and the owner of the interconnect determines how these resources are
-- assigned.
data Interconnect = Interconnect
    { ibandwidth :: Maybe Text
      -- ^ Bandwidth of the connection. Example: 1Gbps Default: None.
    , iinterconnectId :: Maybe Text
      -- ^ The ID of the interconnect. Example: dxcon-abc123.
    , iinterconnectName :: Maybe Text
      -- ^ The name of the interconnect. Example: "1G Interconnect to AWS".
    , iinterconnectState :: Maybe InterconnectState
      -- ^ State of the interconnect. Requested: The initial state of an interconnect.
      -- The interconnect stays in the requested state until the Letter of
      -- Authorization (LOA) is sent to the customer. Pending: The interconnect has
      -- been approved, and is being initialized. Available: The network link is up,
      -- and the interconnect is ready for use. Down: The network link is down.
      -- Deleted: The interconnect has been deleted.
    , ilocation :: Maybe Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    , iregion :: Maybe Text
      -- ^ The AWS region where the connection is located. Example: us-east-1 Default:
      -- None.
    } deriving (Eq, Show, Generic)

instance FromJSON Interconnect
instance ToJSON Interconnect

-- | A connection represents the physical network connection between the AWS
-- Direct Connect location and the customer.
data Connection = Connection
    { cbandwidth :: Maybe Text
      -- ^ Bandwidth of the connection. Example: 1Gbps Default: None.
    , cconnectionId :: Maybe Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    , cconnectionName :: Maybe Text
      -- ^ The name of the connection. Example: "1G Connection to AWS" Default: None.
    , cconnectionState :: Maybe ConnectionState
      -- ^ State of the connection. Ordering: The initial state of a hosted connection
      -- provisioned on an interconnect. The connection stays in the ordering state
      -- until the owner of the hosted connection confirms or declines the
      -- connection order. Requested: The initial state of a standard connection.
      -- The connection stays in the requested state until the Letter of
      -- Authorization (LOA) is sent to the customer. Pending: The connection has
      -- been approved, and is being initialized. Available: The network link is up,
      -- and the connection is ready for use. Down: The network link is down.
      -- Deleted: The connection has been deleted. Rejected: A hosted connection in
      -- the 'Ordering' state will enter the 'Rejected' state if it is deleted by
      -- the end customer.
    , clocation :: Maybe Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    , cownerAccount :: Maybe Text
    , cpartnerName :: Maybe Text
    , cregion :: Maybe Text
      -- ^ The AWS region where the connection is located. Example: us-east-1 Default:
      -- None.
    , cvlan :: Maybe Int
      -- ^ The VLAN ID. Example: 101.
    } deriving (Eq, Show, Generic)

instance FromJSON Connection
instance ToJSON Connection

-- | State of the virtual interface. Confirming: The creation of the virtual
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

data VirtualInterfaceState
    = VirtualInterfaceStateAvailable
    | VirtualInterfaceStateConfirming
    | VirtualInterfaceStateDeleted
    | VirtualInterfaceStateDeleting
    | VirtualInterfaceStatePending
    | VirtualInterfaceStateRejected
    | VirtualInterfaceStateVerifying
      deriving (Eq, Ord, Generic)

instance Hashable VirtualInterfaceState

instance FromText VirtualInterfaceState where
    fromText "available" = Right VirtualInterfaceStateAvailable
    fromText "confirming" = Right VirtualInterfaceStateConfirming
    fromText "deleted" = Right VirtualInterfaceStateDeleted
    fromText "deleting" = Right VirtualInterfaceStateDeleting
    fromText "pending" = Right VirtualInterfaceStatePending
    fromText "rejected" = Right VirtualInterfaceStateRejected
    fromText "verifying" = Right VirtualInterfaceStateVerifying
    fromText e = fromTextFail $ "Unrecognised VirtualInterfaceState: " <> e

instance Read VirtualInterfaceState where
    readsPrec _ = fromTextRead

instance ToText VirtualInterfaceState where
    toText VirtualInterfaceStateAvailable = "available"
    toText VirtualInterfaceStateConfirming = "confirming"
    toText VirtualInterfaceStateDeleted = "deleted"
    toText VirtualInterfaceStateDeleting = "deleting"
    toText VirtualInterfaceStatePending = "pending"
    toText VirtualInterfaceStateRejected = "rejected"
    toText VirtualInterfaceStateVerifying = "verifying"

instance Show VirtualInterfaceState where
    show = toTextShow

instance FromJSON VirtualInterfaceState where
    parseJSON = fromTextJSON "VirtualInterfaceState"

instance FromJSON v => FromJSON (HashMap VirtualInterfaceState v) where
    parseJSON = fromTextHashJSON

instance ToJSON VirtualInterfaceState where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap VirtualInterfaceState v) where
    toJSON = toTextHashJSON

-- | State of the interconnect. Requested: The initial state of an interconnect.
-- The interconnect stays in the requested state until the Letter of
-- Authorization (LOA) is sent to the customer. Pending: The interconnect has
-- been approved, and is being initialized. Available: The network link is up,
-- and the interconnect is ready for use. Down: The network link is down.
-- Deleted: The interconnect has been deleted.

data InterconnectState
    = InterconnectStateAvailable
    | InterconnectStateDeleted
    | InterconnectStateDeleting
    | InterconnectStateDown
    | InterconnectStatePending
    | InterconnectStateRequested
      deriving (Eq, Ord, Generic)

instance Hashable InterconnectState

instance FromText InterconnectState where
    fromText "available" = Right InterconnectStateAvailable
    fromText "deleted" = Right InterconnectStateDeleted
    fromText "deleting" = Right InterconnectStateDeleting
    fromText "down" = Right InterconnectStateDown
    fromText "pending" = Right InterconnectStatePending
    fromText "requested" = Right InterconnectStateRequested
    fromText e = fromTextFail $ "Unrecognised InterconnectState: " <> e

instance Read InterconnectState where
    readsPrec _ = fromTextRead

instance ToText InterconnectState where
    toText InterconnectStateAvailable = "available"
    toText InterconnectStateDeleted = "deleted"
    toText InterconnectStateDeleting = "deleting"
    toText InterconnectStateDown = "down"
    toText InterconnectStatePending = "pending"
    toText InterconnectStateRequested = "requested"

instance Show InterconnectState where
    show = toTextShow

instance FromJSON InterconnectState where
    parseJSON = fromTextJSON "InterconnectState"

instance FromJSON v => FromJSON (HashMap InterconnectState v) where
    parseJSON = fromTextHashJSON

instance ToJSON InterconnectState where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap InterconnectState v) where
    toJSON = toTextHashJSON

-- | State of the connection. Ordering: The initial state of a hosted connection
-- provisioned on an interconnect. The connection stays in the ordering state
-- until the owner of the hosted connection confirms or declines the
-- connection order. Requested: The initial state of a standard connection.
-- The connection stays in the requested state until the Letter of
-- Authorization (LOA) is sent to the customer. Pending: The connection has
-- been approved, and is being initialized. Available: The network link is up,
-- and the connection is ready for use. Down: The network link is down.
-- Deleted: The connection has been deleted. Rejected: A hosted connection in
-- the 'Ordering' state will enter the 'Rejected' state if it is deleted by
-- the end customer.

data ConnectionState
    = ConnectionStateAvailable
    | ConnectionStateDeleted
    | ConnectionStateDeleting
    | ConnectionStateDown
    | ConnectionStateOrdering
    | ConnectionStatePending
    | ConnectionStateRejected
    | ConnectionStateRequested
      deriving (Eq, Ord, Generic)

instance Hashable ConnectionState

instance FromText ConnectionState where
    fromText "available" = Right ConnectionStateAvailable
    fromText "deleted" = Right ConnectionStateDeleted
    fromText "deleting" = Right ConnectionStateDeleting
    fromText "down" = Right ConnectionStateDown
    fromText "ordering" = Right ConnectionStateOrdering
    fromText "pending" = Right ConnectionStatePending
    fromText "rejected" = Right ConnectionStateRejected
    fromText "requested" = Right ConnectionStateRequested
    fromText e = fromTextFail $ "Unrecognised ConnectionState: " <> e

instance Read ConnectionState where
    readsPrec _ = fromTextRead

instance ToText ConnectionState where
    toText ConnectionStateAvailable = "available"
    toText ConnectionStateDeleted = "deleted"
    toText ConnectionStateDeleting = "deleting"
    toText ConnectionStateDown = "down"
    toText ConnectionStateOrdering = "ordering"
    toText ConnectionStatePending = "pending"
    toText ConnectionStateRejected = "rejected"
    toText ConnectionStateRequested = "requested"

instance Show ConnectionState where
    show = toTextShow

instance FromJSON ConnectionState where
    parseJSON = fromTextJSON "ConnectionState"

instance FromJSON v => FromJSON (HashMap ConnectionState v) where
    parseJSON = fromTextHashJSON

instance ToJSON ConnectionState where
    toJSON = toTextJSON

instance ToJSON v => ToJSON (HashMap ConnectionState v) where
    toJSON = toTextHashJSON
