{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.AllocateConnectionOnInterconnect
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a hosted connection on an interconnect. Allocates a VLAN number and
-- a specified amount of bandwidth for use by a hosted connection on the given
-- interconnect.
module Network.AWS.DirectConnect.AllocateConnectionOnInterconnect where

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

data AllocateConnectionOnInterconnect = AllocateConnectionOnInterconnect
    { acoirBandwidth :: !Text
      -- ^ Bandwidth of the connection. Example: 1Gbps Default: None.
    , acoirConnectionName :: !Text
      -- ^ Name of the provisioned connection. Example: "500M Connection to AWS"
      -- Default: None.
    , acoirInterconnectId :: !Text
      -- ^ ID of the interconnect on which the connection will be provisioned.
      -- Example: dxcon-456abc78 Default: None.
    , acoirOwnerAccount :: !Text
      -- ^ Numeric account Id of the customer for whom the connection will be
      -- provisioned. Example: 123443215678 Default: None.
    , acoirVlan :: !Int
      -- ^ The dedicated VLAN provisioned to the connection. Example: 101 Default:
      -- None.
    } deriving (Eq, Show, Generic)

instance ToJSON AllocateConnectionOnInterconnect

instance AWSRequest AllocateConnectionOnInterconnect where
    type Er AllocateConnectionOnInterconnect = DirectConnectError
    type Rs AllocateConnectionOnInterconnect = AllocateConnectionOnInterconnectResponse
    request  = getJSON service
    response = responseJSON

data AllocateConnectionOnInterconnectResponse = AllocateConnectionOnInterconnectResponse
    { acoirrsBandwidth :: Maybe Text
      -- ^ Bandwidth of the connection. Example: 1Gbps Default: None.
    , acoirrsConnectionId :: Maybe Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    , acoirrsConnectionName :: Maybe Text
      -- ^ The name of the connection. Example: "1G Connection to AWS" Default: None.
    , acoirrsConnectionState :: Maybe ConnectionState
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
    , acoirrsLocation :: Maybe Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    , acoirrsOwnerAccount :: Maybe Text
    , acoirrsPartnerName :: Maybe Text
    , acoirrsRegion :: Maybe Text
      -- ^ The AWS region where the connection is located. Example: us-east-1 Default:
      -- None.
    , acoirrsVlan :: Maybe Int
      -- ^ The VLAN ID. Example: 101.
    } deriving (Eq, Show, Generic)

instance FromJSON AllocateConnectionOnInterconnectResponse
