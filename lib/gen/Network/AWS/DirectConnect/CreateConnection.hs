{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.CreateConnection
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new connection between the customer network and a specific AWS
-- Direct Connect location. A connection links your internal network to an AWS
-- Direct Connect location over a standard 1 gigabit or 10 gigabit Ethernet
-- fiber-optic cable. One end of the cable is connected to your router, the
-- other to an AWS Direct Connect router. An AWS Direct Connect location
-- provides access to Amazon Web Services in the region it is associated with.
-- You can establish connections with AWS Direct Connect locations in multiple
-- regions, but a connection in one region does not provide connectivity to
-- other regions.
module Network.AWS.DirectConnect.CreateConnection where

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

data CreateConnection = CreateConnection
    { ccrBandwidth :: !Text
      -- ^ Bandwidth of the connection. Example: 1Gbps Default: None.
    , ccrConnectionName :: !Text
      -- ^ The name of the connection. Example: "1G Connection to AWS" Default: None.
    , ccrLocation :: !Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    } deriving (Eq, Show, Generic)

instance ToJSON CreateConnection

instance AWSRequest CreateConnection where
    type Er CreateConnection = DirectConnectError
    type Rs CreateConnection = CreateConnectionResponse
    request  = getJSON service
    response = responseJSON

data CreateConnectionResponse = CreateConnectionResponse
    { ccrrsBandwidth :: Maybe Text
      -- ^ Bandwidth of the connection. Example: 1Gbps Default: None.
    , ccrrsConnectionId :: Maybe Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    , ccrrsConnectionName :: Maybe Text
      -- ^ The name of the connection. Example: "1G Connection to AWS" Default: None.
    , ccrrsConnectionState :: Maybe ConnectionState
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
    , ccrrsLocation :: Maybe Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    , ccrrsOwnerAccount :: Maybe Text
    , ccrrsPartnerName :: Maybe Text
    , ccrrsRegion :: Maybe Text
      -- ^ The AWS region where the connection is located. Example: us-east-1 Default:
      -- None.
    , ccrrsVlan :: Maybe Int
      -- ^ The VLAN ID. Example: 101.
    } deriving (Eq, Show, Generic)

instance FromJSON CreateConnectionResponse
