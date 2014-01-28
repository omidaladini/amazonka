{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.CreateInterconnect
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new interconnect between a AWS Direct Connect partner's network
-- and a specific AWS Direct Connect location. An interconnect is a connection
-- which is capable of hosting other connections. The AWS Direct Connect
-- partner can use an interconnect to provide sub-1Gbps AWS Direct Connect
-- service to tier 2 customers who do not have their own connections. Like a
-- standard connection, an interconnect links the AWS Direct Connect partner's
-- network to an AWS Direct Connect location over a standard 1 Gbps or 10 Gbps
-- Ethernet fiber-optic cable. One end is connected to the partner's router,
-- the other to an AWS Direct Connect router. For each end customer, the AWS
-- Direct Connect partner provisions a connection on their interconnect by
-- calling AllocateConnectionOnInterconnect. The end customer can then connect
-- to AWS resources by creating a virtual interface on their connection, using
-- the VLAN assigned to them by the AWS Direct Connect partner.
module Network.AWS.DirectConnect.CreateInterconnect where

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

data CreateInterconnect = CreateInterconnect
    { cirBandwidth :: !Text
      -- ^ The port bandwidth Example: 1Gbps Default: None Available values:
      -- 1Gbps,10Gbps.
    , cirInterconnectName :: !Text
      -- ^ The name of the interconnect. Example: "1G Interconnect to AWS" Default:
      -- None.
    , cirLocation :: !Text
      -- ^ Where the interconnect is located Example: EqSV5 Default: None.
    } deriving (Eq, Show, Generic)

instance ToJSON CreateInterconnect

instance AWSRequest CreateInterconnect where
    type Er CreateInterconnect = DirectConnectError
    type Rs CreateInterconnect = CreateInterconnectResponse
    request  = getJSON service
    response = responseJSON

data CreateInterconnectResponse = CreateInterconnectResponse
    { cirrsBandwidth :: Maybe Text
      -- ^ Bandwidth of the connection. Example: 1Gbps Default: None.
    , cirrsInterconnectId :: Maybe Text
      -- ^ The ID of the interconnect. Example: dxcon-abc123.
    , cirrsInterconnectName :: Maybe Text
      -- ^ The name of the interconnect. Example: "1G Interconnect to AWS".
    , cirrsInterconnectState :: Maybe InterconnectState
      -- ^ State of the interconnect. Requested: The initial state of an interconnect.
      -- The interconnect stays in the requested state until the Letter of
      -- Authorization (LOA) is sent to the customer. Pending: The interconnect has
      -- been approved, and is being initialized. Available: The network link is up,
      -- and the interconnect is ready for use. Down: The network link is down.
      -- Deleted: The interconnect has been deleted.
    , cirrsLocation :: Maybe Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    , cirrsRegion :: Maybe Text
      -- ^ The AWS region where the connection is located. Example: us-east-1 Default:
      -- None.
    } deriving (Eq, Show, Generic)

instance FromJSON CreateInterconnectResponse
