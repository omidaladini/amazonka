{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.DeleteConnection
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the connection. Deleting a connection only stops the AWS Direct
-- Connect port hour and data transfer charges. You need to cancel separately
-- with the providers any services or charges for cross-connects or network
-- circuits that connect you to the AWS Direct Connect location.
module Network.AWS.DirectConnect.DeleteConnection where

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
deleteConnection :: Text
                 -> DeleteConnection
deleteConnection p1 = DeleteConnection
    { dcrConnectionId = p1
    }

data DeleteConnection = DeleteConnection
    { dcrConnectionId :: !Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    } deriving (Eq, Show, Generic)

instance ToJSON DeleteConnection where
    toJSON = genericToJSON jsonOptions

instance AWSRequest DeleteConnection where
    type Er DeleteConnection = DirectConnectError
    type Rs DeleteConnection = DeleteConnectionResponse
    request  = getJSON service
    response = responseJSON

data DeleteConnectionResponse = DeleteConnectionResponse
    { dcrrsBandwidth :: Maybe Text
      -- ^ Bandwidth of the connection. Example: 1Gbps Default: None.
    , dcrrsConnectionId :: Maybe Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    , dcrrsConnectionName :: Maybe Text
      -- ^ The name of the connection. Example: "1G Connection to AWS" Default: None.
    , dcrrsConnectionState :: Maybe ConnectionState
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
    , dcrrsLocation :: Maybe Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    , dcrrsOwnerAccount :: Maybe Text
    , dcrrsPartnerName :: Maybe Text
    , dcrrsRegion :: Maybe Text
      -- ^ The AWS region where the connection is located. Example: us-east-1 Default:
      -- None.
    , dcrrsVlan :: Maybe Int
      -- ^ The VLAN ID. Example: 101.
    } deriving (Eq, Show, Generic)

instance FromJSON DeleteConnectionResponse where
    fromJSON = genericFromJSON jsonOptions

