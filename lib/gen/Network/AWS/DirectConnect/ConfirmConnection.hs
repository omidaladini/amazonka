{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.ConfirmConnection
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Confirm the creation of a hosted connection on an interconnect. Upon
-- creation, the hosted connection is initially in the 'Ordering' state, and
-- will remain in this state until the owner calls ConfirmConnection to
-- confirm creation of the hosted connection.
module Network.AWS.DirectConnect.ConfirmConnection where

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
confirmConnection :: Text
                  -> AWS (Either DirectConnectError ConfirmConnectionResponse)
confirmConnection p1 = undefined $ ConfirmConnection
    { ccsConnectionId = p1
    }

data ConfirmConnection = ConfirmConnection
    { ccsConnectionId :: !Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    } deriving (Eq, Show, Generic)

instance ToJSON ConfirmConnection

instance AWSRequest ConfirmConnection where
    type Er ConfirmConnection = DirectConnectError
    type Rs ConfirmConnection = ConfirmConnectionResponse
    request  = getJSON service
    response = responseJSON

data ConfirmConnectionResponse = ConfirmConnectionResponse
    { ccsrsConnectionState :: Maybe ConnectionState
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
    } deriving (Eq, Show, Generic)

instance FromJSON ConfirmConnectionResponse
