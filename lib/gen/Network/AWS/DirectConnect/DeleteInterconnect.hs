{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.DeleteInterconnect
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified interconnect.
module Network.AWS.DirectConnect.DeleteInterconnect where

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
deleteInterconnect :: Text
                   -> AWS (Either DirectConnectError DeleteInterconnectResponse)
deleteInterconnect p1 = undefined $ DeleteInterconnect
    { disInterconnectId = p1
    }

data DeleteInterconnect = DeleteInterconnect
    { disInterconnectId :: !Text
      -- ^ The ID of the interconnect. Example: dxcon-abc123.
    } deriving (Eq, Show, Generic)

instance ToJSON DeleteInterconnect

instance AWSRequest DeleteInterconnect where
    type Er DeleteInterconnect = DirectConnectError
    type Rs DeleteInterconnect = DeleteInterconnectResponse
    request  = getJSON service
    response = responseJSON

data DeleteInterconnectResponse = DeleteInterconnectResponse
    { disrsInterconnectState :: Maybe InterconnectState
      -- ^ State of the interconnect. Requested: The initial state of an interconnect.
      -- The interconnect stays in the requested state until the Letter of
      -- Authorization (LOA) is sent to the customer. Pending: The interconnect has
      -- been approved, and is being initialized. Available: The network link is up,
      -- and the interconnect is ready for use. Down: The network link is down.
      -- Deleted: The interconnect has been deleted.
    } deriving (Eq, Show, Generic)

instance FromJSON DeleteInterconnectResponse
