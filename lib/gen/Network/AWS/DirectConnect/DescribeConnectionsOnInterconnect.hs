{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.DescribeConnectionsOnInterconnect
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Return a list of connections that have been provisioned on the given
-- interconnect.
module Network.AWS.DirectConnect.DescribeConnectionsOnInterconnect where

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
describeConnectionsOnInterconnect :: Text
                                  -> AWS (Either DirectConnectError DescribeConnectionsOnInterconnectResponse)
describeConnectionsOnInterconnect p1 = undefined $ DescribeConnectionsOnInterconnect
    { dcoirInterconnectId = p1
    }

data DescribeConnectionsOnInterconnect = DescribeConnectionsOnInterconnect
    { dcoirInterconnectId :: !Text
      -- ^ ID of the interconnect on which a list of connection is provisioned.
      -- Example: dxcon-abc123 Default: None.
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeConnectionsOnInterconnect

instance AWSRequest DescribeConnectionsOnInterconnect where
    type Er DescribeConnectionsOnInterconnect = DirectConnectError
    type Rs DescribeConnectionsOnInterconnect = DescribeConnectionsOnInterconnectResponse
    request  = getJSON service
    response = responseJSON

data DescribeConnectionsOnInterconnectResponse = DescribeConnectionsOnInterconnectResponse
    { dcoirrsConnections :: [Connection]
      -- ^ A list of connections.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeConnectionsOnInterconnectResponse
