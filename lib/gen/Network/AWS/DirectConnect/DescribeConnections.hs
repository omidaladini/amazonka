{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.DescribeConnections
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Displays all connections in this region. If a connection ID is provided,
-- the call returns only that particular connection.
module Network.AWS.DirectConnect.DescribeConnections where

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
describeConnections :: DescribeConnections
describeConnections = DescribeConnections
    { dcsConnectionId = Nothing
    }

data DescribeConnections = DescribeConnections
    { dcsConnectionId :: Maybe Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeConnections

instance AWSRequest DescribeConnections where
    type Er DescribeConnections = DirectConnectError
    type Rs DescribeConnections = DescribeConnectionsResponse
    request  = getJSON service
    response = responseJSON

data DescribeConnectionsResponse = DescribeConnectionsResponse
    { dcsrsConnections :: [Connection]
      -- ^ A list of connections.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeConnectionsResponse
