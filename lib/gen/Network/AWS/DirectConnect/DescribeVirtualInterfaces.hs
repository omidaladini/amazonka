{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.DescribeVirtualInterfaces
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Displays all virtual interfaces for an AWS account. Virtual interfaces
-- deleted fewer than 15 minutes before DescribeVirtualInterfaces is called
-- are also returned. If a connection ID is included then only virtual
-- interfaces associated with this connection will be returned. If a virtual
-- interface ID is included then only a single virtual interface will be
-- returned. A virtual interface (VLAN) transmits the traffic between the AWS
-- Direct Connect location and the customer. If a connection ID is provided,
-- only virtual interfaces provisioned on the specified connection will be
-- returned. If a virtual interface ID is provided, only this particular
-- virtual interface will be returned.
module Network.AWS.DirectConnect.DescribeVirtualInterfaces where

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
describeVirtualInterfaces :: AWS (Either DirectConnectError DescribeVirtualInterfacesResponse)
describeVirtualInterfaces = undefined $ DescribeVirtualInterfaces
    { dvirConnectionId = Nothing
    , dvirVirtualInterfaceId = Nothing
    }

data DescribeVirtualInterfaces = DescribeVirtualInterfaces
    { dvirConnectionId :: Maybe Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    , dvirVirtualInterfaceId :: Maybe Text
      -- ^ ID of the virtual interface. Example: dxvif-123dfg56 Default: None.
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeVirtualInterfaces

instance AWSRequest DescribeVirtualInterfaces where
    type Er DescribeVirtualInterfaces = DirectConnectError
    type Rs DescribeVirtualInterfaces = DescribeVirtualInterfacesResponse
    request  = getJSON service
    response = responseJSON

data DescribeVirtualInterfacesResponse = DescribeVirtualInterfacesResponse
    { dvirrsVirtualInterfaces :: [VirtualInterface]
      -- ^ A list of virtual interfaces.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeVirtualInterfacesResponse
