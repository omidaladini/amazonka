{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeVpnConnections
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gives you information about your VPN connections. We strongly recommend you
-- use HTTPS when calling this operation because the response contains
-- sensitive cryptographic information for configuring your customer gateway.
-- You can filter the results to return information only about VPN connections
-- that match criteria you specify. For example, you could ask to get
-- information about a particular VPN connection (or all) only if the VPN's
-- state is pending or available. You can specify multiple filters (e.g., the
-- VPN connection is associated with a particular VPN gateway, and the
-- gateway's state is pending or available). The result includes information
-- for a particular VPN connection only if the VPN connection matches all your
-- filters. If there's no match, no special message is returned; the response
-- is simply empty. The following table shows the available filters.
module Network.AWS.EC2.DescribeVpnConnections where

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeVpnConnections = DescribeVpnConnections
    { dvcsDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , dvcsFilters :: [Filter]
      -- ^ A list of filters used to match properties for VPN Connections. For a
      -- complete reference to the available filter keys for this operation, see the
      -- Amazon EC2 API reference.
    , dvcsVpnConnectionIds :: [Text]
      -- ^ A VPN connection ID. More than one may be specified per request.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeVpnConnections

instance AWSRequest DescribeVpnConnections where
    type Er DescribeVpnConnections = EC2Error
    type Rs DescribeVpnConnections = DescribeVpnConnectionsResponse
    request = v2Query service GET "DescribeVpnConnections"

data DescribeVpnConnectionsResponse = DescribeVpnConnectionsResponse
    { dvcsrsVpnConnections :: [VpnConnection]
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance FromXML DescribeVpnConnectionsResponse where
    fromXMLOptions = xmlOptions
