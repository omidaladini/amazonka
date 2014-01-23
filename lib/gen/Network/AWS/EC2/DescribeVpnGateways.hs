{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeVpnGateways
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gives you information about your VPN gateways. You can filter the results
-- to return information only about VPN gateways that match criteria you
-- specify. For example, you could ask to get information about a particular
-- VPN gateway (or all) only if the gateway's state is pending or available.
-- You can specify multiple filters (e.g., the VPN gateway is in a particular
-- Availability Zone and the gateway's state is pending or available). The
-- result includes information for a particular VPN gateway only if the
-- gateway matches all your filters. If there's no match, no special message
-- is returned; the response is simply empty. The following table shows the
-- available filters.
module Network.AWS.EC2.DescribeVpnGateways where

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeVpnGateways = DescribeVpnGateways
    { dvgtDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , dvgtFilters :: [Filter]
      -- ^ A list of filters used to match properties for VPN Gateways. For a complete
      -- reference to the available filter keys for this operation, see the Amazon
      -- EC2 API reference.
    , dvgtVpnGatewayIds :: [Text]
      -- ^ A list of filters used to match properties for VPN Gateways. For a complete
      -- reference to the available filter keys for this operation, see the Amazon
      -- EC2 API reference.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeVpnGateways

instance AWSRequest DescribeVpnGateways where
    type Er DescribeVpnGateways = EC2Error
    type Rs DescribeVpnGateways = DescribeVpnGatewaysResponse
    request = v2Query service GET "DescribeVpnGateways"

data DescribeVpnGatewaysResponse = DescribeVpnGatewaysResponse
    { dvgtrsVpnGateways :: [VpnGateway]
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance FromXML DescribeVpnGatewaysResponse where
    fromXMLOptions = xmlOptions
