{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ReplaceRoute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Replaces an existing route within a route table in a VPC. For more
-- information about route tables, go to Route Tables in the Amazon Virtual
-- Private Cloud User Guide.
module Network.AWS.EC2.ReplaceRoute where

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data ReplaceRoute = ReplaceRoute
    { rrrDestinationCidrBlock :: !Text
      -- ^ The CIDR address block used for the destination match. For example:
      -- 0.0.0.0/0. The value you provide must match the CIDR of an existing route
      -- in the table.
    , rrrDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , rrrGatewayId :: Maybe Text
      -- ^ The ID of a VPN or Internet gateway attached to your VPC.
    , rrrInstanceId :: Maybe Text
      -- ^ The ID of a NAT instance in your VPC.
    , rrrNetworkInterfaceId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , rrrRouteTableId :: !Text
      -- ^ The ID of the route table where the route will be replaced.
    } deriving (Eq, Show, Generic)

instance ToQuery ReplaceRoute

instance AWSRequest ReplaceRoute where
    type Er ReplaceRoute = EC2Error
    type Rs ReplaceRoute = ReplaceRouteResponse
    request = v2Query service GET "ReplaceRoute"

data ReplaceRouteResponse = ReplaceRouteResponse
    deriving (Eq, Show, Generic)

instance FromXML ReplaceRouteResponse where
    fromXMLOptions = xmlOptions
