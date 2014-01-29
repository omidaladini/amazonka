{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateRoute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new route in a route table within a VPC. The route's target can
-- be either a gateway attached to the VPC or a NAT instance in the VPC. When
-- determining how to route traffic, we use the route with the most specific
-- match. For example, let's say the traffic is destined for 192.0.2.3, and
-- the route table includes the following two routes: 192.0.2.0/24 (goes to
-- some target A) 192.0.2.0/28 (goes to some target B) Both routes apply to
-- the traffic destined for 192.0.2.3. However, the second route in the list
-- is more specific, so we use that route to determine where to target the
-- traffic. For more information about route tables, go to Route Tables in the
-- Amazon Virtual Private Cloud User Guide.
module Network.AWS.EC2.CreateRoute where

import           Control.Monad
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import           Text.XML.Generic

import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields where applicable.
createRoute :: Text
            -> Text
            -> AWS (Either EC2Error CreateRouteResponse)
createRoute p1 p2 = undefined $ CreateRoute
    { crrDestinationCidrBlock = p1
    , crrRouteTableId = p2
    , crrDryRun = Nothing
    , crrGatewayId = Nothing
    , crrInstanceId = Nothing
    , crrNetworkInterfaceId = Nothing
    }

data CreateRoute = CreateRoute
    { crrDestinationCidrBlock :: !Text
      -- ^ The CIDR address block used for the destination match. For example:
      -- 0.0.0.0/0. Routing decisions are based on the most specific match.
    , crrDryRun :: Maybe Bool
    , crrGatewayId :: Maybe Text
      -- ^ The ID of a VPN or Internet gateway attached to your VPC. You must provide
      -- either GatewayId or InstanceId, but not both.
    , crrInstanceId :: Maybe Text
      -- ^ The ID of a NAT instance in your VPC. You must provide either GatewayId or
      -- InstanceId, but not both.
    , crrNetworkInterfaceId :: Maybe Text
    , crrRouteTableId :: !Text
      -- ^ The ID of the route table where the route will be added.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateRoute

instance AWSRequest CreateRoute where
    type Er CreateRoute = EC2Error
    type Rs CreateRoute = CreateRouteResponse
    request = getQuery service "CreateRoute"

data CreateRouteResponse = CreateRouteResponse
    deriving (Eq, Show, Generic)

instance FromXML CreateRouteResponse where
    fromXMLOptions = xmlOptions
