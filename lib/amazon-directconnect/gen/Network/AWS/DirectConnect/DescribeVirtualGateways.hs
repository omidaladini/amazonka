{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.DescribeVirtualGateways
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of virtual private gateways owned by the AWS account. You
-- can create one or more AWS Direct Connect private virtual interfaces
-- linking to a virtual private gateway. A virtual private gateway can be
-- managed via Amazon Virtual Private Cloud (VPC) console or the EC2
-- CreateVpnGateway action.
module Network.AWS.DirectConnect.DescribeVirtualGateways where

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
describeVirtualGateways :: DescribeVirtualGateways
describeVirtualGateways = DescribeVirtualGateways

data DescribeVirtualGateways = DescribeVirtualGateways
    deriving (Eq, Show, Generic)

instance ToJSON DescribeVirtualGateways where
    toJSON = genericToJSON jsonOptions

instance AWSRequest DescribeVirtualGateways where
    type Er DescribeVirtualGateways = DirectConnectError
    type Rs DescribeVirtualGateways = DescribeVirtualGatewaysResponse
    request  = getJSON service
    response = responseJSON

data DescribeVirtualGatewaysResponse = DescribeVirtualGatewaysResponse
    { vgVirtualGateways :: [VirtualGateway]
      -- ^ A list of virtual private gateways.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeVirtualGatewaysResponse where
    fromJSON = genericFromJSON jsonOptions

