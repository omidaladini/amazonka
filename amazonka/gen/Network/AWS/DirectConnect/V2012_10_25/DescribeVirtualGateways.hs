{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.DirectConnect.V2012_10_25.DescribeVirtualGateways
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
module Network.AWS.DirectConnect.V2012_10_25.DescribeVirtualGateways where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.DirectConnect.V2012_10_25.Types
import Network.AWS.Prelude

data DescribeVirtualGateways = DescribeVirtualGateways
    deriving (Eq, Show, Generic)

makeLenses ''DescribeVirtualGateways

instance ToPath DescribeVirtualGateways

instance ToQuery DescribeVirtualGateways

instance ToHeaders DescribeVirtualGateways

instance ToJSON DescribeVirtualGateways

data DescribeVirtualGatewaysResponse = DescribeVirtualGatewaysResponse
    { _vgVirtualGateways :: [VirtualGateway]
      -- ^ A list of virtual private gateways.
    } deriving (Show, Generic)

makeLenses ''DescribeVirtualGatewaysResponse

instance FromJSON DescribeVirtualGatewaysResponse

instance AWSRequest DescribeVirtualGateways where
    type Sv DescribeVirtualGateways = DirectConnect
    type Rs DescribeVirtualGateways = DescribeVirtualGatewaysResponse

    request = get
    response _ = jsonResponse