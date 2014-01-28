{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.AttachVpnGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Attaches a VPN gateway to a VPC. This is the last step required to get your
-- VPC fully connected to your data center before launching instances in it.
-- For more information, go to Process for Using Amazon VPC in the Amazon
-- Virtual Private Cloud Developer Guide.
module Network.AWS.EC2.AttachVpnGateway where

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

data AttachVpnGateway = AttachVpnGateway
    { avgrDryRun :: Maybe Bool
    , avgrVpcId :: !Text
      -- ^ The ID of the VPC to attach to the VPN gateway.
    , avgrVpnGatewayId :: !Text
      -- ^ The ID of the VPN gateway to attach to the VPC.
    } deriving (Eq, Show, Generic)

instance ToQuery AttachVpnGateway

instance AWSRequest AttachVpnGateway where
    type Er AttachVpnGateway = EC2Error
    type Rs AttachVpnGateway = AttachVpnGatewayResponse
    request = getQuery service "AttachVpnGateway"

data AttachVpnGatewayResponse = AttachVpnGatewayResponse
    { avgrrsVpcAttachment :: Maybe VpcAttachment
    } deriving (Eq, Show, Generic)

instance FromXML AttachVpnGatewayResponse where
    fromXMLOptions = xmlOptions
