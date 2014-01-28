{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateVpnGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new VPN gateway. A VPN gateway is the VPC-side endpoint for your
-- VPN connection. You can create a VPN gateway before creating the VPC
-- itself.
module Network.AWS.EC2.CreateVpnGateway where

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

data CreateVpnGateway = CreateVpnGateway
    { cvgrAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone in which to create the VPN gateway.
    , cvgrDryRun :: Maybe Bool
    , cvgrType :: !GatewayType
      -- ^ The type of VPN connection this VPN gateway supports.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateVpnGateway

instance AWSRequest CreateVpnGateway where
    type Er CreateVpnGateway = EC2Error
    type Rs CreateVpnGateway = CreateVpnGatewayResponse
    request = getQuery service "CreateVpnGateway"

data CreateVpnGatewayResponse = CreateVpnGatewayResponse
    { cvgrrsVpnGateway :: Maybe VpnGateway
    } deriving (Eq, Show, Generic)

instance FromXML CreateVpnGatewayResponse where
    fromXMLOptions = xmlOptions