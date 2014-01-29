{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateCustomerGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Provides information to AWS about your customer gateway device. The
-- customer gateway is the appliance at your end of the VPN connection
-- (compared to the VPN gateway, which is the device at the AWS side of the
-- VPN connection). You can have a single active customer gateway per AWS
-- account (active means that you've created a VPN connection to use with the
-- customer gateway). AWS might delete any customer gateway that you create
-- with this operation if you leave it inactive for an extended period of
-- time. You must provide the Internet-routable IP address of the customer
-- gateway's external interface. The IP address must be static. You must also
-- provide the device's Border Gateway Protocol (BGP) Autonomous System Number
-- (ASN). You can use an existing ASN assigned to your network. If you don't
-- have an ASN already, you can use a private ASN (in the 64512 - 65534
-- range). For more information about ASNs, go to
-- http://en.wikipedia.org/wiki/Autonomous_system_%28Internet%29.
module Network.AWS.EC2.CreateCustomerGateway where

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
createCustomerGateway :: Int
                      -> Text
                      -> GatewayType
                      -> AWS (Either EC2Error CreateCustomerGatewayResponse)
createCustomerGateway p1 p2 p3 = undefined $ CreateCustomerGateway
    { ccgrBgpAsn = p1
    , ccgrPublicIp = p2
    , ccgrType = p3
    , ccgrDryRun = Nothing
    }

data CreateCustomerGateway = CreateCustomerGateway
    { ccgrBgpAsn :: !Int
      -- ^ The customer gateway's Border Gateway Protocol (BGP) Autonomous System
      -- Number (ASN).
    , ccgrDryRun :: Maybe Bool
    , ccgrPublicIp :: !Text
      -- ^ The Internet-routable IP address for the customer gateway's outside
      -- interface. The address must be static.
    , ccgrType :: !GatewayType
      -- ^ The type of VPN connection this customer gateway supports.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateCustomerGateway

instance AWSRequest CreateCustomerGateway where
    type Er CreateCustomerGateway = EC2Error
    type Rs CreateCustomerGateway = CreateCustomerGatewayResponse
    request = getQuery service "CreateCustomerGateway"

data CreateCustomerGatewayResponse = CreateCustomerGatewayResponse
    { ccgrrsCustomerGateway :: Maybe CustomerGateway
      -- ^ Information about the customer gateway.
    } deriving (Eq, Show, Generic)

instance FromXML CreateCustomerGatewayResponse where
    fromXMLOptions = xmlOptions
