{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteVpnGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a VPN gateway. Use this when you want to delete a VPC and all its
-- associated components because you no longer need them. We recommend that
-- before you delete a VPN gateway, you detach it from the VPC and delete the
-- VPN connection. Note that you don't need to delete the VPN gateway if you
-- just want to delete and re-create the VPN connection between your VPC and
-- data center.
module Network.AWS.EC2.DeleteVpnGateway where

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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteVpnGateway :: Text
                 -> DeleteVpnGateway
deleteVpnGateway p1 = DeleteVpnGateway
    { dvgrVpnGatewayId = p1
    , dvgrDryRun = Nothing
    }

data DeleteVpnGateway = DeleteVpnGateway
    { dvgrDryRun :: Maybe Bool
    , dvgrVpnGatewayId :: !Text
      -- ^ The ID of the VPN gateway to delete.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteVpnGateway

instance AWSRequest DeleteVpnGateway where
    type Er DeleteVpnGateway = EC2Error
    type Rs DeleteVpnGateway = DeleteVpnGatewayResponse
    request = getQuery service "DeleteVpnGateway"

data DeleteVpnGatewayResponse = DeleteVpnGatewayResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteVpnGatewayResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot DeleteVpnGatewayResponse
