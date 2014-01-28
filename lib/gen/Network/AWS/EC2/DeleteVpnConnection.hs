{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteVpnConnection
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a VPN connection. Use this if you want to delete a VPC and all its
-- associated components. Another reason to use this operation is if you
-- believe the tunnel credentials for your VPN connection have been
-- compromised. In that situation, you can delete the VPN connection and
-- create a new one that has new keys, without needing to delete the VPC or
-- VPN gateway. If you create a new VPN connection, you must reconfigure the
-- customer gateway using the new configuration information returned with the
-- new VPN connection ID. If you're deleting the VPC and all its associated
-- parts, we recommend you detach the VPN gateway from the VPC and delete the
-- VPC before deleting the VPN connection.
module Network.AWS.EC2.DeleteVpnConnection where

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

data DeleteVpnConnection = DeleteVpnConnection
    { dvcrDryRun :: Maybe Bool
    , dvcrVpnConnectionId :: !Text
      -- ^ The ID of the VPN connection to delete.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteVpnConnection

instance AWSRequest DeleteVpnConnection where
    type Er DeleteVpnConnection = EC2Error
    type Rs DeleteVpnConnection = DeleteVpnConnectionResponse
    request = getQuery service "DeleteVpnConnection"

data DeleteVpnConnectionResponse = DeleteVpnConnectionResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteVpnConnectionResponse where
    fromXMLOptions = xmlOptions