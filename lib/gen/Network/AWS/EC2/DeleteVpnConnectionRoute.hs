{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteVpnConnectionRoute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for DeleteVpnConnectionRoute
module Network.AWS.EC2.DeleteVpnConnectionRoute where

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DeleteVpnConnectionRoute = DeleteVpnConnectionRoute
    { dvcrrDestinationCidrBlock :: !Text
      -- ^ FIXME: Missing documentation
    , dvcrrVpnConnectionId :: !Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteVpnConnectionRoute

instance AWSRequest DeleteVpnConnectionRoute where
    type Er DeleteVpnConnectionRoute = EC2Error
    type Rs DeleteVpnConnectionRoute = DeleteVpnConnectionRouteResponse
    request = v2Query service GET "DeleteVpnConnectionRoute"

data DeleteVpnConnectionRouteResponse = DeleteVpnConnectionRouteResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteVpnConnectionRouteResponse where
    fromXMLOptions = xmlOptions
