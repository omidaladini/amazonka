{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DisassociateRouteTable
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Disassociates a subnet from a route table. After you perform this action,
-- the subnet no longer uses the routes in the route table. Instead it uses
-- the routes in the VPC's main route table. For more information about route
-- tables, go to Route Tables in the Amazon Virtual Private Cloud User Guide.
module Network.AWS.EC2.DisassociateRouteTable where

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DisassociateRouteTable = DisassociateRouteTable
    { drtrAssociationId :: !Text
      -- ^ The association ID representing the current association between the route
      -- table and subnet.
    , drtrDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery DisassociateRouteTable

instance AWSRequest DisassociateRouteTable where
    type Er DisassociateRouteTable = EC2Error
    type Rs DisassociateRouteTable = DisassociateRouteTableResponse
    request = v2Query service GET "DisassociateRouteTable"

data DisassociateRouteTableResponse = DisassociateRouteTableResponse
    deriving (Eq, Show, Generic)

instance FromXML DisassociateRouteTableResponse where
    fromXMLOptions = xmlOptions
