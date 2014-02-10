{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.AssociateRouteTable
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Associates a subnet with a route table. The subnet and route table must be
-- in the same VPC. This association causes traffic originating from the
-- subnet to be routed according to the routes in the route table. The action
-- returns an association ID, which you need if you want to disassociate the
-- route table from the subnet later. A route table can be associated with
-- multiple subnets. For more information about route tables, go to Route
-- Tables in the Amazon Virtual Private Cloud User Guide.
module Network.AWS.EC2.AssociateRouteTable where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
associateRouteTable :: Text
                    -- ^ The ID of the route table.
                    -> Text
                    -- ^ The ID of the subnet.
                    -> AssociateRouteTable
associateRouteTable p1 p2 = AssociateRouteTable
    { artRouteTableId = p1
    , artSubnetId = p2
    , artDryRun = Nothing
    }

data AssociateRouteTable = AssociateRouteTable
    { artDryRun :: Maybe Bool
    , artRouteTableId :: !Text
      -- ^ The ID of the route table.
    , artSubnetId :: !Text
      -- ^ The ID of the subnet.
    } deriving (Eq, Show, Generic)

instance ToQuery AssociateRouteTable

instance AWSRequest AssociateRouteTable where
    type Er AssociateRouteTable = EC2Error
    type Rs AssociateRouteTable = AssociateRouteTableResponse
    request  = postQuery service "AssociateRouteTable"
    response = responseXML

data AssociateRouteTableResponse = AssociateRouteTableResponse
    { artrAssociationId :: Maybe Text
    } deriving (Eq, Show, Generic)

instance FromXML AssociateRouteTableResponse where
    fromXMLOptions = xmlOptions
