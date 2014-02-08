{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ReplaceRouteTableAssociation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Changes the route table associated with a given subnet in a VPC. After you
-- execute this action, the subnet uses the routes in the new route table it's
-- associated with. For more information about route tables, go to Route
-- Tables in the Amazon Virtual Private Cloud User Guide. You can also use
-- this to change which table is the main route table in the VPC. You just
-- specify the main route table's association ID and the route table that you
-- want to be the new main route table.
module Network.AWS.EC2.ReplaceRouteTableAssociation where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
replaceRouteTableAssociation :: Text
                             -> Text
                             -> ReplaceRouteTableAssociation
replaceRouteTableAssociation p1 p2 = ReplaceRouteTableAssociation
    { rrtarAssociationId = p1
    , rrtarRouteTableId = p2
    , rrtarDryRun = Nothing
    }

data ReplaceRouteTableAssociation = ReplaceRouteTableAssociation
    { rrtarAssociationId :: !Text
      -- ^ The ID representing the current association between the original route
      -- table and the subnet.
    , rrtarDryRun :: Maybe Bool
    , rrtarRouteTableId :: !Text
      -- ^ The ID of the new route table to associate with the subnet.
    } deriving (Eq, Show, Generic)

instance ToQuery ReplaceRouteTableAssociation

instance AWSRequest ReplaceRouteTableAssociation where
    type Er ReplaceRouteTableAssociation = EC2Error
    type Rs ReplaceRouteTableAssociation = ReplaceRouteTableAssociationResponse
    request = getQuery service "ReplaceRouteTableAssociation"

data ReplaceRouteTableAssociationResponse = ReplaceRouteTableAssociationResponse
    { rrtarrsNewAssociationId :: Maybe Text
    } deriving (Eq, Show, Generic)

instance FromXML ReplaceRouteTableAssociationResponse where
    fromXMLOptions = xmlOptions
