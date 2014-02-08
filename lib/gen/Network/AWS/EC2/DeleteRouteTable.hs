{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteRouteTable
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a route table from a VPC. The route table must not be associated
-- with a subnet. You can't delete the main route table. For more information
-- about route tables, go to Route Tables in the Amazon Virtual Private Cloud
-- User Guide.
module Network.AWS.EC2.DeleteRouteTable where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteRouteTable :: Text
                 -- ^ The ID of the route table to be deleted.
                 -> DeleteRouteTable
deleteRouteTable p1 = DeleteRouteTable
    { drtsRouteTableId = p1
    , drtsDryRun = Nothing
    }

data DeleteRouteTable = DeleteRouteTable
    { drtsDryRun :: Maybe Bool
    , drtsRouteTableId :: !Text
      -- ^ The ID of the route table to be deleted.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteRouteTable

instance AWSRequest DeleteRouteTable where
    type Er DeleteRouteTable = EC2Error
    type Rs DeleteRouteTable = DeleteRouteTableResponse
    request = getQuery service "DeleteRouteTable"

data DeleteRouteTableResponse = DeleteRouteTableResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteRouteTableResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteRouteTableResponse"
