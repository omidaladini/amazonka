{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeletePlacementGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a PlacementGroup from a user's account. Terminate all Amazon EC2
-- instances in the placement group before deletion.
module Network.AWS.EC2.DeletePlacementGroup where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deletePlacementGroup :: Text
                     -- ^ The name of the PlacementGroup to delete.
                     -> DeletePlacementGroup
deletePlacementGroup p1 = DeletePlacementGroup
    { dpgGroupName = p1
    , dpgDryRun = Nothing
    }

data DeletePlacementGroup = DeletePlacementGroup
    { dpgDryRun :: Maybe Bool
    , dpgGroupName :: !Text
      -- ^ The name of the PlacementGroup to delete.
    } deriving (Eq, Show, Generic)

instance ToQuery DeletePlacementGroup

instance AWSRequest DeletePlacementGroup where
    type Er DeletePlacementGroup = EC2Error
    type Rs DeletePlacementGroup = DeletePlacementGroupResponse
    request  = postQuery service "DeletePlacementGroup"
    response = responseXML

data DeletePlacementGroupResponse = DeletePlacementGroupResponse
    deriving (Eq, Show, Generic)

instance FromXML DeletePlacementGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeletePlacementGroupResponse"