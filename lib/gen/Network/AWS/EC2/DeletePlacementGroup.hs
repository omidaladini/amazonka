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

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DeletePlacementGroup = DeletePlacementGroup
    { dpgrDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , dpgrGroupName :: !Text
      -- ^ The name of the PlacementGroup to delete.
    } deriving (Eq, Show, Generic)

instance ToQuery DeletePlacementGroup

instance AWSRequest DeletePlacementGroup where
    type Er DeletePlacementGroup = EC2Error
    type Rs DeletePlacementGroup = DeletePlacementGroupResponse
    request = v2Query service GET "DeletePlacementGroup"

data DeletePlacementGroupResponse = DeletePlacementGroupResponse
    deriving (Eq, Show, Generic)

instance FromXML DeletePlacementGroupResponse where
    fromXMLOptions = xmlOptions
