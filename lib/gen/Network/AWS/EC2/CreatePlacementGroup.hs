{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreatePlacementGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a PlacementGroup into which multiple Amazon EC2 instances can be
-- launched. Users must give the group a name unique within the scope of the
-- user account.
module Network.AWS.EC2.CreatePlacementGroup where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
createPlacementGroup :: Text
                     -- ^ The name of the PlacementGroup.
                     -> PlacementStrategy
                     -- ^ The PlacementGroup strategy.
                     -> CreatePlacementGroup
createPlacementGroup p1 p2 = CreatePlacementGroup
    { cpgGroupName = p1
    , cpgStrategy = p2
    , cpgDryRun = Nothing
    }

data CreatePlacementGroup = CreatePlacementGroup
    { cpgDryRun :: Maybe Bool
    , cpgGroupName :: !Text
      -- ^ The name of the PlacementGroup.
    , cpgStrategy :: !PlacementStrategy
      -- ^ The PlacementGroup strategy.
    } deriving (Eq, Show, Generic)

instance ToQuery CreatePlacementGroup

instance AWSRequest CreatePlacementGroup where
    type Er CreatePlacementGroup = EC2Error
    type Rs CreatePlacementGroup = CreatePlacementGroupResponse
    request  = postQuery service "CreatePlacementGroup"
    response = responseXML

data CreatePlacementGroupResponse = CreatePlacementGroupResponse
    deriving (Eq, Show, Generic)

instance FromXML CreatePlacementGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreatePlacementGroupResponse"
