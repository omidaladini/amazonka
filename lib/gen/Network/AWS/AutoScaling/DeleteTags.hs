{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DeleteTags
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes the specified tags or a set of tags from a set of resources.
module Network.AWS.AutoScaling.DeleteTags where

import Network.AWS.Core
import Network.AWS.AutoScaling.Service
import Network.AWS.AutoScaling.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteTags :: [Tag]
           -- ^ Each tag should be defined by its resource type, resource ID, key, value,
           -- and a propagate flag. Valid values are: Resource type = auto-scaling-group,
           -- Resource ID = AutoScalingGroupName, key=value, value=value, propagate=true
           -- or false.
           -> DeleteTags
deleteTags p1 = DeleteTags
    { dtuTags = p1
    }

data DeleteTags = DeleteTags
    { dtuTags :: [Tag]
      -- ^ Each tag should be defined by its resource type, resource ID, key, value,
      -- and a propagate flag. Valid values are: Resource type = auto-scaling-group,
      -- Resource ID = AutoScalingGroupName, key=value, value=value, propagate=true
      -- or false.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteTags

instance AWSRequest DeleteTags where
    type Er DeleteTags = AutoScalingError
    type Rs DeleteTags = DeleteTagsResponse
    request = getQuery service "DeleteTags"

data DeleteTagsResponse = DeleteTagsResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteTagsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteTagsResponse"
