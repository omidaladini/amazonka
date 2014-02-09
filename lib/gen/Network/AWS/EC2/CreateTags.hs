{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateTags
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds or overwrites tags for the specified resources. Each resource can have
-- a maximum of 10 tags. Each tag consists of a key-value pair. Tag keys must
-- be unique per resource.
module Network.AWS.EC2.CreateTags where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
createTags :: [Text]
           -- ^ One or more IDs of resources to tag. This could be the ID of an AMI, an
           -- instance, an EBS volume, or snapshot, etc.
           -> [Tag]
           -- ^ The tags to add or overwrite for the specified resources. Each tag item
           -- consists of a key-value pair.
           -> CreateTags
createTags p1 p2 = CreateTags
    { ctResources = p1
    , ctTags = p2
    , ctDryRun = Nothing
    }

data CreateTags = CreateTags
    { ctDryRun :: Maybe Bool
    , ctResources :: [Text]
      -- ^ One or more IDs of resources to tag. This could be the ID of an AMI, an
      -- instance, an EBS volume, or snapshot, etc.
    , ctTags :: [Tag]
      -- ^ The tags to add or overwrite for the specified resources. Each tag item
      -- consists of a key-value pair.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateTags

instance AWSRequest CreateTags where
    type Er CreateTags = EC2Error
    type Rs CreateTags = CreateTagsResponse
    request = getQuery service "CreateTags"

data CreateTagsResponse = CreateTagsResponse
    deriving (Eq, Show, Generic)

instance FromXML CreateTagsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateTagsResponse"
