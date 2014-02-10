{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteTags
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes tags from the specified Amazon EC2 resources.
module Network.AWS.EC2.DeleteTags where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteTags :: [Text]
           -- ^ A list of one or more resource IDs. This could be the ID of an AMI, an
           -- instance, an EBS volume, or snapshot, etc.
           -> DeleteTags
deleteTags p1 = DeleteTags
    { dtdResources = p1
    , dtdDryRun = Nothing
    , dtdTags = []
    }

data DeleteTags = DeleteTags
    { dtdDryRun :: Maybe Bool
    , dtdResources :: [Text]
      -- ^ A list of one or more resource IDs. This could be the ID of an AMI, an
      -- instance, an EBS volume, or snapshot, etc.
    , dtdTags :: [Tag]
      -- ^ The tags to delete from the specified resources. Each tag item consists of
      -- a key-value pair. If a tag is specified without a value, the tag and all of
      -- its values are deleted.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteTags

instance AWSRequest DeleteTags where
    type Er DeleteTags = EC2Error
    type Rs DeleteTags = DeleteTagsResponse
    request  = postQuery service "DeleteTags"
    response = responseXML

data DeleteTagsResponse = DeleteTagsResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteTagsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteTagsResponse"
