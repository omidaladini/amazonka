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

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DeleteTags = DeleteTags
    { dtsDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , dtsResources :: [Text]
      -- ^ A list of one or more resource IDs. This could be the ID of an AMI, an
      -- instance, an EBS volume, or snapshot, etc.
    , dtsTags :: [Tag]
      -- ^ The tags to delete from the specified resources. Each tag item consists of
      -- a key-value pair. If a tag is specified without a value, the tag and all of
      -- its values are deleted.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteTags

instance AWSRequest DeleteTags where
    type Er DeleteTags = EC2Error
    type Rs DeleteTags = DeleteTagsResponse
    request = v2Query service GET "DeleteTags"

data DeleteTagsResponse = DeleteTagsResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteTagsResponse where
    fromXMLOptions = xmlOptions
