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

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data CreateTags = CreateTags
    { ctrDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , ctrResources :: [Text]
      -- ^ One or more IDs of resources to tag. This could be the ID of an AMI, an
      -- instance, an EBS volume, or snapshot, etc.
    , ctrTags :: [Tag]
      -- ^ The tags to add or overwrite for the specified resources. Each tag item
      -- consists of a key-value pair.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateTags

instance AWSRequest CreateTags where
    type Er CreateTags = EC2Error
    type Rs CreateTags = CreateTagsResponse
    request = v2Query service GET "CreateTags"

data CreateTagsResponse = CreateTagsResponse
    deriving (Eq, Show, Generic)

instance FromXML CreateTagsResponse where
    fromXMLOptions = xmlOptions
