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

import           Control.Monad
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import           Text.XML.Generic

import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields where applicable.
deleteTags :: [Text]
           -> AWS (Either EC2Error DeleteTagsResponse)
deleteTags p1 = undefined $ DeleteTags
    { dtsResources = p1
    , dtsDryRun = Nothing
    , dtsTags = []
    }

data DeleteTags = DeleteTags
    { dtsDryRun :: Maybe Bool
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
    request = getQuery service "DeleteTags"

data DeleteTagsResponse = DeleteTagsResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteTagsResponse where
    fromXMLOptions = xmlOptions
