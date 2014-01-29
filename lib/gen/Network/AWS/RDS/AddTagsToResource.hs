{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.AddTagsToResource
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds metadata tags to an Amazon RDS resource. These tags can also be used
-- with cost allocation reporting to track cost associated with Amazon RDS
-- resources, or used in Condition statement in IAM policy for Amazon RDS. For
-- an overview on tagging Amazon RDS resources, see Tagging Amazon RDS
-- Resources.
module Network.AWS.RDS.AddTagsToResource where

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

import Network.AWS.RDS.Service
import Network.AWS.RDS.Types

-- | Convenience method utilising default fields where applicable.
addTagsToResource :: Text
                  -> [Tag]
                  -> AWS (Either RDSError AddTagsToResourceResponse)
addTagsToResource p1 p2 = undefined $ AddTagsToResource
    { attrmResourceName = p1
    , attrmTags = p2
    }

data AddTagsToResource = AddTagsToResource
    { attrmResourceName :: !Text
      -- ^ The Amazon RDS resource the tags will be added to. This value is an Amazon
      -- Resource Name (ARN). For information about creating an ARN, see
      -- Constructing an RDS Amazon Resource Name (ARN).
    , attrmTags :: [Tag]
      -- ^ The tags to be assigned to the Amazon RDS resource.
    } deriving (Eq, Show, Generic)

instance ToQuery AddTagsToResource

instance AWSRequest AddTagsToResource where
    type Er AddTagsToResource = RDSError
    type Rs AddTagsToResource = AddTagsToResourceResponse
    request = getQuery service "AddTagsToResource"

data AddTagsToResourceResponse = AddTagsToResourceResponse
    deriving (Eq, Show, Generic)

instance FromXML AddTagsToResourceResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "AddTagsToResourceResponse"
        :| ["AddTagsToResourceResult"]
