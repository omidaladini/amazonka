{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EMR.AddTags
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds tags to an Amazon EMR resource. Tags make it easier to associate
-- clusters in various ways, such as grouping clusters to track your Amazon
-- EMR resource allocation costs. For more information, see Tagging Amazon EMR
-- Resources.
module Network.AWS.EMR.AddTags where

import           Control.Monad
import           Data.Aeson
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.EMR.Service
import Network.AWS.EMR.Types

data AddTags = AddTags
    { atiResourceId :: Maybe Text
      -- ^ The Amazon EMR resource identifier to which tags will be added. This value
      -- must be a cluster identifier.
    , atiTags :: [Tag]
      -- ^ A list of tags to associate with a cluster and propagate to Amazon EC2
      -- instances. Tags are user-defined key/value pairs that consist of a required
      -- key string with a maximum of 128 characters, and an optional value string
      -- with a maximum of 256 characters.
    } deriving (Eq, Show, Generic)

instance ToJSON AddTags

instance AWSRequest AddTags where
    type Er AddTags = EMRError
    type Rs AddTags = AddTagsResponse
    request  = getJSON service
    response = responseJSON

data AddTagsResponse = AddTagsResponse
    deriving (Eq, Show, Generic)

instance FromJSON AddTagsResponse
