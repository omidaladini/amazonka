{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.DeleteTags
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes the specified tags or a set of tags from a set of resources.
module Network.AWS.AutoScaling.V2011_01_01.DeleteTags where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Region, Error)
import           Network.AWS.Request.Query
import           Network.AWS.AutoScaling.V2011_01_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data DeleteTags = DeleteTags
    { _dtuTags :: [Tag]
      -- ^ Each tag should be defined by its resource type, resource ID,
      -- key, value, and a propagate flag. Valid values are: Resource type
      -- = auto-scaling-group, Resource ID = AutoScalingGroupName,
      -- key=value, value=value, propagate=true or false.
    } deriving (Generic)

instance ToQuery DeleteTags where
    toQuery = genericToQuery def

instance AWSRequest DeleteTags where
    type Sv DeleteTags = AutoScaling
    type Rs DeleteTags = DeleteTagsResponse

    request = post "DeleteTags"
    response _ _ = return (Right DeleteTagsResponse)

data DeleteTagsResponse = DeleteTagsResponse
    deriving (Eq, Show, Generic)