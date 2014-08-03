{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.V2014_06_15.CreateTags
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds or overwrites one or more tags for the specified EC2 resource or
-- resources. Each resource can have a maximum of 10 tags. Each tag consists
-- of a key and optional value. Tag keys must be unique per resource. For more
-- information about tags, see Tagging Your Resources in the Amazon Elastic
-- Compute Cloud User Guide. Example This example request adds (or overwrites)
-- two tags for an AMI and an instance. One of the tags is just a key
-- (webserver), with no value (we set the value to an empty string). The other
-- tag consists of a key (stack) and value (Production).
-- https://ec2.amazonaws.com/?Action=CreateTags &amp;ResourceId.1=ami-1a2b3c4d
-- &amp;ResourceId.2=i-7f4d3a2b &amp;Tag.1.Key=webserver &amp;Tag.1.Value=
-- &amp;Tag.2.Key=stack &amp;Tag.2.Value=Production &amp;AUTHPARAMS
-- xmlns="http://ec2.amazonaws.com/doc/2014-05-01/">
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE true.
module Network.AWS.EC2.V2014_06_15.CreateTags where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateTags' request.
createTags :: [Text] -- ^ '_ctrResources'
           -> [Tag] -- ^ '_ctrTags'
           -> CreateTags
createTags p1 p2 = CreateTags
    { _ctrResources = p1
    , _ctrTags = p2
    , _ctrDryRun = Nothing
    }

data CreateTags = CreateTags
    { _ctrResources :: [Text]
      -- ^ The IDs of one or more resources to tag. For example,
      -- ami-1a2b3c4d.
    , _ctrTags :: [Tag]
      -- ^ One or more tags. The value parameter is required, but if you
      -- don't want the tag to have a value, specify the parameter with no
      -- value, and we set the value to an empty string.
    , _ctrDryRun :: Maybe Bool
      -- ^ 
    } deriving (Generic)

makeLenses ''CreateTags

instance ToQuery CreateTags where
    toQuery = genericToQuery def

data CreateTagsResponse = CreateTagsResponse
    deriving (Eq, Show, Generic)

makeLenses ''CreateTagsResponse

instance AWSRequest CreateTags where
    type Sv CreateTags = EC2
    type Rs CreateTags = CreateTagsResponse

    request = post "CreateTags"
    response _ _ = return (Right CreateTagsResponse)