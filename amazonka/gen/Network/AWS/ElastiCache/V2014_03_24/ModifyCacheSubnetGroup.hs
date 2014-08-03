{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ElastiCache.V2014_03_24.ModifyCacheSubnetGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ModifyCacheSubnetGroup operation modifies an existing cache subnet
-- group. https://elasticache.amazonaws.com/ ?Action=ModifyCacheSubnetGroup
-- &CacheSubnetGroupName=myCachesubnetgroup
-- &CacheSubnetGroupDescription=My%20modified%20CacheSubnetGroup
-- &Version=2014-03-24 &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2014-03-15T18%3A14%3A49.482Z &AWSAccessKeyId= &Signature=
-- 990524496922 My modified CacheSubnetGroup myCachesubnetgroup Active
-- subnet-7c5b4115 us-east-1c Active subnet-7b5b4112 us-east-1b Active
-- subnet-3ea6bd57 us-east-1d ed662948-a57b-11df-9e38-7ffab86c801f.
module Network.AWS.ElastiCache.V2014_03_24.ModifyCacheSubnetGroup where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_03_24.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ModifyCacheSubnetGroup' request.
modifyCacheSubnetGroup :: Text -- ^ '_mcsgmCacheSubnetGroupName'
                       -> ModifyCacheSubnetGroup
modifyCacheSubnetGroup p1 = ModifyCacheSubnetGroup
    { _mcsgmCacheSubnetGroupName = p1
    , _mcsgmCacheSubnetGroupDescription = Nothing
    , _mcsgmSubnetIds = mempty
    }

data ModifyCacheSubnetGroup = ModifyCacheSubnetGroup
    { _mcsgmCacheSubnetGroupName :: Text
      -- ^ The name for the cache subnet group. This value is stored as a
      -- lowercase string. Constraints: Must contain no more than 255
      -- alphanumeric characters or hyphens. Example: mysubnetgroup.
    , _mcsgmCacheSubnetGroupDescription :: Maybe Text
      -- ^ A description for the cache subnet group.
    , _mcsgmSubnetIds :: [Text]
      -- ^ The EC2 subnet IDs for the cache subnet group.
    } deriving (Generic)

makeLenses ''ModifyCacheSubnetGroup

instance ToQuery ModifyCacheSubnetGroup where
    toQuery = genericToQuery def

data ModifyCacheSubnetGroupResponse = ModifyCacheSubnetGroupResponse
    { _csgcrCacheSubnetGroup :: Maybe CacheSubnetGroup
      -- ^ Represents the output of one of the following operations:
      -- CreateCacheSubnetGroup ModifyCacheSubnetGroup.
    } deriving (Generic)

makeLenses ''ModifyCacheSubnetGroupResponse

instance FromXML ModifyCacheSubnetGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ModifyCacheSubnetGroup where
    type Sv ModifyCacheSubnetGroup = ElastiCache
    type Rs ModifyCacheSubnetGroup = ModifyCacheSubnetGroupResponse

    request = post "ModifyCacheSubnetGroup"
    response _ = xmlResponse