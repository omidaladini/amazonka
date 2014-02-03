{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.ModifyCacheSubnetGroup
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
-- &Version=2013-06-15 &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-02-15T18%3A14%3A49.482Z &AWSAccessKeyId= &Signature=
-- 990524496922 My modified CacheSubnetGroup myCachesubnetgroup Active
-- subnet-7c5b4115 us-east-1c Active subnet-7b5b4112 us-east-1b Active
-- subnet-3ea6bd57 us-east-1d ed662948-a57b-11df-9e38-7ffab86c801f.
module Network.AWS.ElastiCache.ModifyCacheSubnetGroup where

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

import Network.AWS.ElastiCache.Service
import Network.AWS.ElastiCache.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
modifyCacheSubnetGroup :: Text
                       -> ModifyCacheSubnetGroup
modifyCacheSubnetGroup p1 = ModifyCacheSubnetGroup
    { mcsgmCacheSubnetGroupName = p1
    , mcsgmCacheSubnetGroupDescription = Nothing
    , mcsgmSubnetIds = []
    }

data ModifyCacheSubnetGroup = ModifyCacheSubnetGroup
    { mcsgmCacheSubnetGroupDescription :: Maybe Text
      -- ^ A description for the cache subnet group.
    , mcsgmCacheSubnetGroupName :: !Text
      -- ^ The name for the cache subnet group. This value is stored as a lowercase
      -- string. Constraints: Must contain no more than 255 alphanumeric characters
      -- or hyphens. Example: mysubnetgroup.
    , mcsgmSubnetIds :: [Text]
      -- ^ The EC2 subnet IDs for the cache subnet group.
    } deriving (Eq, Show, Generic)

instance ToQuery ModifyCacheSubnetGroup

instance AWSRequest ModifyCacheSubnetGroup where
    type Er ModifyCacheSubnetGroup = ElastiCacheError
    type Rs ModifyCacheSubnetGroup = ModifyCacheSubnetGroupResponse
    request = getQuery service "ModifyCacheSubnetGroup"

data ModifyCacheSubnetGroupResponse = ModifyCacheSubnetGroupResponse
    { mcsgmrsCacheSubnetGroup :: Maybe CacheSubnetGroup
      -- ^ Represents the output of one of the following operations:
      -- CreateCacheSubnetGroup ModifyCacheSubnetGroup.
    } deriving (Eq, Show, Generic)

instance FromXML ModifyCacheSubnetGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ModifyCacheSubnetGroupResponse"
        :| ["ModifyCacheSubnetGroupResult"]
