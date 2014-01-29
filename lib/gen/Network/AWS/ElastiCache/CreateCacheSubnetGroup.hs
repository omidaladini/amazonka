{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.CreateCacheSubnetGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CreateCacheSubnetGroup operation creates a new cache subnet group. Use
-- this parameter only when you are creating a cluster in an Amazon Virtual
-- Private Cloud (VPC). https://elasticache.amazonaws.com/
-- ?Action=CreateCacheSubnetGroup &CacheSubnetGroupName=myCachesubnetgroup
-- &CacheSubnetGroupDescription=My%20new%20CacheSubnetGroup
-- &Version=2013-06-15 &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-02-15T18%3A14%3A49.482Z &AWSAccessKeyId= &Signature=
-- 990524496922 My new CacheSubnetGroup myCachesubnetgroup Active
-- subnet-7c5b4115 us-east-1c Active subnet-7b5b4112 us-east-1b Active
-- subnet-3ea6bd57 us-east-1d ed662948-a57b-11df-9e38-7ffab86c801f.
module Network.AWS.ElastiCache.CreateCacheSubnetGroup where

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
createCacheSubnetGroup :: Text
                       -> Text
                       -> [Text]
                       -> CreateCacheSubnetGroup
createCacheSubnetGroup p1 p2 p3 = undefined $ CreateCacheSubnetGroup
    { ccsgnCacheSubnetGroupDescription = p1
    , ccsgnCacheSubnetGroupName = p2
    , ccsgnSubnetIds = p3
    }

data CreateCacheSubnetGroup = CreateCacheSubnetGroup
    { ccsgnCacheSubnetGroupDescription :: !Text
      -- ^ A description for the cache subnet group.
    , ccsgnCacheSubnetGroupName :: !Text
      -- ^ A name for the cache subnet group. This value is stored as a lowercase
      -- string. Constraints: Must contain no more than 255 alphanumeric characters
      -- or hyphens. Example: mysubnetgroup.
    , ccsgnSubnetIds :: [Text]
      -- ^ A list of VPC subnet IDs for the cache subnet group.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateCacheSubnetGroup

instance AWSRequest CreateCacheSubnetGroup where
    type Er CreateCacheSubnetGroup = ElastiCacheError
    type Rs CreateCacheSubnetGroup = CreateCacheSubnetGroupResponse
    request = getQuery service "CreateCacheSubnetGroup"

data CreateCacheSubnetGroupResponse = CreateCacheSubnetGroupResponse
    { ccsgnrsCacheSubnetGroup :: Maybe CacheSubnetGroup
      -- ^ Represents the output of one of the following operations:
      -- CreateCacheSubnetGroup ModifyCacheSubnetGroup.
    } deriving (Eq, Show, Generic)

instance FromXML CreateCacheSubnetGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CreateCacheSubnetGroupResponse"
        :| ["CreateCacheSubnetGroupResult"]
