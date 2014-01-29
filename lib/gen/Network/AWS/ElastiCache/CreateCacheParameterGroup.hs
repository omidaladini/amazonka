{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.CreateCacheParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CreateCacheParameterGroup operation creates a new cache parameter
-- group. A cache parameter group is a collection of parameters that you apply
-- to all of the nodes in a cache cluster.
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=CreateCacheParameterGroup
-- &Description=My%20first%20cache%20parameter%20group
-- &CacheParameterGroupFamily=memcached1.4
-- &CacheParameterGroupName=mycacheparametergroup1 &Version=2013-06-15
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-07-27T02%3A34%3A47.462Z &AWSAccessKeyId=YOUR-ACCESS-KEY
-- &Signature=YOUR-SIGNATURE mycacheparametergroup3 memcached1.4 My first
-- cache parameter group 05699541-b7f9-11e0-9326-b7275b9d4a6c.
module Network.AWS.ElastiCache.CreateCacheParameterGroup where

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

-- | Convenience method utilising default fields where applicable.
createCacheParameterGroup :: Text
                          -> Text
                          -> Text
                          -> AWS (Either ElastiCacheError CreateCacheParameterGroupResponse)
createCacheParameterGroup p1 p2 p3 = undefined $ CreateCacheParameterGroup
    { ccpgmCacheParameterGroupFamily = p1
    , ccpgmCacheParameterGroupName = p2
    , ccpgmDescription = p3
    }

data CreateCacheParameterGroup = CreateCacheParameterGroup
    { ccpgmCacheParameterGroupFamily :: !Text
      -- ^ The name of the cache parameter group family the cache parameter group can
      -- be used with. Valid values are: memcached1.4 | redis2.6.
    , ccpgmCacheParameterGroupName :: !Text
      -- ^ A user-specified name for the cache parameter group.
    , ccpgmDescription :: !Text
      -- ^ A user-specified description for the cache parameter group.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateCacheParameterGroup

instance AWSRequest CreateCacheParameterGroup where
    type Er CreateCacheParameterGroup = ElastiCacheError
    type Rs CreateCacheParameterGroup = CreateCacheParameterGroupResponse
    request = getQuery service "CreateCacheParameterGroup"

data CreateCacheParameterGroupResponse = CreateCacheParameterGroupResponse
    { ccpgmrsCacheParameterGroup :: Maybe CacheParameterGroup
      -- ^ Represents the output of a CreateCacheParameterGroup operation.
    } deriving (Eq, Show, Generic)

instance FromXML CreateCacheParameterGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CreateCacheParameterGroupResponse"
        :| ["CreateCacheParameterGroupResult"]
