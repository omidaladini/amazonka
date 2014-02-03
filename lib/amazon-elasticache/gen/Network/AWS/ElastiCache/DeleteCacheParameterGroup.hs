{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.DeleteCacheParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DeleteCacheParameterGroup operation deletes the specified cache
-- parameter group. You cannot delete a cache parameter group if it is
-- associated with any cache clusters.
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=DeleteCacheParameterGroup &CacheParameterGroupName=myparametergroup
-- &Version=2013-06-15 &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-07-25T21%3A16%3A39.166Z &AWSAccessKeyId=YOUR-ACCESS-KEY
-- &Signature=YOUR-SIGNATURE d0a417cb-575b-11e0-8869-cd22b4f9d96f.
module Network.AWS.ElastiCache.DeleteCacheParameterGroup where

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
deleteCacheParameterGroup :: Text
                          -> DeleteCacheParameterGroup
deleteCacheParameterGroup p1 = DeleteCacheParameterGroup
    { dcpgnCacheParameterGroupName = p1
    }

data DeleteCacheParameterGroup = DeleteCacheParameterGroup
    { dcpgnCacheParameterGroupName :: !Text
      -- ^ The name of the cache parameter group to delete. The specified cache
      -- security group must not be associated with any cache clusters.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteCacheParameterGroup

instance AWSRequest DeleteCacheParameterGroup where
    type Er DeleteCacheParameterGroup = ElastiCacheError
    type Rs DeleteCacheParameterGroup = DeleteCacheParameterGroupResponse
    request = getQuery service "DeleteCacheParameterGroup"

data DeleteCacheParameterGroupResponse = DeleteCacheParameterGroupResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteCacheParameterGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot DeleteCacheParameterGroupResponse
