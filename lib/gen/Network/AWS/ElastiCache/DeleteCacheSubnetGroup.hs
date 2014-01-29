{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.DeleteCacheSubnetGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DeleteCacheSubnetGroup operation deletes a cache subnet group. You
-- cannot delete a cache subnet group if it is associated with any cache
-- clusters. https://elasticache.amazonaws.com/ ?Action=DeleteCacheSubnetGroup
-- &CacheSubnetGroupName=mysubnetgroup &Version=2013-06-15 &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2011-02-15T17%3A48%3A21.746Z
-- &AWSAccessKeyId= &Signature= 5d013245-4172-11df-8520-e7e1e602a915.
module Network.AWS.ElastiCache.DeleteCacheSubnetGroup where

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
deleteCacheSubnetGroup :: Text
                       -> DeleteCacheSubnetGroup
deleteCacheSubnetGroup p1 = undefined $ DeleteCacheSubnetGroup
    { dcsgpCacheSubnetGroupName = p1
    }

data DeleteCacheSubnetGroup = DeleteCacheSubnetGroup
    { dcsgpCacheSubnetGroupName :: !Text
      -- ^ The name of the cache subnet group to delete. Constraints: Must contain no
      -- more than 255 alphanumeric characters or hyphens.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteCacheSubnetGroup

instance AWSRequest DeleteCacheSubnetGroup where
    type Er DeleteCacheSubnetGroup = ElastiCacheError
    type Rs DeleteCacheSubnetGroup = DeleteCacheSubnetGroupResponse
    request = getQuery service "DeleteCacheSubnetGroup"

data DeleteCacheSubnetGroupResponse = DeleteCacheSubnetGroupResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteCacheSubnetGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DeleteCacheSubnetGroupResponse"
        :| ["DeleteCacheSubnetGroupResult"]
