{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.DeleteCacheCluster
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DeleteCacheCluster operation deletes a previously provisioned cache
-- cluster. DeleteCacheCluster deletes all associated cache nodes, node
-- endpoints and the cache cluster itself. When you receive a successful
-- response from this operation, Amazon ElastiCache immediately begins
-- deleting the cache cluster; you cannot cancel or revert this operation.
-- https://elasticache.us-east-1.amazonaws.com/ ?Action=DeleteCacheCluster
-- &CacheClusterId=simcoprod43&Version=2013-06-15 &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2011-07-27T02%3A46%3A46.129Z
-- &AWSAccessKeyId=YOUR-ACCESS-KEY &Signature=YOUR-SIGNATURE in-sync
-- default.memcached1.4 simcoprod43 deleting 11211
-- simcoprod43.m2st2p.cfg.cache.amazonaws.com cache.m1.large memcached
-- us-east-1b 2011-07-27T02:18:26.497Z 1.4.5 true mon:05:00-mon:05:30 default
-- active 3 ab84aa7e-b7fa-11e0-9b0b-a9261be2b354.
module Network.AWS.ElastiCache.DeleteCacheCluster where

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
deleteCacheCluster :: Text
                   -> DeleteCacheCluster
deleteCacheCluster p1 = DeleteCacheCluster
    { dccmCacheClusterId = p1
    }

data DeleteCacheCluster = DeleteCacheCluster
    { dccmCacheClusterId :: !Text
      -- ^ The cache cluster identifier for the cluster to be deleted. This parameter
      -- is not case sensitive.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteCacheCluster

instance AWSRequest DeleteCacheCluster where
    type Er DeleteCacheCluster = ElastiCacheError
    type Rs DeleteCacheCluster = DeleteCacheClusterResponse
    request = getQuery service "DeleteCacheCluster"

data DeleteCacheClusterResponse = DeleteCacheClusterResponse
    { dccmrsCacheCluster :: Maybe CacheCluster
      -- ^ Contains all of the attributes of a specific cache cluster.
    } deriving (Eq, Show, Generic)

instance FromXML DeleteCacheClusterResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DeleteCacheClusterResponse"
        :| ["DeleteCacheClusterResult"]
