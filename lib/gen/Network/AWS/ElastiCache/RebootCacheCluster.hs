{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.RebootCacheCluster
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The RebootCacheCluster operation reboots some, or all, of the cache cluster
-- nodes within a provisioned cache cluster. This API will apply any modified
-- cache parameter groups to the cache cluster. The reboot action takes place
-- as soon as possible, and results in a momentary outage to the cache
-- cluster. During the reboot, the cache cluster status is set to REBOOTING.
-- The reboot causes the contents of the cache (for each cache cluster node
-- being rebooted) to be lost. When the reboot is complete, a cache cluster
-- event is created.
module Network.AWS.ElastiCache.RebootCacheCluster where

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
rebootCacheCluster :: Text
                   -> [Text]
                   -> AWS (Either ElastiCacheError RebootCacheClusterResponse)
rebootCacheCluster p1 p2 = undefined $ RebootCacheCluster
    { rccmCacheClusterId = p1
    , rccmCacheNodeIdsToReboot = p2
    }

data RebootCacheCluster = RebootCacheCluster
    { rccmCacheClusterId :: !Text
      -- ^ The cache cluster identifier. This parameter is stored as a lowercase
      -- string.
    , rccmCacheNodeIdsToReboot :: [Text]
      -- ^ A list of cache cluster node IDs to reboot. A node ID is a numeric
      -- identifier (0001, 0002, etc.). To reboot an entire cache cluster, specify
      -- all of the cache cluster node IDs.
    } deriving (Eq, Show, Generic)

instance ToQuery RebootCacheCluster

instance AWSRequest RebootCacheCluster where
    type Er RebootCacheCluster = ElastiCacheError
    type Rs RebootCacheCluster = RebootCacheClusterResponse
    request = getQuery service "RebootCacheCluster"

data RebootCacheClusterResponse = RebootCacheClusterResponse
    { rccmrsCacheCluster :: Maybe CacheCluster
      -- ^ Contains all of the attributes of a specific cache cluster.
    } deriving (Eq, Show, Generic)

instance FromXML RebootCacheClusterResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "RebootCacheClusterResponse"
        :| ["RebootCacheClusterResult"]
