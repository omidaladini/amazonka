{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.ModifyCacheCluster
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ModifyCacheCluster operation modifies the settings for a cache cluster.
-- You can use this operation to change one or more cluster configuration
-- parameters by specifying the parameters and the new values.
-- https://elasticache.us-east-1.amazonaws.com/ ?Action=ModifyCacheCluster
-- &NumCacheNodes=5 &CacheClusterId=simcoprod01 &Version=2013-06-15
-- &ApplyImmediately=true &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-07-27T03%3A16%3A34.601Z &AWSAccessKeyId=YOUR-ACCESS-KEY
-- &Signature=YOUR-SIGNATURE in-sync default.memcached1.4 simcoprod01
-- available 11211 simcoprod01.m2st2p.cfg.cache.amazonaws.com cache.m1.large
-- memcached 5 us-east-1b 2011-07-26T23:45:20.937Z 1.4.5 true
-- fri:04:30-fri:05:00 default active 3 d5786c6d-b7fe-11e0-9326-b7275b9d4a6c.
module Network.AWS.ElastiCache.ModifyCacheCluster where

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
modifyCacheCluster :: Text
                   -> AWS (Either ElastiCacheError ModifyCacheClusterResponse)
modifyCacheCluster p1 = undefined $ ModifyCacheCluster
    { mccmCacheClusterId = p1
    , mccmApplyImmediately = Nothing
    , mccmAutoMinorVersionUpgrade = Nothing
    , mccmCacheNodeIdsToRemove = []
    , mccmCacheParameterGroupName = Nothing
    , mccmCacheSecurityGroupNames = []
    , mccmEngineVersion = Nothing
    , mccmNotificationTopicArn = Nothing
    , mccmNotificationTopicStatus = Nothing
    , mccmNumCacheNodes = Nothing
    , mccmPreferredMaintenanceWindow = Nothing
    , mccmSecurityGroupIds = []
    }

data ModifyCacheCluster = ModifyCacheCluster
    { mccmApplyImmediately :: Maybe Bool
      -- ^ If true, this parameter causes the modifications in this request and any
      -- pending modifications to be applied, asynchronously and as soon as
      -- possible, regardless of the PreferredMaintenanceWindow setting for the
      -- cache cluster. If false, then changes to the cache cluster are applied on
      -- the next maintenance reboot, or the next failure reboot, whichever occurs
      -- first. Valid values: true | false Default: false.
    , mccmAutoMinorVersionUpgrade :: Maybe Bool
      -- ^ If true, then minor engine upgrades will be applied automatically to the
      -- cache cluster during the maintenance window. Valid values: true | false
      -- Default: true.
    , mccmCacheClusterId :: !Text
      -- ^ The cache cluster identifier. This value is stored as a lowercase string.
    , mccmCacheNodeIdsToRemove :: [Text]
      -- ^ A list of cache node IDs to be removed. A node ID is a numeric identifier
      -- (0001, 0002, etc.). This parameter is only valid when NumCacheNodes is less
      -- than the existing number of cache nodes. The number of cache node IDs
      -- supplied in this parameter must match the difference between the existing
      -- number of cache nodes in the cluster and the value of NumCacheNodes in the
      -- request.
    , mccmCacheParameterGroupName :: Maybe Text
      -- ^ The name of the cache parameter group to apply to this cache cluster. This
      -- change is asynchronously applied as soon as possible for parameters when
      -- the ApplyImmediately parameter is specified as true for this request.
    , mccmCacheSecurityGroupNames :: [Text]
      -- ^ A list of cache security group names to authorize on this cache cluster.
      -- This change is asynchronously applied as soon as possible. This parameter
      -- can be used only with clusters that are created outside of an Amazon
      -- Virtual Private Cloud (VPC). Constraints: Must contain no more than 255
      -- alphanumeric characters. Must not be "Default".
    , mccmEngineVersion :: Maybe Text
      -- ^ The upgraded version of the cache engine to be run on the cache cluster
      -- nodes.
    , mccmNotificationTopicArn :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the SNS topic to which notifications will
      -- be sent. The SNS topic owner must be same as the cache cluster owner.
    , mccmNotificationTopicStatus :: Maybe Text
      -- ^ The status of the Amazon SNS notification topic. Notifications are sent
      -- only if the status is active. Valid values: active | inactive.
    , mccmNumCacheNodes :: Maybe Int
      -- ^ The number of cache nodes that the cache cluster should have. If the value
      -- for NumCacheNodes is greater than the existing number of cache nodes, then
      -- more nodes will be added. If the value is less than the existing number of
      -- cache nodes, then cache nodes will be removed. If you are removing cache
      -- nodes, you must use the CacheNodeIdsToRemove parameter to provide the IDs
      -- of the specific cache nodes to be removed.
    , mccmPreferredMaintenanceWindow :: Maybe Text
      -- ^ The weekly time range (in UTC) during which system maintenance can occur.
      -- Note that system maintenance may result in an outage. This change is made
      -- immediately. If you are moving this window to the current time, there must
      -- be at least 120 minutes between the current time and end of the window to
      -- ensure that pending changes are applied.
    , mccmSecurityGroupIds :: [Text]
      -- ^ Specifies the VPC Security Groups associated with the cache cluster. This
      -- parameter can be used only with clusters that are created in an Amazon
      -- Virtual Private Cloud (VPC).
    } deriving (Eq, Show, Generic)

instance ToQuery ModifyCacheCluster

instance AWSRequest ModifyCacheCluster where
    type Er ModifyCacheCluster = ElastiCacheError
    type Rs ModifyCacheCluster = ModifyCacheClusterResponse
    request = getQuery service "ModifyCacheCluster"

data ModifyCacheClusterResponse = ModifyCacheClusterResponse
    { mccmrsCacheCluster :: Maybe CacheCluster
      -- ^ Contains all of the attributes of a specific cache cluster.
    } deriving (Eq, Show, Generic)

instance FromXML ModifyCacheClusterResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ModifyCacheClusterResponse"
        :| ["ModifyCacheClusterResult"]
