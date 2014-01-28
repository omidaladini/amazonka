{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.CreateCacheCluster
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CreateCacheCluster operation creates a new cache cluster. All nodes in
-- the cache cluster run the same protocol-compliant cache engine software -
-- either Memcached or Redis. https://elasticache.us-east-1.amazonaws.com/
-- ?Action=CreateCacheCluster &NumCacheNodes=3 &CacheClusterId=mycache
-- &Engine=memcached &CacheSecurityGroupNames.member.1=default
-- &CacheNodeType=cache.m1.large &Version=2013-06-15 &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2011-07-27T00%3A33%3A09.322Z
-- &AWSAccessKeyId=YOUR-ACCESS-KEY &Signature=YOUR-SIGNATURE creating 3
-- memcached 1.4.5 true sun:08:00-sun:09:00 cache.m1.large default active
-- mycache aaf2e796-363f-11e0-a564-8f11342c56b0.
module Network.AWS.ElastiCache.CreateCacheCluster where

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

data CreateCacheCluster = CreateCacheCluster
    { cccmAutoMinorVersionUpgrade :: Maybe Bool
      -- ^ Determines whether minor engine upgrades will be applied automatically to
      -- the cache cluster during the maintenance window. A value of true allows
      -- these upgrades to occur; false disables automatic upgrades. Default: true.
    , cccmCacheClusterId :: !Text
      -- ^ The cache cluster identifier. This parameter is stored as a lowercase
      -- string. Constraints: Must contain from 1 to 20 alphanumeric characters or
      -- hyphens. First character must be a letter. Cannot end with a hyphen or
      -- contain two consecutive hyphens.
    , cccmCacheNodeType :: Maybe Text
      -- ^ The compute and memory capacity of the nodes in the cache cluster. Valid
      -- values for Memcached: cache.t1.micro | cache.m1.small | cache.m1.medium |
      -- cache.m1.large | cache.m1.xlarge | cache.m3.xlarge | cache.m3.2xlarge |
      -- cache.m2.xlarge | cache.m2.2xlarge | cache.m2.4xlarge | cache.c1.xlarge
      -- Valid values for Redis: cache.t1.micro | cache.m1.small | cache.m1.medium |
      -- cache.m1.large | cache.m1.xlarge | cache.m2.xlarge | cache.m2.2xlarge |
      -- cache.m2.4xlarge | cache.c1.xlarge For a complete listing of cache node
      -- types and specifications, see .
    , cccmCacheParameterGroupName :: Maybe Text
      -- ^ The name of the cache parameter group to associate with this cache cluster.
      -- If this argument is omitted, the default cache parameter group for the
      -- specified engine will be used.
    , cccmCacheSecurityGroupNames :: [Text]
      -- ^ A list of cache security group names to associate with this cache cluster.
      -- Use this parameter only when you are creating a cluster outside of an
      -- Amazon Virtual Private Cloud (VPC).
    , cccmCacheSubnetGroupName :: Maybe Text
      -- ^ The name of the cache subnet group to be used for the cache cluster. Use
      -- this parameter only when you are creating a cluster in an Amazon Virtual
      -- Private Cloud (VPC).
    , cccmEngine :: Maybe Text
      -- ^ The name of the cache engine to be used for this cache cluster. Valid
      -- values for this parameter are: memcached | redis.
    , cccmEngineVersion :: Maybe Text
      -- ^ The version number of the cache engine to be used for this cluster. To view
      -- the supported cache engine versions, use the DescribeCacheEngineVersions
      -- operation.
    , cccmNotificationTopicArn :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
      -- (SNS) topic to which notifications will be sent. The Amazon SNS topic owner
      -- must be the same as the cache cluster owner.
    , cccmNumCacheNodes :: Maybe Int
      -- ^ The initial number of cache nodes that the cache cluster will have. For a
      -- Memcached cluster, valid values are between 1 and 20. If you need to exceed
      -- this limit, please fill out the ElastiCache Limit Increase Request form at
      -- . For Redis, only single-node cache clusters are supported at this time, so
      -- the value for this parameter must be 1.
    , cccmPort :: Maybe Int
      -- ^ The port number on which each of the cache nodes will accept connections.
    , cccmPreferredAvailabilityZone :: Maybe Text
      -- ^ The EC2 Availability Zone in which the cache cluster will be created. All
      -- cache nodes belonging to a cache cluster are placed in the preferred
      -- availability zone. Default: System chosen availability zone.
    , cccmPreferredMaintenanceWindow :: Maybe Text
      -- ^ The weekly time range (in UTC) during which system maintenance can occur.
      -- Example: sun:05:00-sun:09:00.
    , cccmReplicationGroupId :: Maybe Text
      -- ^ The replication group to which this cache cluster should belong. If this
      -- parameter is specified, the cache cluster will be added to the specified
      -- replication group as a read replica; otherwise, the cache cluster will be a
      -- standalone primary that is not part of any replication group.
    , cccmSecurityGroupIds :: [Text]
      -- ^ One or more VPC security groups associated with the cache cluster. Use this
      -- parameter only when you are creating a cluster in an Amazon Virtual Private
      -- Cloud (VPC).
    , cccmSnapshotArns :: [Text]
      -- ^ A single-element string list containing an Amazon Resource Name (ARN) that
      -- uniquely identifies a Redis RDB snapshot file stored in Amazon S3. The
      -- snapshot file will be used to populate the Redis cache in the new cache
      -- cluster. The Amazon S3 object name in the ARN cannot contain any commas.
      -- Here is an example of an Amazon S3 ARN:
      -- arn:aws:s3:::my_bucket/snapshot1.rdb Note: This parameter is only valid if
      -- the Engine parameter is redis.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateCacheCluster

instance AWSRequest CreateCacheCluster where
    type Er CreateCacheCluster = ElastiCacheError
    type Rs CreateCacheCluster = CreateCacheClusterResponse
    request = getQuery service "CreateCacheCluster"

data CreateCacheClusterResponse = CreateCacheClusterResponse
    { cccmrsCacheCluster :: Maybe CacheCluster
      -- ^ Contains all of the attributes of a specific cache cluster.
    } deriving (Eq, Show, Generic)

instance FromXML CreateCacheClusterResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CreateCacheClusterResponse"
        :| ["CreateCacheClusterResult"]
