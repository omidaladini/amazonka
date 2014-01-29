{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.ModifyReplicationGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ModifyReplicationGroup operation modifies the settings for a
-- replication group.
module Network.AWS.ElastiCache.ModifyReplicationGroup where

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
modifyReplicationGroup :: Text
                       -> AWS (Either ElastiCacheError ModifyReplicationGroupResponse)
modifyReplicationGroup p1 = undefined $ ModifyReplicationGroup
    { mrgmReplicationGroupId = p1
    , mrgmApplyImmediately = Nothing
    , mrgmAutoMinorVersionUpgrade = Nothing
    , mrgmCacheParameterGroupName = Nothing
    , mrgmCacheSecurityGroupNames = []
    , mrgmEngineVersion = Nothing
    , mrgmNotificationTopicArn = Nothing
    , mrgmNotificationTopicStatus = Nothing
    , mrgmPreferredMaintenanceWindow = Nothing
    , mrgmPrimaryClusterId = Nothing
    , mrgmReplicationGroupDescription = Nothing
    , mrgmSecurityGroupIds = []
    }

data ModifyReplicationGroup = ModifyReplicationGroup
    { mrgmApplyImmediately :: Maybe Bool
      -- ^ If true, this parameter causes the modifications in this request and any
      -- pending modifications to be applied, asynchronously and as soon as
      -- possible, regardless of the PreferredMaintenanceWindow setting for the
      -- replication group. If false, then changes to the nodes in the replication
      -- group are applied on the next maintenance reboot, or the next failure
      -- reboot, whichever occurs first. Valid values: true | false Default: false.
    , mrgmAutoMinorVersionUpgrade :: Maybe Bool
      -- ^ Determines whether minor engine upgrades will be applied automatically to
      -- all of the cache nodes in the replication group during the maintenance
      -- window. A value of true allows these upgrades to occur; false disables
      -- automatic upgrades.
    , mrgmCacheParameterGroupName :: Maybe Text
      -- ^ The name of the cache parameter group to apply to all of the cache nodes in
      -- this replication group. This change is asynchronously applied as soon as
      -- possible for parameters when the ApplyImmediately parameter is specified as
      -- true for this request.
    , mrgmCacheSecurityGroupNames :: [Text]
      -- ^ A list of cache security group names to authorize for the clusters in this
      -- replication group. This change is asynchronously applied as soon as
      -- possible. This parameter can be used only with replication groups
      -- containing cache clusters running outside of an Amazon Virtual Private
      -- Cloud (VPC). Constraints: Must contain no more than 255 alphanumeric
      -- characters. Must not be "Default".
    , mrgmEngineVersion :: Maybe Text
      -- ^ The upgraded version of the cache engine to be run on the nodes in the
      -- replication group..
    , mrgmNotificationTopicArn :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the SNS topic to which notifications will
      -- be sent. The SNS topic owner must be same as the replication group owner.
    , mrgmNotificationTopicStatus :: Maybe Text
      -- ^ The status of the Amazon SNS notification topic for the replication group.
      -- Notifications are sent only if the status is active. Valid values: active |
      -- inactive.
    , mrgmPreferredMaintenanceWindow :: Maybe Text
      -- ^ The weekly time range (in UTC) during which replication group system
      -- maintenance can occur. Note that system maintenance may result in an
      -- outage. This change is made immediately. If you are moving this window to
      -- the current time, there must be at least 120 minutes between the current
      -- time and end of the window to ensure that pending changes are applied.
    , mrgmPrimaryClusterId :: Maybe Text
      -- ^ If this parameter is specified, ElastiCache will promote each of the nodes
      -- in the specified cache cluster to the primary role. The nodes of all other
      -- clusters in the replication group will be read replicas.
    , mrgmReplicationGroupDescription :: Maybe Text
      -- ^ A description for the replication group. Maximum length is 255 characters.
    , mrgmReplicationGroupId :: !Text
      -- ^ The identifier of the replication group to modify.
    , mrgmSecurityGroupIds :: [Text]
      -- ^ Specifies the VPC Security Groups associated with the cache clusters in the
      -- replication group. This parameter can be used only with replication groups
      -- containing cache clusters running in an Amazon Virtual Private Cloud (VPC).
    } deriving (Eq, Show, Generic)

instance ToQuery ModifyReplicationGroup

instance AWSRequest ModifyReplicationGroup where
    type Er ModifyReplicationGroup = ElastiCacheError
    type Rs ModifyReplicationGroup = ModifyReplicationGroupResponse
    request = getQuery service "ModifyReplicationGroup"

data ModifyReplicationGroupResponse = ModifyReplicationGroupResponse
    { mrgmrsReplicationGroup :: Maybe ReplicationGroup
      -- ^ Contains all of the attributes of a specific replication group.
    } deriving (Eq, Show, Generic)

instance FromXML ModifyReplicationGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ModifyReplicationGroupResponse"
        :| ["ModifyReplicationGroupResult"]
