{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.ElastiCache.Types where

import Data.ByteString      (ByteString)
import Data.Hashable
import Data.Monoid
import Data.List.NonEmpty
import Data.Text            (Text)
import Data.HashMap.Strict  (HashMap)
import Data.Time
import GHC.Generics         (Generic)
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.ElastiCache.Service

-- | Represents the subnet associated with a cache cluster. This parameter
-- refers to subnets defined in Amazon Virtual Private Cloud (Amazon VPC) and
-- used with ElastiCache.
data Subnet = Subnet
    { sSubnetAvailabilityZone :: Maybe AvailabilityZone
      -- ^ The Availability Zone associated with the subnet.
    , sSubnetIdentifier :: Maybe Text
      -- ^ The unique identifier for the subnet.
    } deriving (Eq, Show, Generic)

instance ToQuery Subnet

instance FromXML Subnet where
    fromXMLOptions = xmlOptions

instance ToXML Subnet where
    toXMLOptions = xmlOptions

-- | Represents a single cache security group and its status..
data SecurityGroupMembership = SecurityGroupMembership
    { sgmSecurityGroupId :: Maybe Text
      -- ^ The identifier of the cache security group.
    , sgmStatus :: Maybe Text
      -- ^ The status of the cache security group membership. The status changes
      -- whenever a cache security group is modified, or when the cache security
      -- groups assigned to a cache cluster are modified.
    } deriving (Eq, Show, Generic)

instance ToQuery SecurityGroupMembership

instance FromXML SecurityGroupMembership where
    fromXMLOptions = xmlOptions

instance ToXML SecurityGroupMembership where
    toXMLOptions = xmlOptions

-- | Describes all of the attributes of a reserved cache node offering.
data ReservedCacheNodesOffering = ReservedCacheNodesOffering
    { rcnoCacheNodeType :: Maybe Text
      -- ^ The cache node type for the reserved cache node.
    , rcnoDuration :: Maybe Int
      -- ^ The duration of the offering. in seconds.
    , rcnoFixedPrice :: Maybe Double
      -- ^ The fixed price charged for this offering.
    , rcnoOfferingType :: Maybe Text
      -- ^ The offering type.
    , rcnoProductDescription :: Maybe Text
      -- ^ The cache engine used by the offering.
    , rcnoRecurringCharges :: [RecurringCharge]
      -- ^ The recurring price charged to run this reserved cache node.
    , rcnoReservedCacheNodesOfferingId :: Maybe Text
      -- ^ A unique identifier for the reserved cache node offering.
    , rcnoUsagePrice :: Maybe Double
      -- ^ The hourly price charged for this offering.
    } deriving (Eq, Show, Generic)

instance ToQuery ReservedCacheNodesOffering

instance FromXML ReservedCacheNodesOffering where
    fromXMLOptions = xmlOptions

instance ToXML ReservedCacheNodesOffering where
    toXMLOptions = xmlOptions

-- | Represents the output of a PurchaseReservedCacheNodesOffering operation.
data ReservedCacheNode = ReservedCacheNode
    { rcnCacheNodeCount :: Maybe Int
      -- ^ The number of cache nodes that have been reserved.
    , rcnCacheNodeType :: Maybe Text
      -- ^ The cache node type for the reserved cache nodes.
    , rcnDuration :: Maybe Int
      -- ^ The duration of the reservation in seconds.
    , rcnFixedPrice :: Maybe Double
      -- ^ The fixed price charged for this reserved cache node.
    , rcnOfferingType :: Maybe Text
      -- ^ The offering type of this reserved cache node.
    , rcnProductDescription :: Maybe Text
      -- ^ The description of the reserved cache node.
    , rcnRecurringCharges :: [RecurringCharge]
      -- ^ The recurring price charged to run this reserved cache node.
    , rcnReservedCacheNodeId :: Maybe Text
      -- ^ The unique identifier for the reservation.
    , rcnReservedCacheNodesOfferingId :: Maybe Text
      -- ^ The offering identifier.
    , rcnStartTime :: Maybe UTCTime
      -- ^ The time the reservation started.
    , rcnState :: Maybe Text
      -- ^ The state of the reserved cache node.
    , rcnUsagePrice :: Maybe Double
      -- ^ The hourly price charged for this reserved cache node.
    } deriving (Eq, Show, Generic)

instance ToQuery ReservedCacheNode

instance FromXML ReservedCacheNode where
    fromXMLOptions = xmlOptions

instance ToXML ReservedCacheNode where
    toXMLOptions = xmlOptions

-- | A group of settings to be applied to the replication group, either
-- immediately or during the next maintenance window.
newtype ReplicationGroupPendingModifiedValues = ReplicationGroupPendingModifiedValues
    { rgpmvPrimaryClusterId :: Maybe Text
      -- ^ The primary cluster ID which will be applied immediately (if
      -- --apply-immediately was specified), or during the next maintenance window.
    } deriving (Eq, Show, Generic)

instance ToQuery ReplicationGroupPendingModifiedValues

instance FromXML ReplicationGroupPendingModifiedValues where
    fromXMLOptions = xmlOptions

instance ToXML ReplicationGroupPendingModifiedValues where
    toXMLOptions = xmlOptions

-- | Contains all of the attributes of a specific replication group.
data ReplicationGroup = ReplicationGroup
    { rgDescription :: Maybe Text
      -- ^ The description of the replication group.
    , rgMemberClusters :: [Text]
      -- ^ The names of all the cache clusters that are part of this replication
      -- group.
    , rgNodeGroups :: [NodeGroup]
      -- ^ A single element list with information about the nodes in the replication
      -- group.
    , rgPendingModifiedValues :: Maybe ReplicationGroupPendingModifiedValues
      -- ^ A group of settings to be applied to the replication group, either
      -- immediately or during the next maintenance window.
    , rgReplicationGroupId :: Maybe Text
      -- ^ The identifier for the replication group.
    , rgStatus :: Maybe Text
      -- ^ The current state of this replication group - creating, available, etc.
    } deriving (Eq, Show, Generic)

instance ToQuery ReplicationGroup

instance FromXML ReplicationGroup where
    fromXMLOptions = xmlOptions

instance ToXML ReplicationGroup where
    toXMLOptions = xmlOptions

-- | Contains the specific price and frequency of a recurring charges for a
-- reserved cache node, or for a reserved cache node offering.
data RecurringCharge = RecurringCharge
    { rcRecurringChargeAmount :: Maybe Double
      -- ^ The monetary amount of the recurring charge.
    , rcRecurringChargeFrequency :: Maybe Text
      -- ^ The frequency of the recurring charge.
    } deriving (Eq, Show, Generic)

instance ToQuery RecurringCharge

instance FromXML RecurringCharge where
    fromXMLOptions = xmlOptions

instance ToXML RecurringCharge where
    toXMLOptions = xmlOptions

-- | A group of settings that will be applied to the cache cluster in the
-- future, or that are currently being applied.
data PendingModifiedValues = PendingModifiedValues
    { pmvCacheNodeIdsToRemove :: [Text]
      -- ^ A list of cache node IDs that are being removed (or will be removed) from
      -- the cache cluster. A node ID is a numeric identifier (0001, 0002, etc.).
    , pmvEngineVersion :: Maybe Text
      -- ^ The new cache engine version that the cache cluster will run.
    , pmvNumCacheNodes :: Maybe Int
      -- ^ The new number of cache nodes for the cache cluster.
    } deriving (Eq, Show, Generic)

instance ToQuery PendingModifiedValues

instance FromXML PendingModifiedValues where
    fromXMLOptions = xmlOptions

instance ToXML PendingModifiedValues where
    toXMLOptions = xmlOptions

-- | Describes a name-value pair that is used to update the value of a
-- parameter.
data ParameterNameValue = ParameterNameValue
    { pnvParameterName :: Maybe Text
      -- ^ The name of the parameter.
    , pnvParameterValue :: Maybe Text
      -- ^ The value of the parameter.
    } deriving (Eq, Show, Generic)

instance ToQuery ParameterNameValue

instance FromXML ParameterNameValue where
    fromXMLOptions = xmlOptions

instance ToXML ParameterNameValue where
    toXMLOptions = xmlOptions

-- | Describes an individual setting that controls some aspect of ElastiCache
-- behavior.
data Parameter = Parameter
    { pAllowedValues :: Maybe Text
      -- ^ The valid range of values for the parameter.
    , pDataType :: Maybe Text
      -- ^ The valid data type for the parameter.
    , pDescription :: Maybe Text
      -- ^ A description of the parameter.
    , pIsModifiable :: Maybe Bool
      -- ^ Indicates whether (true) or not (false) the parameter can be modified. Some
      -- parameters have security or operational implications that prevent them from
      -- being changed.
    , pMinimumEngineVersion :: Maybe Text
      -- ^ The earliest cache engine version to which the parameter can apply.
    , pParameterName :: Maybe Text
      -- ^ The name of the parameter.
    , pParameterValue :: Maybe Text
      -- ^ The value of the parameter.
    , pSource :: Maybe Text
      -- ^ The source of the parameter.
    } deriving (Eq, Show, Generic)

instance ToQuery Parameter

instance FromXML Parameter where
    fromXMLOptions = xmlOptions

instance ToXML Parameter where
    toXMLOptions = xmlOptions

-- | Describes a notification topic and its status. Notification topics are used
-- for publishing ElastiCache events to subscribers using Amazon Simple
-- Notification Service (SNS).
data NotificationConfiguration = NotificationConfiguration
    { ncTopicArn :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) that identifies the topic.
    , ncTopicStatus :: Maybe Text
      -- ^ The current state of the topic.
    } deriving (Eq, Show, Generic)

instance ToQuery NotificationConfiguration

instance FromXML NotificationConfiguration where
    fromXMLOptions = xmlOptions

instance ToXML NotificationConfiguration where
    toXMLOptions = xmlOptions

-- | Represents a single node within a node group.
data NodeGroupMember = NodeGroupMember
    { ngmCacheClusterId :: Maybe Text
      -- ^ The ID of the cache cluster to which the node belongs.
    , ngmCacheNodeId :: Maybe Text
      -- ^ The ID of the node within its cache cluster. A node ID is a numeric
      -- identifier (0001, 0002, etc.).
    , ngmCurrentRole :: Maybe Text
      -- ^ The role that is currently assigned to the node - primary or replica.
    , ngmPreferredAvailabilityZone :: Maybe Text
      -- ^ The name of the Availability Zone in which the node is located.
    , ngmReadEndpoint :: Maybe Endpoint
      -- ^ Represents the information required for client programs to connect to a
      -- cache node.
    } deriving (Eq, Show, Generic)

instance ToQuery NodeGroupMember

instance FromXML NodeGroupMember where
    fromXMLOptions = xmlOptions

instance ToXML NodeGroupMember where
    toXMLOptions = xmlOptions

-- | Represents a collection of cache nodes in a replication group.
data NodeGroup = NodeGroup
    { ngNodeGroupId :: Maybe Text
      -- ^ The identifier for the node group. A replication group contains only one
      -- node group; therefore, the node group ID is 0001.
    , ngNodeGroupMembers :: [NodeGroupMember]
      -- ^ A list containing information about individual nodes within the node group.
    , ngPrimaryEndpoint :: Maybe Endpoint
      -- ^ Represents the information required for client programs to connect to a
      -- cache node.
    , ngStatus :: Maybe Text
      -- ^ The current state of this replication group - creating, available, etc.
    } deriving (Eq, Show, Generic)

instance ToQuery NodeGroup

instance FromXML NodeGroup where
    fromXMLOptions = xmlOptions

instance ToXML NodeGroup where
    toXMLOptions = xmlOptions

-- | Represents a single occurrence of something interesting within the system.
-- Some examples of events are creating a cache cluster, adding or removing a
-- cache node, or rebooting a node.
data Event = Event
    { fDate :: Maybe UTCTime
      -- ^ The date and time when the event occurred.
    , fMessage :: Maybe Text
      -- ^ The text of the event.
    , fSourceIdentifier :: Maybe Text
      -- ^ The identifier for the source of the event. For example, if the event
      -- occurred at the cache cluster level, the identifier would be the name of
      -- the cache cluster.
    , fSourceType :: Maybe SourceType
      -- ^ Specifies the origin of this event - a cache cluster, a parameter group, a
      -- security group, etc.
    } deriving (Eq, Show, Generic)

instance ToQuery Event

instance FromXML Event where
    fromXMLOptions = xmlOptions

instance ToXML Event where
    toXMLOptions = xmlOptions

-- | Represents the output of a DescribeEngineDefaultParameters operation.
data EngineDefaults = EngineDefaults
    { edCacheNodeTypeSpecificParameters :: [CacheNodeTypeSpecificParameter]
      -- ^ A list of parameters specific to a particular cache node type. Each element
      -- in the list contains detailed information about one parameter.
    , edCacheParameterGroupFamily :: Maybe Text
      -- ^ Specifies the name of the cache parameter group family to which the engine
      -- default parameters apply.
    , edMarker :: Maybe Text
      -- ^ Provides an identifier to allow retrieval of paginated results.
    , edParameters :: [Parameter]
      -- ^ Contains a list of engine default parameters.
    } deriving (Eq, Show, Generic)

instance ToQuery EngineDefaults

instance FromXML EngineDefaults where
    fromXMLOptions = xmlOptions

instance ToXML EngineDefaults where
    toXMLOptions = xmlOptions

-- | Represents the information required for client programs to connect to a
-- cache node.
data Endpoint = Endpoint
    { eAddress :: Maybe Text
      -- ^ The DNS hostname of the cache node.
    , ePort :: Maybe Int
      -- ^ The port number that the cache engine is listening on.
    } deriving (Eq, Show, Generic)

instance ToQuery Endpoint

instance FromXML Endpoint where
    fromXMLOptions = xmlOptions

instance ToXML Endpoint where
    toXMLOptions = xmlOptions

-- | Provides ownership and status information for an Amazon EC2 security group.
data EC2SecurityGroup = EC2SecurityGroup
    { ec2sgEC2SecurityGroupName :: Maybe Text
      -- ^ The name of the Amazon EC2 security group.
    , ec2sgEC2SecurityGroupOwnerId :: Maybe Text
      -- ^ The AWS account ID of the Amazon EC2 security group owner.
    , ec2sgStatus :: Maybe Text
      -- ^ The status of the Amazon EC2 security group.
    } deriving (Eq, Show, Generic)

instance ToQuery EC2SecurityGroup

instance FromXML EC2SecurityGroup where
    fromXMLOptions = xmlOptions

instance ToXML EC2SecurityGroup where
    toXMLOptions = xmlOptions

-- | Represents the output of one of the following operations:
-- CreateCacheSubnetGroup ModifyCacheSubnetGroup.
data CacheSubnetGroup = CacheSubnetGroup
    { cshCacheSubnetGroupDescription :: Maybe Text
      -- ^ The description of the cache subnet group.
    , cshCacheSubnetGroupName :: Maybe Text
      -- ^ The name of the cache subnet group.
    , cshSubnets :: [Subnet]
      -- ^ A list of subnets associated with the cache subnet group.
    , cshVpcId :: Maybe Text
      -- ^ The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet
      -- group.
    } deriving (Eq, Show, Generic)

instance ToQuery CacheSubnetGroup

instance FromXML CacheSubnetGroup where
    fromXMLOptions = xmlOptions

instance ToXML CacheSubnetGroup where
    toXMLOptions = xmlOptions

-- | Represents a cache cluster's status within a particular cache security
-- group.
data CacheSecurityGroupMembership = CacheSecurityGroupMembership
    { csgmCacheSecurityGroupName :: Maybe Text
      -- ^ The name of the cache security group.
    , csgmStatus :: Maybe Text
      -- ^ The membership status in the cache security group. The status changes when
      -- a cache security group is modified, or when the cache security groups
      -- assigned to a cache cluster are modified.
    } deriving (Eq, Show, Generic)

instance ToQuery CacheSecurityGroupMembership

instance FromXML CacheSecurityGroupMembership where
    fromXMLOptions = xmlOptions

instance ToXML CacheSecurityGroupMembership where
    toXMLOptions = xmlOptions

-- | Represents the output of one of the following operations:
-- AuthorizeCacheSecurityGroupIngress CreateCacheSecurityGroup
-- RevokeCacheSecurityGroupIngress.
data CacheSecurityGroup = CacheSecurityGroup
    { csgCacheSecurityGroupName :: Maybe Text
      -- ^ The name of the cache security group.
    , csgDescription :: Maybe Text
      -- ^ The description of the cache security group.
    , csgEC2SecurityGroups :: [EC2SecurityGroup]
      -- ^ A list of Amazon EC2 security groups that are associated with this cache
      -- security group.
    , csgOwnerId :: Maybe Text
      -- ^ The AWS account ID of the cache security group owner.
    } deriving (Eq, Show, Generic)

instance ToQuery CacheSecurityGroup

instance FromXML CacheSecurityGroup where
    fromXMLOptions = xmlOptions

instance ToXML CacheSecurityGroup where
    toXMLOptions = xmlOptions

-- | The status of the cache parameter group.
data CacheParameterGroupStatus = CacheParameterGroupStatus
    { cpgsCacheNodeIdsToReboot :: [Text]
      -- ^ A list of the cache node IDs which need to be rebooted for parameter
      -- changes to be applied. A node ID is a numeric identifier (0001, 0002,
      -- etc.).
    , cpgsCacheParameterGroupName :: Maybe Text
      -- ^ The name of the cache parameter group.
    , cpgsParameterApplyStatus :: Maybe Text
      -- ^ The status of parameter updates.
    } deriving (Eq, Show, Generic)

instance ToQuery CacheParameterGroupStatus

instance FromXML CacheParameterGroupStatus where
    fromXMLOptions = xmlOptions

instance ToXML CacheParameterGroupStatus where
    toXMLOptions = xmlOptions

-- | Represents the output of a CreateCacheParameterGroup operation.
data CacheParameterGroup = CacheParameterGroup
    { cpgCacheParameterGroupFamily :: Maybe Text
      -- ^ The name of the cache parameter group family that this cache parameter
      -- group is compatible with.
    , cpgCacheParameterGroupName :: Maybe Text
      -- ^ The name of the cache parameter group.
    , cpgDescription :: Maybe Text
      -- ^ The description for this cache parameter group.
    } deriving (Eq, Show, Generic)

instance ToQuery CacheParameterGroup

instance FromXML CacheParameterGroup where
    fromXMLOptions = xmlOptions

instance ToXML CacheParameterGroup where
    toXMLOptions = xmlOptions

-- | A value that applies only to a certain cache node type.
data CacheNodeTypeSpecificValue = CacheNodeTypeSpecificValue
    { cntsvCacheNodeType :: Maybe Text
      -- ^ The cache node type for which this value applies.
    , cntsvValue :: Maybe Text
      -- ^ The value for the cache node type.
    } deriving (Eq, Show, Generic)

instance ToQuery CacheNodeTypeSpecificValue

instance FromXML CacheNodeTypeSpecificValue where
    fromXMLOptions = xmlOptions

instance ToXML CacheNodeTypeSpecificValue where
    toXMLOptions = xmlOptions

-- | A parameter that has a different value for each cache node type it is
-- applied to. For example, in a Redis cache cluster, a cache.m1.large cache
-- node type would have a larger maxmemory value than a cache.m1.small type.
data CacheNodeTypeSpecificParameter = CacheNodeTypeSpecificParameter
    { cntspAllowedValues :: Maybe Text
      -- ^ The valid range of values for the parameter.
    , cntspCacheNodeTypeSpecificValues :: [CacheNodeTypeSpecificValue]
      -- ^ A list of cache node types and their corresponding values for this
      -- parameter.
    , cntspDataType :: Maybe Text
      -- ^ The valid data type for the parameter.
    , cntspDescription :: Maybe Text
      -- ^ A description of the parameter.
    , cntspIsModifiable :: Maybe Bool
      -- ^ Indicates whether (true) or not (false) the parameter can be modified. Some
      -- parameters have security or operational implications that prevent them from
      -- being changed.
    , cntspMinimumEngineVersion :: Maybe Text
      -- ^ The earliest cache engine version to which the parameter can apply.
    , cntspParameterName :: Maybe Text
      -- ^ The name of the parameter.
    , cntspSource :: Maybe Text
      -- ^ The source of the parameter value.
    } deriving (Eq, Show, Generic)

instance ToQuery CacheNodeTypeSpecificParameter

instance FromXML CacheNodeTypeSpecificParameter where
    fromXMLOptions = xmlOptions

instance ToXML CacheNodeTypeSpecificParameter where
    toXMLOptions = xmlOptions

-- | Represents an individual cache node within a cache cluster. Each cache node
-- runs its own instance of the cluster's protocol-compliant caching software
-- - either Memcached or Redis.
data CacheNode = CacheNode
    { cnCacheNodeCreateTime :: Maybe UTCTime
      -- ^ The date and time the cache node was created.
    , cnCacheNodeId :: Maybe Text
      -- ^ The cache node identifier. A node ID is a numeric identifier (0001, 0002,
      -- etc.). The combination of cluster ID and node ID uniquely identifies every
      -- cache node used in a customer's AWS account.
    , cnCacheNodeStatus :: Maybe Text
      -- ^ The current state of this cache node.
    , cnEndpoint :: Maybe Endpoint
      -- ^ The hostname and IP address for connecting to this cache node.
    , cnParameterGroupStatus :: Maybe Text
      -- ^ The status of the parameter group applied to this cache node.
    , cnSourceCacheNodeId :: Maybe Text
      -- ^ The ID of the primary node to which this read replica node is synchronized.
      -- If this field is empty, then this node is not associated with a primary
      -- cache cluster.
    } deriving (Eq, Show, Generic)

instance ToQuery CacheNode

instance FromXML CacheNode where
    fromXMLOptions = xmlOptions

instance ToXML CacheNode where
    toXMLOptions = xmlOptions

-- | Provides all of the details about a particular cache engine version.
data CacheEngineVersion = CacheEngineVersion
    { cevCacheEngineDescription :: Maybe Text
      -- ^ The description of the cache engine.
    , cevCacheEngineVersionDescription :: Maybe Text
      -- ^ The description of the cache engine version.
    , cevCacheParameterGroupFamily :: Maybe Text
      -- ^ The name of the cache parameter group family associated with this cache
      -- engine.
    , cevEngine :: Maybe Text
      -- ^ The name of the cache engine.
    , cevEngineVersion :: Maybe Text
      -- ^ The version number of the cache engine.
    } deriving (Eq, Show, Generic)

instance ToQuery CacheEngineVersion

instance FromXML CacheEngineVersion where
    fromXMLOptions = xmlOptions

instance ToXML CacheEngineVersion where
    toXMLOptions = xmlOptions

-- | Contains all of the attributes of a specific cache cluster.
data CacheCluster = CacheCluster
    { ccAutoMinorVersionUpgrade :: Maybe Bool
      -- ^ If true, then minor version patches are applied automatically; if false,
      -- then automatic minor version patches are disabled.
    , ccCacheClusterCreateTime :: Maybe UTCTime
      -- ^ The date and time the cache cluster was created.
    , ccCacheClusterId :: Maybe Text
      -- ^ The user-supplied identifier of the cache cluster. This is a unique key
      -- that identifies a cache cluster.
    , ccCacheClusterStatus :: Maybe Text
      -- ^ The current state of this cache cluster - creating, available, etc.
    , ccCacheNodeType :: Maybe Text
      -- ^ The name of the compute and memory capacity node type for the cache
      -- cluster.
    , ccCacheNodes :: [CacheNode]
      -- ^ A list of cache nodes that are members of the cache cluster.
    , ccCacheParameterGroup :: Maybe CacheParameterGroupStatus
      -- ^ The status of the cache parameter group.
    , ccCacheSecurityGroups :: [CacheSecurityGroupMembership]
      -- ^ A list of cache security group elements, composed of name and status
      -- sub-elements.
    , ccCacheSubnetGroupName :: Maybe Text
      -- ^ The name of the cache subnet group associated with the cache cluster.
    , ccClientDownloadLandingPage :: Maybe Text
      -- ^ The URL of the web page where you can download the latest ElastiCache
      -- client library.
    , ccConfigurationEndpoint :: Maybe Endpoint
      -- ^ Represents the information required for client programs to connect to a
      -- cache node.
    , ccEngine :: Maybe Text
      -- ^ The name of the cache engine (memcached or redis) to be used for this cache
      -- cluster.
    , ccEngineVersion :: Maybe Text
      -- ^ The version of the cache engine version that is used in this cache cluster.
    , ccNotificationConfiguration :: Maybe NotificationConfiguration
      -- ^ Describes a notification topic and its status. Notification topics are used
      -- for publishing ElastiCache events to subscribers using Amazon Simple
      -- Notification Service (SNS).
    , ccNumCacheNodes :: Maybe Int
      -- ^ The number of cache nodes in the cache cluster.
    , ccPendingModifiedValues :: Maybe PendingModifiedValues
      -- ^ A group of settings that will be applied to the cache cluster in the
      -- future, or that are currently being applied.
    , ccPreferredAvailabilityZone :: Maybe Text
      -- ^ The name of the Availability Zone in which the cache cluster is located.
    , ccPreferredMaintenanceWindow :: Maybe Text
      -- ^ The time range (in UTC) during which weekly system maintenance can occur.
    , ccReplicationGroupId :: Maybe Text
      -- ^ The replication group to which this cache cluster belongs. If this field is
      -- empty, the cache cluster is not associated with any replication group.
    , ccSecurityGroups :: [SecurityGroupMembership]
      -- ^ A list of VPC Security Groups associated with the cache cluster.
    } deriving (Eq, Show, Generic)

instance ToQuery CacheCluster

instance FromXML CacheCluster where
    fromXMLOptions = xmlOptions

instance ToXML CacheCluster where
    toXMLOptions = xmlOptions

-- | The Availability Zone associated with the subnet.
newtype AvailabilityZone = AvailabilityZone
    { azName :: Maybe Text
      -- ^ The name of the availability zone.
    } deriving (Eq, Show, Generic)

instance ToQuery AvailabilityZone

instance FromXML AvailabilityZone where
    fromXMLOptions = xmlOptions

instance ToXML AvailabilityZone where
    toXMLOptions = xmlOptions

-- | The event source to retrieve events for. If no value is specified, all
-- events are returned. Valid values are: cache-cluster |
-- cache-parameter-group | cache-security-group | cache-subnet-group.
data SourceType
    = SourceTypeCacheCluster
    | SourceTypeCacheParameterGroup
    | SourceTypeCacheSecurityGroup
    | SourceTypeCacheSubnetGroup
      deriving (Eq, Ord, Generic)

instance Hashable SourceType

instance FromText SourceType where
    fromText "cache-cluster" = Right SourceTypeCacheCluster
    fromText "cache-parameter-group" = Right SourceTypeCacheParameterGroup
    fromText "cache-security-group" = Right SourceTypeCacheSecurityGroup
    fromText "cache-subnet-group" = Right SourceTypeCacheSubnetGroup
    fromText e = fromTextFail $ "Unrecognised SourceType: " <> e

instance Read SourceType where
    readsPrec _ = fromTextRead

instance ToText SourceType where
    toText SourceTypeCacheCluster = "cache-cluster"
    toText SourceTypeCacheParameterGroup = "cache-parameter-group"
    toText SourceTypeCacheSecurityGroup = "cache-security-group"
    toText SourceTypeCacheSubnetGroup = "cache-subnet-group"

instance Show SourceType where
    show = toTextShow

instance ToQuery SourceType where
    toQuery = toTextQuery

instance FromXML SourceType where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML SourceType where
    toXMLOptions = xmlOptions
    toXML        = toTextXML
