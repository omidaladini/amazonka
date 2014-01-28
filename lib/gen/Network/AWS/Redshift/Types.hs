{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Redshift.Types where

import Data.ByteString      (ByteString)
import Data.Hashable
import Data.Monoid
import Data.List.NonEmpty
import Data.Text            (Text)
import Data.HashMap.Strict  (HashMap)
import Data.Time
import GHC.Generics         (Generic)
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.Redshift.Service

-- | Describes the members of a VPC security group.
data VpcSecurityGroupMembership = VpcSecurityGroupMembership
    { vsgmStatus :: Maybe Text
    , vsgmVpcSecurityGroupId :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery VpcSecurityGroupMembership

instance FromXML VpcSecurityGroupMembership where
    fromXMLOptions = xmlOptions

instance ToXML VpcSecurityGroupMembership where
    toXMLOptions = xmlOptions

-- | Describes a subnet.
data Subnet = Subnet
    { tSubnetAvailabilityZone :: Maybe AvailabilityZone
      -- ^ Describes an availability zone.
    , tSubnetIdentifier :: Maybe Text
      -- ^ The identifier of the subnet.
    , tSubnetStatus :: Maybe Text
      -- ^ The status of the subnet.
    } deriving (Eq, Show, Generic)

instance ToQuery Subnet

instance FromXML Subnet where
    fromXMLOptions = xmlOptions

instance ToXML Subnet where
    toXMLOptions = xmlOptions

-- | Describes a snapshot.
data Snapshot = Snapshot
    { sAccountsWithRestoreAccess :: [AccountWithRestoreAccess]
      -- ^ A list of the AWS customer accounts authorized to restore the snapshot.
      -- Returns null if no accounts are authorized. Visible only to the snapshot
      -- owner.
    , sActualIncrementalBackupSizeInMegaBytes :: Maybe Double
      -- ^ The size of the incremental backup.
    , sAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone in which the cluster was created.
    , sBackupProgressInMegaBytes :: Maybe Double
      -- ^ The number of megabytes that have been transferred to the snapshot backup.
    , sClusterCreateTime :: Maybe UTCTime
      -- ^ The time (UTC) when the cluster was originally created.
    , sClusterIdentifier :: Maybe Text
      -- ^ The identifier of the cluster for which the snapshot was taken.
    , sClusterVersion :: Maybe Text
      -- ^ The version ID of the Amazon Redshift engine that is running on the
      -- cluster.
    , sCurrentBackupRateInMegaBytesPerSecond :: Maybe Double
      -- ^ The number of megabytes per second being transferred to the snapshot
      -- backup. Returns 0 for a completed backup.
    , sDBName :: Maybe Text
      -- ^ The name of the database that was created when the cluster was created.
    , sElapsedTimeInSeconds :: Maybe Integer
      -- ^ The amount of time an in-progress snapshot backup has been running, or the
      -- amount of time it took a completed backup to finish.
    , sEncrypted :: Maybe Bool
      -- ^ If true, the data in the snapshot is encrypted at rest.
    , sEncryptedWithHSM :: Maybe Bool
      -- ^ A boolean that indicates whether the snapshot data is encrypted using the
      -- HSM keys of the source cluster. true indicates that the data is encrypted
      -- using HSM keys.
    , sEstimatedSecondsToCompletion :: Maybe Integer
      -- ^ The estimate of the time remaining before the snapshot backup will
      -- complete. Returns 0 for a completed backup.
    , sMasterUsername :: Maybe Text
      -- ^ The master user name for the cluster.
    , sNodeType :: Maybe Text
      -- ^ The node type of the nodes in the cluster.
    , sNumberOfNodes :: Maybe Int
      -- ^ The number of nodes in the cluster.
    , sOwnerAccount :: Maybe Text
      -- ^ For manual snapshots, the AWS customer account used to create or copy the
      -- snapshot. For automatic snapshots, the owner of the cluster. The owner can
      -- perform all snapshot actions, such as sharing a manual snapshot.
    , sPort :: Maybe Int
      -- ^ The port that the cluster is listening on.
    , sSnapshotCreateTime :: Maybe UTCTime
      -- ^ The time (UTC) when Amazon Redshift began the snapshot. A snapshot contains
      -- a copy of the cluster data as of this exact time.
    , sSnapshotIdentifier :: Maybe Text
      -- ^ The snapshot identifier that is provided in the request.
    , sSnapshotType :: Maybe Text
      -- ^ The snapshot type. Snapshots created using CreateClusterSnapshot and
      -- CopyClusterSnapshot will be of type "manual".
    , sSourceRegion :: Maybe Text
      -- ^ The source region from which the snapshot was copied.
    , sStatus :: Maybe Text
      -- ^ The snapshot status. The value of the status depends on the API operation
      -- used. CreateClusterSnapshot and CopyClusterSnapshot returns status as
      -- "creating". DescribeClusterSnapshots returns status as "creating",
      -- "available", or "failed". DeleteClusterSnapshot returns status as
      -- "deleted".
    , sTotalBackupSizeInMegaBytes :: Maybe Double
      -- ^ The size of the complete set of backup data that would be used to restore
      -- the cluster.
    , sVpcId :: Maybe Text
      -- ^ The VPC identifier of the cluster if the snapshot is from a cluster in a
      -- VPC. Otherwise, this field is not in the output.
    } deriving (Eq, Show, Generic)

instance ToQuery Snapshot

instance FromXML Snapshot where
    fromXMLOptions = xmlOptions

instance ToXML Snapshot where
    toXMLOptions = xmlOptions

-- | Describes the status of a cluster restore action. Returns null if the
-- cluster was not created by restoring a snapshot.
data RestoreStatus = RestoreStatus
    { rsCurrentRestoreRateInMegaBytesPerSecond :: Maybe Double
      -- ^ The number of megabytes per second being transferred from the backup
      -- storage. Returns the average rate for a completed backup.
    , rsElapsedTimeInSeconds :: Maybe Integer
      -- ^ The amount of time an in-progress restore has been running, or the amount
      -- of time it took a completed restore to finish.
    , rsEstimatedTimeToCompletionInSeconds :: Maybe Integer
      -- ^ The estimate of the time remaining before the restore will complete.
      -- Returns 0 for a completed restore.
    , rsProgressInMegaBytes :: Maybe Integer
      -- ^ The number of megabytes that have been transferred from snapshot storage.
    , rsSnapshotSizeInMegaBytes :: Maybe Integer
      -- ^ The size of the set of snapshot data used to restore the cluster.
    , rsStatus :: Maybe Text
      -- ^ The status of the restore action. Returns starting, restoring, completed,
      -- or failed.
    } deriving (Eq, Show, Generic)

instance ToQuery RestoreStatus

instance FromXML RestoreStatus where
    fromXMLOptions = xmlOptions

instance ToXML RestoreStatus where
    toXMLOptions = xmlOptions

-- | Describes a reserved node offering.
data ReservedNodeOffering = ReservedNodeOffering
    { rnoCurrencyCode :: Maybe Text
      -- ^ The currency code for the compute nodes offering.
    , rnoDuration :: Maybe Int
      -- ^ The duration, in seconds, for which the offering will reserve the node.
    , rnoFixedPrice :: Maybe Double
      -- ^ The upfront fixed charge you will pay to purchase the specific reserved
      -- node offering.
    , rnoNodeType :: Maybe Text
      -- ^ The node type offered by the reserved node offering.
    , rnoOfferingType :: Maybe Text
      -- ^ The anticipated utilization of the reserved node, as defined in the
      -- reserved node offering.
    , rnoRecurringCharges :: [RecurringCharge]
      -- ^ The charge to your account regardless of whether you are creating any
      -- clusters using the node offering. Recurring charges are only in effect for
      -- heavy-utilization reserved nodes.
    , rnoReservedNodeOfferingId :: Maybe Text
      -- ^ The offering identifier.
    , rnoUsagePrice :: Maybe Double
      -- ^ The rate you are charged for each hour the cluster that is using the
      -- offering is running.
    } deriving (Eq, Show, Generic)

instance ToQuery ReservedNodeOffering

instance FromXML ReservedNodeOffering where
    fromXMLOptions = xmlOptions

instance ToXML ReservedNodeOffering where
    toXMLOptions = xmlOptions

-- | Describes a reserved node.
data ReservedNode = ReservedNode
    { rnCurrencyCode :: Maybe Text
      -- ^ The currency code for the reserved cluster.
    , rnDuration :: Maybe Int
      -- ^ The duration of the node reservation in seconds.
    , rnFixedPrice :: Maybe Double
      -- ^ The fixed cost Amazon Redshift charged you for this reserved node.
    , rnNodeCount :: Maybe Int
      -- ^ The number of reserved compute nodes.
    , rnNodeType :: Maybe Text
      -- ^ The node type of the reserved node.
    , rnOfferingType :: Maybe Text
      -- ^ The anticipated utilization of the reserved node, as defined in the
      -- reserved node offering.
    , rnRecurringCharges :: [RecurringCharge]
      -- ^ The recurring charges for the reserved node.
    , rnReservedNodeId :: Maybe Text
      -- ^ The unique identifier for the reservation.
    , rnReservedNodeOfferingId :: Maybe Text
      -- ^ The identifier for the reserved node offering.
    , rnStartTime :: Maybe UTCTime
      -- ^ The time the reservation started. You purchase a reserved node offering for
      -- a duration. This is the start time of that duration.
    , rnState :: Maybe Text
      -- ^ The state of the reserved Compute Node. Possible Values:
      -- pending-payment-This reserved node has recently been purchased, and the
      -- sale has been approved, but payment has not yet been confirmed. active-This
      -- reserved node is owned by the caller and is available for use.
      -- payment-failed-Payment failed for the purchase attempt.
    , rnUsagePrice :: Maybe Double
      -- ^ The hourly rate Amazon Redshift charge you for this reserved node.
    } deriving (Eq, Show, Generic)

instance ToQuery ReservedNode

instance FromXML ReservedNode where
    fromXMLOptions = xmlOptions

instance ToXML ReservedNode where
    toXMLOptions = xmlOptions

-- | Describes a recurring charge.
data RecurringCharge = RecurringCharge
    { rcRecurringChargeAmount :: Maybe Double
      -- ^ The amount charged per the period of time specified by the recurring charge
      -- frequency.
    , rcRecurringChargeFrequency :: Maybe Text
      -- ^ The frequency at which the recurring charge amount is applied.
    } deriving (Eq, Show, Generic)

instance ToQuery RecurringCharge

instance FromXML RecurringCharge where
    fromXMLOptions = xmlOptions

instance ToXML RecurringCharge where
    toXMLOptions = xmlOptions

-- | If present, changes to the cluster are pending. Specific pending changes
-- are identified by subelements.
data PendingModifiedValues = PendingModifiedValues
    { pmvAutomatedSnapshotRetentionPeriod :: Maybe Int
      -- ^ The pending or in-progress change of the automated snapshot retention
      -- period.
    , pmvClusterType :: Maybe Text
      -- ^ The pending or in-progress change of the cluster type.
    , pmvClusterVersion :: Maybe Text
      -- ^ The pending or in-progress change of the service version.
    , pmvMasterUserPassword :: Maybe Text
      -- ^ The pending or in-progress change of the master password for the cluster.
    , pmvNodeType :: Maybe Text
      -- ^ The pending or in-progress change of the cluster's node type.
    , pmvNumberOfNodes :: Maybe Int
      -- ^ The pending or in-progress change of the number nodes in the cluster.
    } deriving (Eq, Show, Generic)

instance ToQuery PendingModifiedValues

instance FromXML PendingModifiedValues where
    fromXMLOptions = xmlOptions

instance ToXML PendingModifiedValues where
    toXMLOptions = xmlOptions

-- | Describes a parameter in a cluster parameter group.
data Parameter = Parameter
    { pAllowedValues :: Maybe Text
      -- ^ The valid range of values for the parameter.
    , pDataType :: Maybe Text
      -- ^ The data type of the parameter.
    , pDescription :: Maybe Text
      -- ^ A description of the parameter.
    , pIsModifiable :: Maybe Bool
      -- ^ If true, the parameter can be modified. Some parameters have security or
      -- operational implications that prevent them from being changed.
    , pMinimumEngineVersion :: Maybe Text
      -- ^ The earliest engine version to which the parameter can apply.
    , pParameterName :: Maybe Text
      -- ^ The name of the parameter.
    , pParameterValue :: Maybe Text
      -- ^ The value of the parameter.
    , pSource :: Maybe Text
      -- ^ The source of the parameter value, such as "engine-default" or "user".
    } deriving (Eq, Show, Generic)

instance ToQuery Parameter

instance FromXML Parameter where
    fromXMLOptions = xmlOptions

instance ToXML Parameter where
    toXMLOptions = xmlOptions

-- | Describes an orderable cluster option.
data OrderableClusterOption = OrderableClusterOption
    { ocoAvailabilityZones :: [AvailabilityZone]
      -- ^ A list of availability zones for the orderable cluster.
    , ocoClusterType :: Maybe Text
      -- ^ The cluster type, for example multi-node.
    , ocoClusterVersion :: Maybe Text
      -- ^ The version of the orderable cluster.
    , ocoNodeType :: Maybe Text
      -- ^ The node type for the orderable cluster.
    } deriving (Eq, Show, Generic)

instance ToQuery OrderableClusterOption

instance FromXML OrderableClusterOption where
    fromXMLOptions = xmlOptions

instance ToXML OrderableClusterOption where
    toXMLOptions = xmlOptions

-- | Describes an IP range used in a security group.
data IPRange = IPRange
    { iprCIDRIP :: Maybe Text
      -- ^ The IP range in Classless Inter-Domain Routing (CIDR) notation.
    , iprStatus :: Maybe Text
      -- ^ The status of the IP range, for example, "authorized".
    } deriving (Eq, Show, Generic)

instance ToQuery IPRange

instance FromXML IPRange where
    fromXMLOptions = xmlOptions

instance ToXML IPRange where
    toXMLOptions = xmlOptions

-- | Reports whether the Amazon Redshift cluster has finished applying any HSM
-- settings changes specified in a modify cluster command. Values: active,
-- applying.
data HsmStatus = HsmStatus
    { hsHsmClientCertificateIdentifier :: Maybe Text
      -- ^ Specifies the name of the HSM client certificate the Amazon Redshift
      -- cluster uses to retrieve the data encryption keys stored in an HSM.
    , hsHsmConfigurationIdentifier :: Maybe Text
      -- ^ Specifies the name of the HSM configuration that contains the information
      -- the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
    , hsStatus :: Maybe Text
      -- ^ Reports whether the Amazon Redshift cluster has finished applying any HSM
      -- settings changes specified in a modify cluster command. Values: active,
      -- applying.
    } deriving (Eq, Show, Generic)

instance ToQuery HsmStatus

instance FromXML HsmStatus where
    fromXMLOptions = xmlOptions

instance ToXML HsmStatus where
    toXMLOptions = xmlOptions

-- | Returns information about an HSM configuration, which is an object that
-- describes to Amazon Redshift clusters the information they require to
-- connect to an HSM where they can store database encryption keys.
data HsmConfiguration = HsmConfiguration
    { hcDescription :: Maybe Text
      -- ^ A text description of the HSM configuration.
    , hcHsmConfigurationIdentifier :: Maybe Text
      -- ^ The name of the Amazon Redshift HSM configuration.
    , hcHsmIpAddress :: Maybe Text
      -- ^ The IP address that the Amazon Redshift cluster must use to access the HSM.
    , hcHsmPartitionName :: Maybe Text
      -- ^ The name of the partition in the HSM where the Amazon Redshift clusters
      -- will store their database encryption keys.
    } deriving (Eq, Show, Generic)

instance ToQuery HsmConfiguration

instance FromXML HsmConfiguration where
    fromXMLOptions = xmlOptions

instance ToXML HsmConfiguration where
    toXMLOptions = xmlOptions

-- | Returns information about an HSM client certificate. The certificate is
-- stored in a secure Hardware Storage Module (HSM), and used by the Amazon
-- Redshift cluster to encrypt data files.
data HsmClientCertificate = HsmClientCertificate
    { hccHsmClientCertificateIdentifier :: Maybe Text
      -- ^ The identifier of the HSM client certificate.
    , hccHsmClientCertificatePublicKey :: Maybe Text
      -- ^ The public key that the Amazon Redshift cluster will use to retrieve the
      -- client certificate from the HSM. You must register the public key in the
      -- HSM.
    } deriving (Eq, Show, Generic)

instance ToQuery HsmClientCertificate

instance FromXML HsmClientCertificate where
    fromXMLOptions = xmlOptions

instance ToXML HsmClientCertificate where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for EventSubscription
data EventSubscription = EventSubscription
    { esCustSubscriptionId :: Maybe Text
      -- ^ The name of the Amazon Redshift event notification subscription.
    , esCustomerAwsId :: Maybe Text
      -- ^ The AWS customer account associated with the Amazon Redshift event
      -- notification subscription.
    , esEnabled :: Maybe Bool
      -- ^ A Boolean value indicating whether the subscription is enabled. true
      -- indicates the subscription is enabled.
    , esEventCategoriesList :: [Text]
      -- ^ The list of Amazon Redshift event categories specified in the event
      -- notification subscription. Values: Configuration, Management, Monitoring,
      -- Security.
    , esSeverity :: Maybe Text
      -- ^ The event severity specified in the Amazon Redshift event notification
      -- subscription. Values: ERROR, INFO.
    , esSnsTopicArn :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the Amazon SNS topic used by the event
      -- notification subscription.
    , esSourceIdsList :: [Text]
      -- ^ A list of the sources that publish events to the Amazon Redshift event
      -- notification subscription.
    , esSourceType :: Maybe Text
      -- ^ The source type of the events returned the Amazon Redshift event
      -- notification, such as cluster, or cluster-snapshot.
    , esStatus :: Maybe Text
      -- ^ The status of the Amazon Redshift event notification subscription.
      -- Constraints: Can be one of the following: active | no-permission |
      -- topic-not-exist The status "no-permission" indicates that Amazon Redshift
      -- no longer has permission to post to the Amazon SNS topic. The status
      -- "topic-not-exist" indicates that the topic was deleted after the
      -- subscription was created.
    , esSubscriptionCreationTime :: Maybe UTCTime
      -- ^ The date and time the Amazon Redshift event notification subscription was
      -- created.
    } deriving (Eq, Show, Generic)

instance ToQuery EventSubscription

instance FromXML EventSubscription where
    fromXMLOptions = xmlOptions

instance ToXML EventSubscription where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for EventInfoMap
data EventInfoMap = EventInfoMap
    { eimEventCategories :: [Text]
      -- ^ The category of an Amazon Redshift event.
    , eimEventDescription :: Maybe Text
      -- ^ The description of an Amazon Redshift event.
    , eimEventId :: Maybe Text
      -- ^ The identifier of an Amazon Redshift event.
    , eimSeverity :: Maybe Text
      -- ^ The severity of the event. Values: ERROR, INFO.
    } deriving (Eq, Show, Generic)

instance ToQuery EventInfoMap

instance FromXML EventInfoMap where
    fromXMLOptions = xmlOptions

instance ToXML EventInfoMap where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for EventCategoriesMap
data EventCategoriesMap = EventCategoriesMap
    { ecmEvents :: [EventInfoMap]
      -- ^ The events in the event category.
    , ecmSourceType :: Maybe Text
      -- ^ The Amazon Redshift source type, such as cluster or cluster-snapshot, that
      -- the returned categories belong to.
    } deriving (Eq, Show, Generic)

instance ToQuery EventCategoriesMap

instance FromXML EventCategoriesMap where
    fromXMLOptions = xmlOptions

instance ToXML EventCategoriesMap where
    toXMLOptions = xmlOptions

-- | Describes an event.
data Event = Event
    { fDate :: Maybe UTCTime
      -- ^ The date and time of the event.
    , fEventCategories :: [Text]
      -- ^ A list of the event categories.
    , fEventId :: Maybe Text
      -- ^ The identifier of the event.
    , fMessage :: Maybe Text
      -- ^ The text of this event.
    , fSeverity :: Maybe Text
      -- ^ The severity of the event. Values: ERROR, INFO.
    , fSourceIdentifier :: Maybe Text
      -- ^ The identifier for the source of the event.
    , fSourceType :: Maybe SourceType
      -- ^ The source type for this event.
    } deriving (Eq, Show, Generic)

instance ToQuery Event

instance FromXML Event where
    fromXMLOptions = xmlOptions

instance ToXML Event where
    toXMLOptions = xmlOptions

-- | The connection endpoint.
data Endpoint = Endpoint
    { eAddress :: Maybe Text
      -- ^ The DNS address of the Cluster.
    , ePort :: Maybe Int
      -- ^ The port that the database engine is listening on.
    } deriving (Eq, Show, Generic)

instance ToQuery Endpoint

instance FromXML Endpoint where
    fromXMLOptions = xmlOptions

instance ToXML Endpoint where
    toXMLOptions = xmlOptions

-- | Describes the status of the elastic IP (EIP) address.
data ElasticIpStatus = ElasticIpStatus
    { eisElasticIp :: Maybe Text
      -- ^ The elastic IP (EIP) address for the cluster.
    , eisStatus :: Maybe Text
      -- ^ Describes the status of the elastic IP (EIP) address.
    } deriving (Eq, Show, Generic)

instance ToQuery ElasticIpStatus

instance FromXML ElasticIpStatus where
    fromXMLOptions = xmlOptions

instance ToXML ElasticIpStatus where
    toXMLOptions = xmlOptions

-- | Describes an Amazon EC2 security group.
data EC2SecurityGroup = EC2SecurityGroup
    { ec2sgEC2SecurityGroupName :: Maybe Text
      -- ^ The name of the EC2 Security Group.
    , ec2sgEC2SecurityGroupOwnerId :: Maybe Text
      -- ^ The AWS ID of the owner of the EC2 security group specified in the
      -- EC2SecurityGroupName field.
    , ec2sgStatus :: Maybe Text
      -- ^ The status of the EC2 security group.
    } deriving (Eq, Show, Generic)

instance ToQuery EC2SecurityGroup

instance FromXML EC2SecurityGroup where
    fromXMLOptions = xmlOptions

instance ToXML EC2SecurityGroup where
    toXMLOptions = xmlOptions

-- | Describes the default cluster parameters for a parameter group family.
data DefaultClusterParameters = DefaultClusterParameters
    { dcpMarker :: Maybe Text
      -- ^ An identifier to allow retrieval of paginated results.
    , dcpParameterGroupFamily :: Maybe Text
      -- ^ The name of the cluster parameter group family to which the engine default
      -- parameters apply.
    , dcpParameters :: [Parameter]
      -- ^ The list of cluster default parameters.
    } deriving (Eq, Show, Generic)

instance ToQuery DefaultClusterParameters

instance FromXML DefaultClusterParameters where
    fromXMLOptions = xmlOptions

instance ToXML DefaultClusterParameters where
    toXMLOptions = xmlOptions

-- | Describes a cluster version, including the parameter group family and
-- description of the version.
data ClusterVersion = ClusterVersion
    { cvClusterParameterGroupFamily :: Maybe Text
      -- ^ The name of the cluster parameter group family for the cluster.
    , cvClusterVersion :: Maybe Text
      -- ^ The version number used by the cluster.
    , cvDescription :: Maybe Text
      -- ^ The description of the cluster version.
    } deriving (Eq, Show, Generic)

instance ToQuery ClusterVersion

instance FromXML ClusterVersion where
    fromXMLOptions = xmlOptions

instance ToXML ClusterVersion where
    toXMLOptions = xmlOptions

-- | Describes a subnet group.
data ClusterSubnetGroup = ClusterSubnetGroup
    { cshClusterSubnetGroupName :: Maybe Text
      -- ^ The name of the cluster subnet group.
    , cshDescription :: Maybe Text
      -- ^ The description of the cluster subnet group.
    , cshSubnetGroupStatus :: Maybe Text
      -- ^ The status of the cluster subnet group. Possible values are Complete,
      -- Incomplete and Invalid.
    , cshSubnets :: [Subnet]
      -- ^ A list of the VPC Subnet elements.
    , cshVpcId :: Maybe Text
      -- ^ The VPC ID of the cluster subnet group.
    } deriving (Eq, Show, Generic)

instance ToQuery ClusterSubnetGroup

instance FromXML ClusterSubnetGroup where
    fromXMLOptions = xmlOptions

instance ToXML ClusterSubnetGroup where
    toXMLOptions = xmlOptions

-- | Returns the destination region and retention period that are configured for
-- cross-region snapshot copy.
data ClusterSnapshotCopyStatus = ClusterSnapshotCopyStatus
    { cscsDestinationRegion :: Maybe Text
      -- ^ The destination region that snapshots are automatically copied to when
      -- cross-region snapshot copy is enabled.
    , cscsRetentionPeriod :: Maybe Integer
      -- ^ The number of days that automated snapshots are retained in the destination
      -- region after they are copied from a source region.
    } deriving (Eq, Show, Generic)

instance ToQuery ClusterSnapshotCopyStatus

instance FromXML ClusterSnapshotCopyStatus where
    fromXMLOptions = xmlOptions

instance ToXML ClusterSnapshotCopyStatus where
    toXMLOptions = xmlOptions

-- | Describes a security group.
data ClusterSecurityGroupMembership = ClusterSecurityGroupMembership
    { csgmClusterSecurityGroupName :: Maybe Text
      -- ^ The name of the cluster security group.
    , csgmStatus :: Maybe Text
      -- ^ The status of the cluster security group.
    } deriving (Eq, Show, Generic)

instance ToQuery ClusterSecurityGroupMembership

instance FromXML ClusterSecurityGroupMembership where
    fromXMLOptions = xmlOptions

instance ToXML ClusterSecurityGroupMembership where
    toXMLOptions = xmlOptions

-- | Describes a security group.
data ClusterSecurityGroup = ClusterSecurityGroup
    { csgClusterSecurityGroupName :: Maybe Text
      -- ^ The name of the cluster security group to which the operation was applied.
    , csgDescription :: Maybe Text
      -- ^ A description of the security group.
    , csgEC2SecurityGroups :: [EC2SecurityGroup]
      -- ^ A list of EC2 security groups that are permitted to access clusters
      -- associated with this cluster security group.
    , csgIPRanges :: [IPRange]
      -- ^ A list of IP ranges (CIDR blocks) that are permitted to access clusters
      -- associated with this cluster security group.
    } deriving (Eq, Show, Generic)

instance ToQuery ClusterSecurityGroup

instance FromXML ClusterSecurityGroup where
    fromXMLOptions = xmlOptions

instance ToXML ClusterSecurityGroup where
    toXMLOptions = xmlOptions

-- | Describes the status of a parameter group.
data ClusterParameterGroupStatus = ClusterParameterGroupStatus
    { cpgsParameterApplyStatus :: Maybe Text
      -- ^ The status of parameter updates.
    , cpgsParameterGroupName :: Maybe Text
      -- ^ The name of the cluster parameter group.
    } deriving (Eq, Show, Generic)

instance ToQuery ClusterParameterGroupStatus

instance FromXML ClusterParameterGroupStatus where
    fromXMLOptions = xmlOptions

instance ToXML ClusterParameterGroupStatus where
    toXMLOptions = xmlOptions

-- | Describes a parameter group.
data ClusterParameterGroup = ClusterParameterGroup
    { cpgDescription :: Maybe Text
      -- ^ The description of the parameter group.
    , cpgParameterGroupFamily :: Maybe Text
      -- ^ The name of the cluster parameter group family that this cluster parameter
      -- group is compatible with.
    , cpgParameterGroupName :: Maybe Text
      -- ^ The name of the cluster parameter group.
    } deriving (Eq, Show, Generic)

instance ToQuery ClusterParameterGroup

instance FromXML ClusterParameterGroup where
    fromXMLOptions = xmlOptions

instance ToXML ClusterParameterGroup where
    toXMLOptions = xmlOptions

-- | The identifier of a node in a cluster. --&gt;.
data ClusterNode = ClusterNode
    { cnNodeRole :: Maybe Text
      -- ^ Whether the node is a leader node or a compute node.
    , cnPrivateIPAddress :: Maybe Text
      -- ^ The private IP address of a node within a cluster.
    , cnPublicIPAddress :: Maybe Text
      -- ^ The public IP address of a node within a cluster.
    } deriving (Eq, Show, Generic)

instance ToQuery ClusterNode

instance FromXML ClusterNode where
    fromXMLOptions = xmlOptions

instance ToXML ClusterNode where
    toXMLOptions = xmlOptions

-- | Describes a cluster.
data Cluster = Cluster
    { cAllowVersionUpgrade :: Maybe Bool
      -- ^ If true, version upgrades will be applied automatically to the cluster
      -- during the maintenance window.
    , cAutomatedSnapshotRetentionPeriod :: Maybe Int
      -- ^ The number of days that automatic cluster snapshots are retained.
    , cAvailabilityZone :: Maybe Text
      -- ^ The name of the Availability Zone in which the cluster is located.
    , cClusterCreateTime :: Maybe UTCTime
      -- ^ The date and time that the cluster was created.
    , cClusterIdentifier :: Maybe Text
      -- ^ The unique identifier of the cluster.
    , cClusterNodes :: [ClusterNode]
      -- ^ The nodes in a cluster.
    , cClusterParameterGroups :: [ClusterParameterGroupStatus]
      -- ^ The list of cluster parameter groups that are associated with this cluster.
    , cClusterPublicKey :: Maybe Text
      -- ^ The public key for the cluster.
    , cClusterSecurityGroups :: [ClusterSecurityGroupMembership]
      -- ^ A list of cluster security group that are associated with the cluster. Each
      -- security group is represented by an element that contains
      -- ClusterSecurityGroup.Name and ClusterSecurityGroup.Status subelements.
      -- Cluster security groups are used when the cluster is not created in a VPC.
      -- Clusters that are created in a VPC use VPC security groups, which are
      -- listed by the VpcSecurityGroups parameter.
    , cClusterSnapshotCopyStatus :: Maybe ClusterSnapshotCopyStatus
      -- ^ Returns the destination region and retention period that are configured for
      -- cross-region snapshot copy.
    , cClusterStatus :: Maybe Text
      -- ^ The current state of this cluster. Possible values include available,
      -- creating, deleting, rebooting, and resizing.
    , cClusterSubnetGroupName :: Maybe Text
      -- ^ The name of the subnet group that is associated with the cluster. This
      -- parameter is valid only when the cluster is in a VPC.
    , cClusterVersion :: Maybe Text
      -- ^ The version ID of the Amazon Redshift engine that is running on the
      -- cluster.
    , cDBName :: Maybe Text
      -- ^ The name of the initial database that was created when the cluster was
      -- created. This same name is returned for the life of the cluster. If an
      -- initial database was not specified, a database named "dev" was created by
      -- default.
    , cElasticIpStatus :: Maybe ElasticIpStatus
      -- ^ Describes the status of the elastic IP (EIP) address.
    , cEncrypted :: Maybe Bool
      -- ^ If true, data in cluster is encrypted at rest.
    , cEndpoint :: Maybe Endpoint
      -- ^ The connection endpoint.
    , cHsmStatus :: Maybe HsmStatus
      -- ^ Reports whether the Amazon Redshift cluster has finished applying any HSM
      -- settings changes specified in a modify cluster command. Values: active,
      -- applying.
    , cMasterUsername :: Maybe Text
      -- ^ The master user name for the cluster. This name is used to connect to the
      -- database that is specified in DBName.
    , cModifyStatus :: Maybe Text
      -- ^ The status of a modify operation, if any, initiated for the cluster.
    , cNodeType :: Maybe Text
      -- ^ The node type for the nodes in the cluster.
    , cNumberOfNodes :: Maybe Int
      -- ^ The number of compute nodes in the cluster.
    , cPendingModifiedValues :: Maybe PendingModifiedValues
      -- ^ If present, changes to the cluster are pending. Specific pending changes
      -- are identified by subelements.
    , cPreferredMaintenanceWindow :: Maybe Text
      -- ^ The weekly time range (in UTC) during which system maintenance can occur.
    , cPubliclyAccessible :: Maybe Bool
      -- ^ If true, the cluster can be accessed from a public network.
    , cRestoreStatus :: Maybe RestoreStatus
      -- ^ Describes the status of a cluster restore action. Returns null if the
      -- cluster was not created by restoring a snapshot.
    , cVpcId :: Maybe Text
      -- ^ The identifier of the VPC the cluster is in, if the cluster is in a VPC.
    , cVpcSecurityGroups :: [VpcSecurityGroupMembership]
      -- ^ A list of Virtual Private Cloud (VPC) security groups that are associated
      -- with the cluster. This parameter is returned only if the cluster is in a
      -- VPC.
    } deriving (Eq, Show, Generic)

instance ToQuery Cluster

instance FromXML Cluster where
    fromXMLOptions = xmlOptions

instance ToXML Cluster where
    toXMLOptions = xmlOptions

-- | Describes an availability zone.
newtype AvailabilityZone = AvailabilityZone
    { azName :: Maybe Text
      -- ^ The name of the availability zone.
    } deriving (Eq, Show, Generic)

instance ToQuery AvailabilityZone

instance FromXML AvailabilityZone where
    fromXMLOptions = xmlOptions

instance ToXML AvailabilityZone where
    toXMLOptions = xmlOptions

-- | Describes an AWS customer account authorized to restore a snapshot.
newtype AccountWithRestoreAccess = AccountWithRestoreAccess
    { awraAccountId :: Maybe Text
      -- ^ The identifier of an AWS customer account authorized to restore a snapshot.
    } deriving (Eq, Show, Generic)

instance ToQuery AccountWithRestoreAccess

instance FromXML AccountWithRestoreAccess where
    fromXMLOptions = xmlOptions

instance ToXML AccountWithRestoreAccess where
    toXMLOptions = xmlOptions

-- | The event source to retrieve events for. If no value is specified, all
-- events are returned. Constraints: If SourceType is supplied,
-- SourceIdentifier must also be provided. Specify cluster when
-- SourceIdentifier is a cluster identifier. Specify cluster-security-group
-- when SourceIdentifier is a cluster security group name. Specify
-- cluster-parameter-group when SourceIdentifier is a cluster parameter group
-- name. Specify cluster-snapshot when SourceIdentifier is a cluster snapshot
-- identifier.
data SourceType
    = SourceTypeCluster
    | SourceTypeClusterParameterGroup
    | SourceTypeClusterSecurityGroup
    | SourceTypeClusterSnapshot
      deriving (Eq, Ord, Generic)

instance Hashable SourceType

instance FromText SourceType where
    fromText "cluster" = Right SourceTypeCluster
    fromText "cluster-parameter-group" = Right SourceTypeClusterParameterGroup
    fromText "cluster-security-group" = Right SourceTypeClusterSecurityGroup
    fromText "cluster-snapshot" = Right SourceTypeClusterSnapshot
    fromText e = fromTextFail $ "Unrecognised SourceType: " <> e

instance Read SourceType where
    readsPrec _ = fromTextRead

instance ToText SourceType where
    toText SourceTypeCluster = "cluster"
    toText SourceTypeClusterParameterGroup = "cluster-parameter-group"
    toText SourceTypeClusterSecurityGroup = "cluster-security-group"
    toText SourceTypeClusterSnapshot = "cluster-snapshot"

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
