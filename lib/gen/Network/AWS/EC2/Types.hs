{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.EC2.Types where

import Network.AWS.Core
import Network.AWS.EC2.Service

-- | FIXME: Type documentation for VpnStaticRoute
data VpnStaticRoute = VpnStaticRoute
    { vsrDestinationCidrBlock :: Maybe Text
    , vsrSource :: Maybe VpnStaticRouteSource
    , vsrState :: Maybe VpnState
    } deriving (Eq, Show, Generic)

instance ToQuery VpnStaticRoute

instance FromXML VpnStaticRoute where
    fromXMLOptions = xmlOptions

instance ToXML VpnStaticRoute where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for VpnGateway
data VpnGateway = VpnGateway
    { vgAvailabilityZone :: Maybe Text
      -- ^ Specifies the Availability Zone where the VPN gateway was created.
    , vgState :: Maybe VpnState
      -- ^ Describes the current state of the VPN gateway. Valid values are pending,
      -- available, deleting, and deleted.
    , vgTagSet :: [Tag]
      -- ^ A list of tags for the VpnGateway.
    , vgType :: Maybe GatewayType
      -- ^ Specifies the type of VPN connection the VPN gateway supports.
    , vgAttachments :: [VpcAttachment]
      -- ^ Contains information about the VPCs attached to the VPN gateway.
    , vgVpnGatewayId :: Maybe Text
      -- ^ Specifies the ID of the VPN gateway.
    } deriving (Eq, Show, Generic)

instance ToQuery VpnGateway

instance FromXML VpnGateway where
    fromXMLOptions = xmlOptions

instance ToXML VpnGateway where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for VpnConnectionOptionsSpecification
newtype VpnConnectionOptionsSpecification = VpnConnectionOptionsSpecification
    { vcosStaticRoutesOnly :: Maybe Bool
      -- ^ FIXME: Type documentation for Bool
    } deriving (Eq, Show, Generic)

instance ToQuery VpnConnectionOptionsSpecification

instance FromXML VpnConnectionOptionsSpecification where
    fromXMLOptions = xmlOptions

instance ToXML VpnConnectionOptionsSpecification where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for VpnConnectionOptions
newtype VpnConnectionOptions = VpnConnectionOptions
    { vcoStaticRoutesOnly :: Maybe Bool
      -- ^ FIXME: Type documentation for Bool
    } deriving (Eq, Show, Generic)

instance ToQuery VpnConnectionOptions

instance FromXML VpnConnectionOptions where
    fromXMLOptions = xmlOptions

instance ToXML VpnConnectionOptions where
    toXMLOptions = xmlOptions

-- | The VpnConnection data type.
data VpnConnection = VpnConnection
    { vcCustomerGatewayConfiguration :: Maybe Text
      -- ^ Contains configuration information in the native XML format for the VPN
      -- connection's customer gateway. This element is always present in the
      -- CreateVpnConnection response; however, it's present in the
      -- DescribeVpnConnections response only if the VPN connection is in the
      -- pending or available state.
    , vcCustomerGatewayId :: Maybe Text
      -- ^ Specifies ID of the customer gateway at the end of the VPN connection.
    , vcOptions :: Maybe VpnConnectionOptions
    , vcRoutes :: [VpnStaticRoute]
    , vcState :: Maybe VpnState
      -- ^ Describes the current state of the VPN connection. Valid values are
      -- pending, available, deleting, and deleted.
    , vcTagSet :: [Tag]
      -- ^ A list of tags for the VpnConnection.
    , vcType :: Maybe GatewayType
      -- ^ Specifies the type of VPN connection.
    , vcVgwTelemetry :: [VgwTelemetry]
    , vcVpnConnectionId :: Maybe Text
      -- ^ Specifies the ID of the VPN gateway at the VPC end of the VPN connection.
    , vcVpnGatewayId :: Maybe Text
      -- ^ Specfies the ID of the VPN gateway at the VPC end of the VPN connection.
    } deriving (Eq, Show, Generic)

instance ToQuery VpnConnection

instance FromXML VpnConnection where
    fromXMLOptions = xmlOptions

instance ToXML VpnConnection where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for VpcAttachment
data VpcAttachment = VpcAttachment
    { vadState :: Maybe AttachmentStatus
    , vadVpcId :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery VpcAttachment

instance FromXML VpcAttachment where
    fromXMLOptions = xmlOptions

instance ToXML VpcAttachment where
    toXMLOptions = xmlOptions

-- | The Vpc data type.
data Vpc = Vpc
    { veCidrBlock :: Maybe Text
      -- ^ Specifies the CIDR block the VPC covers.
    , veDhcpOptionsId :: Maybe Text
      -- ^ Specifies the ID of the set of DHCP options associated with the VPC.
      -- Contains a value of default if the default options are associated with the
      -- VPC.
    , veInstanceTenancy :: Maybe Tenancy
      -- ^ The allowed tenancy of instances launched into the VPC.
    , veIsDefault :: Maybe Bool
    , veState :: Maybe VpcState
      -- ^ Describes the current state of the VPC. The state of the subnet may be
      -- either pending or available.
    , veTagSet :: [Tag]
      -- ^ A list of tags for the VPC.
    , veVpcId :: Maybe Text
      -- ^ Specifies the ID of the VPC.
    } deriving (Eq, Show, Generic)

instance ToQuery Vpc

instance FromXML Vpc where
    fromXMLOptions = xmlOptions

instance ToXML Vpc where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for VolumeStatusItem
data VolumeStatusItem = VolumeStatusItem
    { vsidActionsSet :: [VolumeStatusAction]
    , vsidAvailabilityZone :: Maybe Text
    , vsidEventsSet :: [VolumeStatusEvent]
    , vsidVolumeId :: Maybe Text
    , vsidVolumeStatus :: Maybe VolumeStatusInfo
    } deriving (Eq, Show, Generic)

instance ToQuery VolumeStatusItem

instance FromXML VolumeStatusItem where
    fromXMLOptions = xmlOptions

instance ToXML VolumeStatusItem where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for VolumeStatusInfo
data VolumeStatusInfo = VolumeStatusInfo
    { vsiDetails :: [VolumeStatusDetails]
    , vsiStatus :: Maybe VolumeStatusInfoStatus
    } deriving (Eq, Show, Generic)

instance ToQuery VolumeStatusInfo

instance FromXML VolumeStatusInfo where
    fromXMLOptions = xmlOptions

instance ToXML VolumeStatusInfo where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for VolumeStatusEvent
data VolumeStatusEvent = VolumeStatusEvent
    { vseDescription :: Maybe Text
    , vseEventId :: Maybe Text
    , vseEventType :: Maybe Text
    , vseNotAfter :: Maybe UTCTime
    , vseNotBefore :: Maybe UTCTime
    } deriving (Eq, Show, Generic)

instance ToQuery VolumeStatusEvent

instance FromXML VolumeStatusEvent where
    fromXMLOptions = xmlOptions

instance ToXML VolumeStatusEvent where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for VolumeStatusDetails
data VolumeStatusDetails = VolumeStatusDetails
    { vsdName :: Maybe VolumeStatusName
    , vsdStatus :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery VolumeStatusDetails

instance FromXML VolumeStatusDetails where
    fromXMLOptions = xmlOptions

instance ToXML VolumeStatusDetails where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for VolumeStatusAction
data VolumeStatusAction = VolumeStatusAction
    { vsaCode :: Maybe Text
    , vsaDescription :: Maybe Text
    , vsaEventId :: Maybe Text
    , vsaEventType :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery VolumeStatusAction

instance FromXML VolumeStatusAction where
    fromXMLOptions = xmlOptions

instance ToXML VolumeStatusAction where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for VolumeDetail
newtype VolumeDetail = VolumeDetail
    { vdSize :: Integer
      -- ^ FIXME: Type documentation for Integer
    } deriving (Eq, Show, Generic)

instance ToQuery VolumeDetail

instance FromXML VolumeDetail where
    fromXMLOptions = xmlOptions

instance ToXML VolumeDetail where
    toXMLOptions = xmlOptions

-- | Specifies the details of a how an EC2 EBS volume is attached to an
-- instance.
data VolumeAttachment = VolumeAttachment
    { vaAttachTime :: Maybe UTCTime
      -- ^ Timestamp when this attachment initiated.
    , vaDeleteOnTermination :: Maybe Bool
      -- ^ ` Whether this volume will be deleted or not when the associated instance
      -- is terminated.
    , vaDevice :: Maybe Text
      -- ^ How the device is exposed to the instance (e.g., /dev/sdh).
    , vaInstanceId :: Maybe Text
    , vaStatus :: Maybe VolumeAttachmentState
    , vaVolumeId :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery VolumeAttachment

instance FromXML VolumeAttachment where
    fromXMLOptions = xmlOptions

instance ToXML VolumeAttachment where
    toXMLOptions = xmlOptions

-- | Represents an Amazon Elastic Block Storage (EBS) volume.
data Volume = Volume
    { vAttachmentSet :: [VolumeAttachment]
      -- ^ Information on what this volume is attached to.
    , vAvailabilityZone :: Maybe Text
      -- ^ Availability zone in which this volume was created.
    , vCreateTime :: Maybe UTCTime
      -- ^ Timestamp when volume creation was initiated.
    , vIops :: Maybe Int
    , vSize :: Maybe Int
      -- ^ The size of this volume, in gigabytes.
    , vSnapshotId :: Maybe Text
      -- ^ Optional snapshot from which this volume was created.
    , vStatus :: Maybe VolumeState
      -- ^ State of this volume (e.g., creating, available).
    , vTagSet :: [Tag]
      -- ^ A list of tags for the Volume.
    , vVolumeId :: Maybe Text
      -- ^ The unique ID of this volume.
    , vVolumeType :: Maybe VolumeType
    } deriving (Eq, Show, Generic)

instance ToQuery Volume

instance FromXML Volume where
    fromXMLOptions = xmlOptions

instance ToXML Volume where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for VgwTelemetry
data VgwTelemetry = VgwTelemetry
    { vtAcceptedRouteCount :: Maybe Int
    , vtLastStatusChange :: Maybe UTCTime
    , vtOutsideIpAddress :: Maybe Text
    , vtStatus :: Maybe TelemetryStatus
    , vtStatusMessage :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery VgwTelemetry

instance FromXML VgwTelemetry where
    fromXMLOptions = xmlOptions

instance ToXML VgwTelemetry where
    toXMLOptions = xmlOptions

-- | An AWS user ID identifying an AWS account, and the name of a security group
-- within that account.
data UserIdGroupPair = UserIdGroupPair
    { uigpGroupId :: Maybe Text
      -- ^ ID of the security group in the specified AWS account. Cannot be used when
      -- specifying a CIDR IP address range.
    , uigpGroupName :: Maybe Text
      -- ^ Name of the security group in the specified AWS account. Cannot be used
      -- when specifying a CIDR IP address range.
    , uigpUserId :: Maybe Text
      -- ^ The AWS user ID of an account.
    } deriving (Eq, Show, Generic)

instance ToQuery UserIdGroupPair

instance FromXML UserIdGroupPair where
    fromXMLOptions = xmlOptions

instance ToXML UserIdGroupPair where
    toXMLOptions = xmlOptions

-- | Provides information about an Amazon EC2 resource Tag.
data TagDescription = TagDescription
    { tdKey :: Maybe Text
      -- ^ The tag's key.
    , tdResourceId :: Maybe Text
      -- ^ The resource ID for the tag.
    , tdResourceType :: Maybe ResourceType
      -- ^ The type of resource identified by the associated resource ID (ex:
      -- instance, AMI, EBS volume, etc).
    , tdValue :: Maybe Text
      -- ^ The tag's value.
    } deriving (Eq, Show, Generic)

instance ToQuery TagDescription

instance FromXML TagDescription where
    fromXMLOptions = xmlOptions

instance ToXML TagDescription where
    toXMLOptions = xmlOptions

-- | Represents metadata to associate with Amazon EC2 resources. Each tag
-- consists of a user-defined key and value. Use tags to categorize EC2
-- resources, such as by purpose, owner, or environment.
data Tag = Tag
    { tKey :: Maybe Text
      -- ^ The tag's key.
    , tValue :: Maybe Text
      -- ^ The tag's value.
    } deriving (Eq, Show, Generic)

instance ToQuery Tag

instance FromXML Tag where
    fromXMLOptions = xmlOptions

instance ToXML Tag where
    toXMLOptions = xmlOptions

-- | The Subnet data type.
data Subnet = Subnet
    { seAvailabilityZone :: Maybe Text
      -- ^ Specifies the Availability Zone the subnet is in.
    , seAvailableIpAddressCount :: Maybe Int
      -- ^ Specifies the number of unused IP addresses in the subnet. The IP addresses
      -- for any stopped instances are considered unavailable.
    , seCidrBlock :: Maybe Text
      -- ^ Specifies the CIDR block assigned to the subnet.
    , seDefaultForAz :: Maybe Bool
    , seMapPublicIpOnLaunch :: Maybe Bool
    , seState :: Maybe SubnetState
      -- ^ Describes the current state of the subnet. The state of the subnet may be
      -- either pending or available.
    , seSubnetId :: Maybe Text
      -- ^ Specifies the ID of the subnet.
    , seTagSet :: [Tag]
      -- ^ A list of tags for the Subnet.
    , seVpcId :: Maybe Text
      -- ^ Contains the ID of the VPC the subnet is in.
    } deriving (Eq, Show, Generic)

instance ToQuery Subnet

instance FromXML Subnet where
    fromXMLOptions = xmlOptions

instance ToXML Subnet where
    toXMLOptions = xmlOptions

-- | Amazon S3 storage locations.
newtype Storage = Storage
    { sdS3 :: Maybe S3Storage
      -- ^ The details of S3 storage for bundling a Windows instance.
    } deriving (Eq, Show, Generic)

instance ToQuery Storage

instance FromXML Storage where
    fromXMLOptions = xmlOptions

instance ToXML Storage where
    toXMLOptions = xmlOptions

-- | The reason for the state change.
data StateReason = StateReason
    { srCode :: Maybe Text
      -- ^ Reason code for the state change.
    , srMessage :: Maybe Text
      -- ^ Descriptive message for the state change.
    } deriving (Eq, Show, Generic)

instance ToQuery StateReason

instance FromXML StateReason where
    fromXMLOptions = xmlOptions

instance ToXML StateReason where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for SpotPrice
data SpotPrice = SpotPrice
    { spdAvailabilityZone :: Maybe Text
    , spdInstanceType :: Maybe InstanceType
    , spdProductDescription :: Maybe RIProductDescription
    , spdSpotPrice :: Maybe Text
    , spdTimestamp :: Maybe UTCTime
    } deriving (Eq, Show, Generic)

instance ToQuery SpotPrice

instance FromXML SpotPrice where
    fromXMLOptions = xmlOptions

instance ToXML SpotPrice where
    toXMLOptions = xmlOptions

-- | Defines a placement item.
data SpotPlacement = SpotPlacement
    { spAvailabilityZone :: Maybe Text
      -- ^ The availability zone in which an Amazon EC2 instance runs.
    , spGroupName :: Maybe Text
      -- ^ The name of the PlacementGroup in which an Amazon EC2 instance runs.
      -- Placement groups are primarily used for launching High Performance
      -- Computing instances in the same group to ensure fast connection speeds.
    } deriving (Eq, Show, Generic)

instance ToQuery SpotPlacement

instance FromXML SpotPlacement where
    fromXMLOptions = xmlOptions

instance ToXML SpotPlacement where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for SpotInstanceStatus
data SpotInstanceStatus = SpotInstanceStatus
    { sisCode :: Maybe Text
    , sisMessage :: Maybe Text
    , sisUpdateTime :: Maybe UTCTime
    } deriving (Eq, Show, Generic)

instance ToQuery SpotInstanceStatus

instance FromXML SpotInstanceStatus where
    fromXMLOptions = xmlOptions

instance ToXML SpotInstanceStatus where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for SpotInstanceStateFault
data SpotInstanceStateFault = SpotInstanceStateFault
    { sisfCode :: Maybe Text
    , sisfMessage :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery SpotInstanceStateFault

instance FromXML SpotInstanceStateFault where
    fromXMLOptions = xmlOptions

instance ToXML SpotInstanceStateFault where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for SpotInstanceRequest
data SpotInstanceRequest = SpotInstanceRequest
    { sirdAvailabilityZoneGroup :: Maybe Text
    , sirdCreateTime :: Maybe UTCTime
    , sirdFault :: Maybe SpotInstanceStateFault
    , sirdInstanceId :: Maybe Text
    , sirdLaunchGroup :: Maybe Text
    , sirdLaunchSpecification :: Maybe LaunchSpecification
      -- ^ The LaunchSpecificationType data type.
    , sirdLaunchedAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone in which the bid is launched.
    , sirdProductDescription :: Maybe RIProductDescription
    , sirdSpotInstanceRequestId :: Maybe Text
    , sirdSpotPrice :: Maybe Text
    , sirdState :: Maybe SpotInstanceState
    , sirdStatus :: Maybe SpotInstanceStatus
    , sirdTagSet :: [Tag]
      -- ^ A list of tags for this spot instance request.
    , sirdType :: Maybe SpotInstanceType
    , sirdValidFrom :: Maybe UTCTime
    , sirdValidUntil :: Maybe UTCTime
    } deriving (Eq, Show, Generic)

instance ToQuery SpotInstanceRequest

instance FromXML SpotInstanceRequest where
    fromXMLOptions = xmlOptions

instance ToXML SpotInstanceRequest where
    toXMLOptions = xmlOptions

-- | The Spot Instance datafeed subscription.
data SpotDatafeedSubscription = SpotDatafeedSubscription
    { sdsBucket :: Maybe Text
      -- ^ Specifies the Amazon S3 bucket where the Spot Instance data feed is
      -- located.
    , sdsFault :: Maybe SpotInstanceStateFault
      -- ^ Specifies a fault code for the Spot Instance request, if present.
    , sdsOwnerId :: Maybe Text
      -- ^ Specifies the AWS account ID of the account.
    , sdsPrefix :: Maybe Text
      -- ^ Contains the prefix that is prepended to data feed files.
    , sdsState :: Maybe DatafeedSubscriptionState
      -- ^ Specifies the state of the Spot Instance request.
    } deriving (Eq, Show, Generic)

instance ToQuery SpotDatafeedSubscription

instance FromXML SpotDatafeedSubscription where
    fromXMLOptions = xmlOptions

instance ToXML SpotDatafeedSubscription where
    toXMLOptions = xmlOptions

-- | Represents a snapshot of an Amazon EC2 EBS volume.
data Snapshot = Snapshot
    { sDescription :: Maybe Text
      -- ^ Description of the snapshot.
    , sOwnerAlias :: Maybe Text
      -- ^ The AWS account alias (e.g., "amazon", "redhat", "self", etc.) or AWS
      -- account ID that owns the AMI.
    , sOwnerId :: Maybe Text
      -- ^ AWS Access Key ID of the user who owns the snapshot.
    , sProgress :: Maybe Text
      -- ^ The progress of the snapshot, in percentage.
    , sSnapshotId :: Maybe Text
      -- ^ The unique ID of this snapshot.
    , sStartTime :: Maybe UTCTime
      -- ^ Time stamp when the snapshot was initiated.
    , sStatus :: Maybe SnapshotState
      -- ^ Snapshot state (e.g., pending, completed, or error).
    , sTagSet :: [Tag]
      -- ^ A list of tags for the Snapshot.
    , sVolumeId :: Maybe Text
      -- ^ The ID of the volume from which this snapshot was created.
    , sVolumeSize :: Maybe Int
      -- ^ The size of the volume, in gigabytes.
    } deriving (Eq, Show, Generic)

instance ToQuery Snapshot

instance FromXML Snapshot where
    fromXMLOptions = xmlOptions

instance ToXML Snapshot where
    toXMLOptions = xmlOptions

-- | An Amazon EC2 security group, describing how EC2 instances in this group
-- can receive network traffic.
data SecurityGroup = SecurityGroup
    { sgGroupDescription :: Maybe Text
      -- ^ The description of this security group.
    , sgGroupId :: Maybe Text
    , sgGroupName :: Maybe Text
      -- ^ The name of this security group.
    , sgIpPermissions :: [IpPermission]
      -- ^ The permissions enabled for this security group.
    , sgIpPermissionsEgress :: [IpPermission]
    , sgOwnerId :: Maybe Text
      -- ^ The AWS Access Key ID of the owner of the security group.
    , sgTagSet :: [Tag]
    , sgVpcId :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery SecurityGroup

instance FromXML SecurityGroup where
    fromXMLOptions = xmlOptions

instance ToXML SecurityGroup where
    toXMLOptions = xmlOptions

-- | The details of S3 storage for bundling a Windows instance.
data S3Storage = S3Storage
    { s3sAWSAccessKeyId :: Maybe Text
      -- ^ The Access Key ID of the owner of the Amazon S3 bucket.
    , s3sBucket :: Maybe Text
      -- ^ The bucket in which to store the AMI. You can specify a bucket that you
      -- already own or a new bucket that Amazon EC2 creates on your behalf. If you
      -- specify a bucket that belongs to someone else, Amazon EC2 returns an error.
    , s3sPrefix :: Maybe Text
      -- ^ The prefix to use when storing the AMI in S3.
    , s3sUploadPolicy :: Maybe Text
      -- ^ A Base64-encoded Amazon S3 upload policy that gives Amazon EC2 permission
      -- to upload items into Amazon S3 on the user's behalf.
    , s3sUploadPolicySignature :: Maybe Text
      -- ^ The signature of the Base64 encoded JSON document.
    } deriving (Eq, Show, Generic)

instance ToQuery S3Storage

instance FromXML S3Storage where
    fromXMLOptions = xmlOptions

instance ToXML S3Storage where
    toXMLOptions = xmlOptions

-- | Enables monitoring for the instance.
newtype RunInstancesMonitoringEnabled = RunInstancesMonitoringEnabled
    { rimeEnabled :: Bool
      -- ^ FIXME: Type documentation for Bool
    } deriving (Eq, Show, Generic)

instance ToQuery RunInstancesMonitoringEnabled

instance FromXML RunInstancesMonitoringEnabled where
    fromXMLOptions = xmlOptions

instance ToXML RunInstancesMonitoringEnabled where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for RouteTableAssociation
data RouteTableAssociation = RouteTableAssociation
    { rtaMain :: Maybe Bool
    , rtaRouteTableAssociationId :: Maybe Text
    , rtaRouteTableId :: Maybe Text
    , rtaSubnetId :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery RouteTableAssociation

instance FromXML RouteTableAssociation where
    fromXMLOptions = xmlOptions

instance ToXML RouteTableAssociation where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for RouteTable
data RouteTable = RouteTable
    { rtAssociationSet :: [RouteTableAssociation]
    , rtPropagatingVgwSet :: [PropagatingVgw]
    , rtRouteTableId :: Maybe Text
    , rtRouteSet :: [Route]
    , rtTagSet :: [Tag]
    , rtVpcId :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery RouteTable

instance FromXML RouteTable where
    fromXMLOptions = xmlOptions

instance ToXML RouteTable where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for Route
data Route = Route
    { reDestinationCidrBlock :: Maybe Text
    , reGatewayId :: Maybe Text
    , reInstanceId :: Maybe Text
    , reInstanceOwnerId :: Maybe Text
    , reNetworkInterfaceId :: Maybe Text
    , reState :: Maybe RouteState
    } deriving (Eq, Show, Generic)

instance ToQuery Route

instance FromXML Route where
    fromXMLOptions = xmlOptions

instance ToXML Route where
    toXMLOptions = xmlOptions

-- | An active offer for Amazon EC2 Reserved Instances.
data ReservedInstancesOffering = ReservedInstancesOffering
    { rioAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone in which the Reserved Instances can be used.
    , rioCurrencyCode :: Maybe CurrencyCodeValues
      -- ^ The currency of the reserved instance. Specified using ISO 4217 standard
      -- (e.g., USD, JPY).
    , rioDuration :: Maybe Integer
      -- ^ The duration of the Reserved Instance, in seconds.
    , rioFixedPrice :: Maybe Double
      -- ^ The purchase price of the Reserved Instance.
    , rioInstanceTenancy :: Maybe Tenancy
      -- ^ The tenancy of the reserved instance (ex: default or dedicated).
    , rioInstanceType :: Maybe InstanceType
      -- ^ The instance type on which the Reserved Instances can be used.
    , rioMarketplace :: Maybe Bool
    , rioOfferingType :: Maybe OfferingTypeValues
      -- ^ The Reserved Instance offering type.
    , rioPricingDetailsSet :: [PricingDetail]
    , rioProductDescription :: Maybe RIProductDescription
      -- ^ The Reserved Instances description (ex: Windows or Unix/Linux).
    , rioRecurringCharges :: [RecurringCharge]
      -- ^ The recurring charge tag assigned to the resource.
    , rioReservedInstancesOfferingId :: Maybe Text
      -- ^ The unique ID of this Reserved Instances offering.
    , rioUsagePrice :: Maybe Double
      -- ^ The usage price of the Reserved Instance, per hour.
    } deriving (Eq, Show, Generic)

instance ToQuery ReservedInstancesOffering

instance FromXML ReservedInstancesOffering where
    fromXMLOptions = xmlOptions

instance ToXML ReservedInstancesOffering where
    toXMLOptions = xmlOptions

-- | The resulting information about the modified Reserved Instances.
data ReservedInstancesModificationResult = ReservedInstancesModificationResult
    { rimrReservedInstancesId :: Maybe Text
      -- ^ The ID for the Reserved Instances created as part of the modification
      -- request.
    , rimrTargetConfiguration :: Maybe ReservedInstancesConfiguration
      -- ^ The configuration settings for the modified Reserved Instances.
    } deriving (Eq, Show, Generic)

instance ToQuery ReservedInstancesModificationResult

instance FromXML ReservedInstancesModificationResult where
    fromXMLOptions = xmlOptions

instance ToXML ReservedInstancesModificationResult where
    toXMLOptions = xmlOptions

-- | Information about a specific modification request to your Reserved
-- Instances.
data ReservedInstancesModification = ReservedInstancesModification
    { rimClientToken :: Maybe Text
      -- ^ The idempotency token for the modification request.
    , rimCreateDate :: Maybe UTCTime
      -- ^ The time the modification request was created.
    , rimEffectiveDate :: Maybe UTCTime
      -- ^ The time the modification becomes effective.
    , rimModificationResultSet :: [ReservedInstancesModificationResult]
      -- ^ The resulting information about the modified Reserved Instances.
    , rimReservedInstancesSet :: [ReservedInstancesId]
      -- ^ The IDs of the Reserved Instances submitted for modification.
    , rimReservedInstancesModificationId :: Maybe Text
      -- ^ The unique ID for the submitted modification request.
    , rimStatus :: Maybe Text
      -- ^ The status of the modification request, which can be any of the following
      -- values: processing, fulfilled, failed.
    , rimStatusMessage :: Maybe Text
      -- ^ The reason for the status.
    , rimUpdateDate :: Maybe UTCTime
      -- ^ The time the modification request was last updated.
    } deriving (Eq, Show, Generic)

instance ToQuery ReservedInstancesModification

instance FromXML ReservedInstancesModification where
    fromXMLOptions = xmlOptions

instance ToXML ReservedInstancesModification where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for ReservedInstancesListing
data ReservedInstancesListing = ReservedInstancesListing
    { rilClientToken :: Maybe Text
    , rilCreateDate :: Maybe UTCTime
    , rilInstanceCounts :: [InstanceCount]
    , rilPriceSchedules :: [PriceSchedule]
    , rilReservedInstancesId :: Maybe Text
    , rilReservedInstancesListingId :: Maybe Text
    , rilStatus :: Maybe ListingStatus
    , rilStatusMessage :: Maybe Text
    , rilTagSet :: [Tag]
    , rilUpdateDate :: Maybe UTCTime
    } deriving (Eq, Show, Generic)

instance ToQuery ReservedInstancesListing

instance FromXML ReservedInstancesListing where
    fromXMLOptions = xmlOptions

instance ToXML ReservedInstancesListing where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for ReservedInstancesId
newtype ReservedInstancesId = ReservedInstancesId
    { riiReservedInstancesId :: Maybe Text
      -- ^ FIXME: Type documentation for Text
    } deriving (Eq, Show, Generic)

instance ToQuery ReservedInstancesId

instance FromXML ReservedInstancesId where
    fromXMLOptions = xmlOptions

instance ToXML ReservedInstancesId where
    toXMLOptions = xmlOptions

-- | The configuration settings for the modified Reserved Instances.
data ReservedInstancesConfiguration = ReservedInstancesConfiguration
    { ricAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone for the modified Reserved Instances.
    , ricInstanceCount :: Maybe Int
      -- ^ The number of modified Reserved Instances.
    , ricInstanceType :: Maybe InstanceType
      -- ^ The instance type for the modified Reserved Instances.
    , ricPlatform :: Maybe Text
      -- ^ The network platform of the modified Reserved Instances, which is either
      -- EC2-Classic or EC2-VPC.
    } deriving (Eq, Show, Generic)

instance ToQuery ReservedInstancesConfiguration

instance FromXML ReservedInstancesConfiguration where
    fromXMLOptions = xmlOptions

instance ToXML ReservedInstancesConfiguration where
    toXMLOptions = xmlOptions

-- | A group of Amazon EC2 Reserved Instances purchased by this account.
data ReservedInstances = ReservedInstances
    { rifAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone in which the Reserved Instances can be used.
    , rifCurrencyCode :: Maybe CurrencyCodeValues
      -- ^ The currency of the reserved instance. Specified using ISO 4217 standard
      -- (e.g., USD, JPY).
    , rifDuration :: Maybe Integer
      -- ^ The duration of the Reserved Instances, in seconds.
    , rifEnd :: Maybe UTCTime
    , rifFixedPrice :: Maybe Double
      -- ^ The purchase price of the Reserved Instances.
    , rifInstanceCount :: Maybe Int
      -- ^ The number of Reserved Instances purchased.
    , rifInstanceTenancy :: Maybe Tenancy
      -- ^ The tenancy of the reserved instance (ex: default or dedicated).
    , rifInstanceType :: Maybe InstanceType
      -- ^ The instance type on which the Reserved Instances can be used.
    , rifOfferingType :: Maybe OfferingTypeValues
      -- ^ The Reserved Instance offering type.
    , rifProductDescription :: Maybe RIProductDescription
      -- ^ The Reserved Instances product description (ex: Windows or Unix/Linux).
    , rifRecurringCharges :: [RecurringCharge]
      -- ^ The recurring charge tag assigned to the resource.
    , rifReservedInstancesId :: Maybe Text
      -- ^ The unique ID of the Reserved Instances purchase.
    , rifStart :: Maybe UTCTime
      -- ^ The date and time the Reserved Instances started.
    , rifState :: Maybe ReservedInstanceState
      -- ^ The state of the Reserved Instances purchase.
    , rifTagSet :: [Tag]
      -- ^ A list of tags for the ReservedInstances.
    , rifUsagePrice :: Maybe Double
      -- ^ The usage price of the Reserved Instances, per hour.
    } deriving (Eq, Show, Generic)

instance ToQuery ReservedInstances

instance FromXML ReservedInstances where
    fromXMLOptions = xmlOptions

instance ToXML ReservedInstances where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for ReservedInstanceLimitPrice
data ReservedInstanceLimitPrice = ReservedInstanceLimitPrice
    { rilpAmount :: Maybe Double
    , rilpCurrencyCode :: Maybe CurrencyCodeValues
    } deriving (Eq, Show, Generic)

instance ToQuery ReservedInstanceLimitPrice

instance FromXML ReservedInstanceLimitPrice where
    fromXMLOptions = xmlOptions

instance ToXML ReservedInstanceLimitPrice where
    toXMLOptions = xmlOptions

-- | An Amazon EC2 reservation of requested EC2 instances.
data Reservation = Reservation
    { rdGroupSet :: [GroupIdentifier]
      -- ^ The list of security groups requested for the instances in this
      -- reservation.
    , rdInstancesSet :: [Instance]
      -- ^ The list of Amazon EC2 instances included in this reservation.
    , rdOwnerId :: Maybe Text
      -- ^ The AWS Access Key ID of the user who owns the reservation.
    , rdRequesterId :: Maybe Text
      -- ^ The unique ID of the user who requested the instances in this reservation.
    , rdReservationId :: Maybe Text
      -- ^ The unique ID of this reservation.
    } deriving (Eq, Show, Generic)

instance ToQuery Reservation

instance FromXML Reservation where
    fromXMLOptions = xmlOptions

instance ToXML Reservation where
    toXMLOptions = xmlOptions

-- | Represents an Amazon EC2 region. EC2 regions are completely isolated from
-- each other.
data Region = Region
    { rRegionEndpoint :: Maybe Text
      -- ^ Region service endpoint.
    , rRegionName :: Maybe Text
      -- ^ Name of the region.
    } deriving (Eq, Show, Generic)

instance ToQuery Region

instance FromXML Region where
    fromXMLOptions = xmlOptions

instance ToXML Region where
    toXMLOptions = xmlOptions

-- | Represents a usage charge for Amazon EC2 resources that repeats on a
-- schedule.
data RecurringCharge = RecurringCharge
    { rcAmount :: Maybe Double
      -- ^ The amount of the recurring charge.
    , rcFrequency :: Maybe RecurringChargeFrequency
      -- ^ The frequency of the recurring charge.
    } deriving (Eq, Show, Generic)

instance ToQuery RecurringCharge

instance FromXML RecurringCharge where
    fromXMLOptions = xmlOptions

instance ToXML RecurringCharge where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for PropagatingVgw
newtype PropagatingVgw = PropagatingVgw
    { pvGatewayId :: Maybe Text
      -- ^ FIXME: Type documentation for Text
    } deriving (Eq, Show, Generic)

instance ToQuery PropagatingVgw

instance FromXML PropagatingVgw where
    fromXMLOptions = xmlOptions

instance ToXML PropagatingVgw where
    toXMLOptions = xmlOptions

-- | An AWS DevPay product code.
data ProductCode = ProductCode
    { pcProductCode :: Maybe Text
      -- ^ The unique ID of an AWS DevPay product code.
    , pcType :: Maybe ProductCodeValues
    } deriving (Eq, Show, Generic)

instance ToQuery ProductCode

instance FromXML ProductCode where
    fromXMLOptions = xmlOptions

instance ToXML ProductCode where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for PrivateIpAddressSpecification
data PrivateIpAddressSpecification = PrivateIpAddressSpecification
    { piasPrimary :: Maybe Bool
    , piasPrivateIpAddress :: !Text
    } deriving (Eq, Show, Generic)

instance ToQuery PrivateIpAddressSpecification

instance FromXML PrivateIpAddressSpecification where
    fromXMLOptions = xmlOptions

instance ToXML PrivateIpAddressSpecification where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for PricingDetail
data PricingDetail = PricingDetail
    { pdCount :: Maybe Int
    , pdPrice :: Maybe Double
    } deriving (Eq, Show, Generic)

instance ToQuery PricingDetail

instance FromXML PricingDetail where
    fromXMLOptions = xmlOptions

instance ToXML PricingDetail where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for PriceScheduleSpecification
data PriceScheduleSpecification = PriceScheduleSpecification
    { pssCurrencyCode :: Maybe CurrencyCodeValues
    , pssPrice :: Maybe Double
    , pssTerm :: Maybe Integer
    } deriving (Eq, Show, Generic)

instance ToQuery PriceScheduleSpecification

instance FromXML PriceScheduleSpecification where
    fromXMLOptions = xmlOptions

instance ToXML PriceScheduleSpecification where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for PriceSchedule
data PriceSchedule = PriceSchedule
    { psActive :: Maybe Bool
    , psCurrencyCode :: Maybe CurrencyCodeValues
    , psPrice :: Maybe Double
    , psTerm :: Maybe Integer
    } deriving (Eq, Show, Generic)

instance ToQuery PriceSchedule

instance FromXML PriceSchedule where
    fromXMLOptions = xmlOptions

instance ToXML PriceSchedule where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for PortRange
data PortRange = PortRange
    { prFrom :: Maybe Int
      -- ^ The first port in the range. Required if specifying tcp or udp for the
      -- protocol.
    , prTo :: Maybe Int
      -- ^ The last port in the range. Required if specifying tcp or udp for the
      -- protocol.
    } deriving (Eq, Show, Generic)

instance ToQuery PortRange

instance FromXML PortRange where
    fromXMLOptions = xmlOptions

instance ToXML PortRange where
    toXMLOptions = xmlOptions

-- | Represents a placement group into which multiple Amazon EC2 instances can
-- be launched. A placement group ensures that Amazon EC2 instances are
-- physically located close enough to support HPC features, such as higher IO
-- network connections between instances in the group.
data PlacementGroup = PlacementGroup
    { pgGroupName :: Maybe Text
      -- ^ The name of this PlacementGroup.
    , pgState :: Maybe PlacementGroupState
      -- ^ The state of this PlacementGroup.
    , pgStrategy :: Maybe PlacementStrategy
      -- ^ The strategy to use when allocating Amazon EC2 instances for the
      -- PlacementGroup.
    } deriving (Eq, Show, Generic)

instance ToQuery PlacementGroup

instance FromXML PlacementGroup where
    fromXMLOptions = xmlOptions

instance ToXML PlacementGroup where
    toXMLOptions = xmlOptions

-- | Describes where an Amazon EC2 instance is running within an Amazon EC2
-- region.
data Placement = Placement
    { pAvailabilityZone :: Maybe Text
      -- ^ The availability zone in which an Amazon EC2 instance runs.
    , pGroupName :: Maybe Text
      -- ^ The name of the PlacementGroup in which an Amazon EC2 instance runs.
      -- Placement groups are primarily used for launching High Performance
      -- Computing instances in the same group to ensure fast connection speeds.
    , pTenancy :: Maybe Tenancy
      -- ^ The allowed tenancy of instances launched into the VPC. A value of default
      -- means instances can be launched with any tenancy; a value of dedicated
      -- means all instances launched into the VPC will be launched as dedicated
      -- tenancy regardless of the tenancy assigned to the instance at launch.
    } deriving (Eq, Show, Generic)

instance ToQuery Placement

instance FromXML Placement where
    fromXMLOptions = xmlOptions

instance ToXML Placement where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for NetworkInterfacePrivateIpAddress
data NetworkInterfacePrivateIpAddress = NetworkInterfacePrivateIpAddress
    { nipiaAssociation :: Maybe NetworkInterfaceAssociation
    , nipiaPrimary :: Maybe Bool
    , nipiaPrivateDnsName :: Maybe Text
    , nipiaPrivateIpAddress :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery NetworkInterfacePrivateIpAddress

instance FromXML NetworkInterfacePrivateIpAddress where
    fromXMLOptions = xmlOptions

instance ToXML NetworkInterfacePrivateIpAddress where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for NetworkInterfaceAttachmentChanges
data NetworkInterfaceAttachmentChanges = NetworkInterfaceAttachmentChanges
    { niacAttachmentId :: Maybe Text
    , niacDeleteOnTermination :: Maybe Bool
    } deriving (Eq, Show, Generic)

instance ToQuery NetworkInterfaceAttachmentChanges

instance FromXML NetworkInterfaceAttachmentChanges where
    fromXMLOptions = xmlOptions

instance ToXML NetworkInterfaceAttachmentChanges where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for NetworkInterfaceAttachment
data NetworkInterfaceAttachment = NetworkInterfaceAttachment
    { niadAttachTime :: Maybe UTCTime
    , niadAttachmentId :: Maybe Text
    , niadDeleteOnTermination :: Maybe Bool
    , niadDeviceIndex :: Maybe Int
    , niadInstanceId :: Maybe Text
    , niadInstanceOwnerId :: Maybe Text
    , niadStatus :: Maybe AttachmentStatus
    } deriving (Eq, Show, Generic)

instance ToQuery NetworkInterfaceAttachment

instance FromXML NetworkInterfaceAttachment where
    fromXMLOptions = xmlOptions

instance ToXML NetworkInterfaceAttachment where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for NetworkInterfaceAssociation
data NetworkInterfaceAssociation = NetworkInterfaceAssociation
    { niaAllocationId :: Maybe Text
    , niaAssociationId :: Maybe Text
    , niaIpOwnerId :: Maybe Text
    , niaPublicIp :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery NetworkInterfaceAssociation

instance FromXML NetworkInterfaceAssociation where
    fromXMLOptions = xmlOptions

instance ToXML NetworkInterfaceAssociation where
    toXMLOptions = xmlOptions

-- | Specifies the characteristics of a network interface.
data NetworkInterface = NetworkInterface
    { niAssociation :: Maybe NetworkInterfaceAssociation
    , niAttachment :: Maybe NetworkInterfaceAttachment
    , niAvailabilityZone :: Maybe Text
    , niDescription :: Maybe Text
    , niGroupSet :: [GroupIdentifier]
    , niMacAddress :: Maybe Text
    , niNetworkInterfaceId :: Maybe Text
    , niOwnerId :: Maybe Text
    , niPrivateDnsName :: Maybe Text
    , niPrivateIpAddress :: Maybe Text
    , niPrivateIpAddressesSet :: [NetworkInterfacePrivateIpAddress]
    , niRequesterId :: Maybe Text
    , niRequesterManaged :: Maybe Bool
    , niSourceDestCheck :: Maybe Bool
    , niStatus :: Maybe NetworkInterfaceStatus
    , niSubnetId :: Maybe Text
    , niTagSet :: [Tag]
    , niVpcId :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery NetworkInterface

instance FromXML NetworkInterface where
    fromXMLOptions = xmlOptions

instance ToXML NetworkInterface where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for NetworkAclEntry
data NetworkAclEntry = NetworkAclEntry
    { naeCidrBlock :: Maybe Text
    , naeEgress :: Maybe Bool
    , naeIcmpTypeCode :: Maybe IcmpTypeCode
    , naePortRange :: Maybe PortRange
    , naeProtocol :: Maybe Text
    , naeRuleAction :: Maybe RuleAction
    , naeRuleNumber :: Maybe Int
    } deriving (Eq, Show, Generic)

instance ToQuery NetworkAclEntry

instance FromXML NetworkAclEntry where
    fromXMLOptions = xmlOptions

instance ToXML NetworkAclEntry where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for NetworkAclAssociation
data NetworkAclAssociation = NetworkAclAssociation
    { naaNetworkAclAssociationId :: Maybe Text
    , naaNetworkAclId :: Maybe Text
    , naaSubnetId :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery NetworkAclAssociation

instance FromXML NetworkAclAssociation where
    fromXMLOptions = xmlOptions

instance ToXML NetworkAclAssociation where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for NetworkAcl
data NetworkAcl = NetworkAcl
    { naAssociationSet :: [NetworkAclAssociation]
    , naEntrySet :: [NetworkAclEntry]
    , naDefault :: Maybe Bool
    , naNetworkAclId :: Maybe Text
    , naTagSet :: [Tag]
    , naVpcId :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery NetworkAcl

instance FromXML NetworkAcl where
    fromXMLOptions = xmlOptions

instance ToXML NetworkAcl where
    toXMLOptions = xmlOptions

-- | Monitoring status for this instance.
newtype Monitoring = Monitoring
    { mState :: Maybe MonitoringState
      -- ^ The state of monitoring on an Amazon EC2 instance (ex: enabled, disabled).
    } deriving (Eq, Show, Generic)

instance ToQuery Monitoring

instance FromXML Monitoring where
    fromXMLOptions = xmlOptions

instance ToXML Monitoring where
    toXMLOptions = xmlOptions

-- | Represents the capacity that a license is able to support.
data LicenseCapacity = LicenseCapacity
    { lcCapacity :: Maybe Int
      -- ^ The number of licenses available.
    , lcEarliestAllowedDeactivationTime :: Maybe UTCTime
      -- ^ The earliest allowed time at which a license can be deactivated. Some
      -- licenses have time restrictions on when they can be activated and
      -- reactivated.
    , lcInstanceCapacity :: Maybe Int
      -- ^ The number of Amazon EC2 instances that can be supported with the license's
      -- capacity.
    , lcState :: Maybe Text
      -- ^ The state of this license capacity, indicating whether the license is
      -- actively being used or not.
    } deriving (Eq, Show, Generic)

instance ToQuery LicenseCapacity

instance FromXML LicenseCapacity where
    fromXMLOptions = xmlOptions

instance ToXML LicenseCapacity where
    toXMLOptions = xmlOptions

-- | A software license that can be associated with an Amazon EC2 instance when
-- launched (ex. a Microsoft Windows license).
data License = License
    { lCapacitySet :: [LicenseCapacity]
      -- ^ The capacities available for this license, indicating how many licenses are
      -- in use, how many are available, how many Amazon EC2 instances can be
      -- supported, etc.
    , lLicenseId :: Maybe Text
      -- ^ The unique ID identifying the license.
    , lPool :: Maybe Text
      -- ^ The name of the pool in which the license is kept.
    , lTagSet :: [Tag]
      -- ^ A list of tags for the License.
    , lType :: Maybe Text
      -- ^ The license type (ex. "Microsoft/Windows/Standard").
    } deriving (Eq, Show, Generic)

instance ToQuery License

instance FromXML License where
    fromXMLOptions = xmlOptions

instance ToXML License where
    toXMLOptions = xmlOptions

-- | Specifies additional launch instance information.
data LaunchSpecification = LaunchSpecification
    { lsAddressingType :: Maybe Text
      -- ^ Deprecated.
    , lsBlockDeviceMappings :: [BlockDeviceMapping]
      -- ^ Specifies how block devices are exposed to the instance. Each mapping is
      -- made up of a virtualName and a deviceName.
    , lsEbsOptimized :: Maybe Bool
    , lsIamInstanceProfile :: Maybe IamInstanceProfileSpecification
    , lsImageId :: Maybe Text
      -- ^ The AMI ID.
    , lsInstanceType :: Maybe InstanceType
      -- ^ Specifies the instance type.
    , lsKernelId :: Maybe Text
      -- ^ Specifies the ID of the kernel to select.
    , lsKeyName :: Maybe Text
      -- ^ The name of the key pair.
    , lsMonitoring :: Maybe Monitoring
    , lsNetworkInterfaces :: [InstanceNetworkInterfaceSpecification]
    , lsPlacement :: Maybe SpotPlacement
      -- ^ Defines a placement item.
    , lsRamdiskId :: Maybe Text
      -- ^ Specifies the ID of the RAM disk to select. Some kernels require additional
      -- drivers at launch. Check the kernel requirements for information on whether
      -- or not you need to specify a RAM disk and search for the kernel ID.
    , lsSecurityGroupIds :: [Text]
    , lsSecurityGroups :: [Text]
    , lsSubnetId :: Maybe Text
      -- ^ Specifies the Amazon VPC subnet ID within which to launch the instance(s)
      -- for Amazon Virtual Private Cloud.
    , lsUserData :: Maybe Text
      -- ^ Optional data, specific to a user's application, to provide in the launch
      -- request. All instances that collectively comprise the launch request have
      -- access to this data. User data is never returned through API responses.
    } deriving (Eq, Show, Generic)

instance ToQuery LaunchSpecification

instance FromXML LaunchSpecification where
    fromXMLOptions = xmlOptions

instance ToXML LaunchSpecification where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for LaunchPermissionModifications
data LaunchPermissionModifications = LaunchPermissionModifications
    { lpmAdd :: [LaunchPermission]
    , lpmRemove :: [LaunchPermission]
    } deriving (Eq, Show, Generic)

instance ToQuery LaunchPermissionModifications

instance FromXML LaunchPermissionModifications where
    fromXMLOptions = xmlOptions

instance ToXML LaunchPermissionModifications where
    toXMLOptions = xmlOptions

-- | Describes a permission to launch an Amazon Machine Image (AMI).
data LaunchPermission = LaunchPermission
    { lpGroup :: Maybe PermissionGroup
      -- ^ The AWS group of the user involved in this launch permission. Available
      -- groups: all.
    , lpUserId :: Maybe Text
      -- ^ The AWS user ID of the user involved in this launch permission.
    } deriving (Eq, Show, Generic)

instance ToQuery LaunchPermission

instance FromXML LaunchPermission where
    fromXMLOptions = xmlOptions

instance ToXML LaunchPermission where
    toXMLOptions = xmlOptions

-- | Describes an Amazon EC2 key pair. This is a summary of the key pair data,
-- and will not contain the actual private key material. The private key
-- material is only available when initially creating the key pair.
data KeyPairInfo = KeyPairInfo
    { kpiKeyFingerprint :: Maybe Text
      -- ^ The SHA-1 digest of the DER encoded private key.
    , kpiKeyName :: Maybe Text
      -- ^ The name of the key pair.
    } deriving (Eq, Show, Generic)

instance ToQuery KeyPairInfo

instance FromXML KeyPairInfo where
    fromXMLOptions = xmlOptions

instance ToXML KeyPairInfo where
    toXMLOptions = xmlOptions

-- | Contains a list of CIDR IP ranges.
newtype IpRange = IpRange
    { irCidrIp :: Maybe Text
      -- ^ The list of CIDR IP ranges.
    } deriving (Eq, Show, Generic)

instance ToQuery IpRange

instance FromXML IpRange where
    fromXMLOptions = xmlOptions

instance ToXML IpRange where
    toXMLOptions = xmlOptions

-- | An IP permission describing allowed incoming IP traffic to an Amazon EC2
-- security group.
data IpPermission = IpPermission
    { ipFromPort :: Maybe Int
      -- ^ Start of port range for the TCP and UDP protocols, or an ICMP type number.
      -- An ICMP type number of -1 indicates a wildcard (i.e., any ICMP type
      -- number).
    , ipIpProtocol :: Maybe Text
      -- ^ The IP protocol of this permission. Valid protocol values: tcp, udp, icmp.
    , ipIpRanges :: [IpRange]
      -- ^ The list of CIDR IP ranges included in this permission.
    , ipToPort :: Maybe Int
      -- ^ End of port range for the TCP and UDP protocols, or an ICMP code. An ICMP
      -- code of -1 indicates a wildcard (i.e., any ICMP code).
    , ipUserIdGroupPairs :: [UserIdGroupPair]
      -- ^ The list of AWS user IDs and groups included in this permission.
    } deriving (Eq, Show, Generic)

instance ToQuery IpPermission

instance FromXML IpPermission where
    fromXMLOptions = xmlOptions

instance ToXML IpPermission where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for InternetGatewayAttachment
data InternetGatewayAttachment = InternetGatewayAttachment
    { igaState :: Maybe AttachmentStatus
    , igaVpcId :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery InternetGatewayAttachment

instance FromXML InternetGatewayAttachment where
    fromXMLOptions = xmlOptions

instance ToXML InternetGatewayAttachment where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for InternetGateway
data InternetGateway = InternetGateway
    { igAttachmentSet :: [InternetGatewayAttachment]
    , igInternetGatewayId :: Maybe Text
    , igTagSet :: [Tag]
    } deriving (Eq, Show, Generic)

instance ToQuery InternetGateway

instance FromXML InternetGateway where
    fromXMLOptions = xmlOptions

instance ToXML InternetGateway where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for InstanceStatusSummary
data InstanceStatusSummary = InstanceStatusSummary
    { issDetails :: [InstanceStatusDetails]
    , issStatus :: Maybe SummaryStatus
    } deriving (Eq, Show, Generic)

instance ToQuery InstanceStatusSummary

instance FromXML InstanceStatusSummary where
    fromXMLOptions = xmlOptions

instance ToXML InstanceStatusSummary where
    toXMLOptions = xmlOptions

-- | Represents an event that affects the status of an Amazon EC2 instance.
data InstanceStatusEvent = InstanceStatusEvent
    { iseCode :: Maybe EventCode
      -- ^ The associated code of the event. Valid values: instance-reboot,
      -- system-reboot, instance-retirement.
    , iseDescription :: Maybe Text
      -- ^ A description of the event.
    , iseNotAfter :: Maybe UTCTime
      -- ^ The latest scheduled end time for the event.
    , iseNotBefore :: Maybe UTCTime
      -- ^ The earliest scheduled start time for the event.
    } deriving (Eq, Show, Generic)

instance ToQuery InstanceStatusEvent

instance FromXML InstanceStatusEvent where
    fromXMLOptions = xmlOptions

instance ToXML InstanceStatusEvent where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for InstanceStatusDetails
data InstanceStatusDetails = InstanceStatusDetails
    { isddImpairedSince :: Maybe UTCTime
    , isddName :: Maybe StatusName
    , isddStatus :: Maybe StatusType
    } deriving (Eq, Show, Generic)

instance ToQuery InstanceStatusDetails

instance FromXML InstanceStatusDetails where
    fromXMLOptions = xmlOptions

instance ToXML InstanceStatusDetails where
    toXMLOptions = xmlOptions

-- | Represents the status of an Amazon EC2 instance.
data InstanceStatus = InstanceStatus
    { isdAvailabilityZone :: Maybe Text
      -- ^ The Amazon EC2 instance's availability zone.
    , isdEventsSet :: [InstanceStatusEvent]
      -- ^ Events that affect the status of the associated Amazon EC2 instance.
    , isdInstanceId :: Maybe Text
      -- ^ The ID of the Amazon EC2 instance.
    , isdInstanceState :: Maybe InstanceState
      -- ^ Represents the state of an Amazon EC2 instance.
    , isdInstanceStatus :: Maybe InstanceStatusSummary
    , isdSystemStatus :: Maybe InstanceStatusSummary
    } deriving (Eq, Show, Generic)

instance ToQuery InstanceStatus

instance FromXML InstanceStatus where
    fromXMLOptions = xmlOptions

instance ToXML InstanceStatus where
    toXMLOptions = xmlOptions

-- | Represents a state change for a specific EC2 instance.
data InstanceStateChange = InstanceStateChange
    { iscCurrentState :: Maybe InstanceState
      -- ^ The current state of the specified instance.
    , iscInstanceId :: Maybe Text
      -- ^ The ID of the instance whose state changed.
    , iscPreviousState :: Maybe InstanceState
      -- ^ The previous state of the specified instance.
    } deriving (Eq, Show, Generic)

instance ToQuery InstanceStateChange

instance FromXML InstanceStateChange where
    fromXMLOptions = xmlOptions

instance ToXML InstanceStateChange where
    toXMLOptions = xmlOptions

-- | The current state of the specified instance.
data InstanceState = InstanceState
    { isCode :: Maybe Int
      -- ^ A 16-bit unsigned integer. The high byte is an opaque internal value and
      -- should be ignored. The low byte is set based on the state represented.
    , isName :: Maybe InstanceStateName
      -- ^ The current state of the instance.
    } deriving (Eq, Show, Generic)

instance ToQuery InstanceState

instance FromXML InstanceState where
    fromXMLOptions = xmlOptions

instance ToXML InstanceState where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for InstancePrivateIpAddress
data InstancePrivateIpAddress = InstancePrivateIpAddress
    { ipiaAssociation :: Maybe InstanceNetworkInterfaceAssociation
    , ipiaPrimary :: Maybe Bool
    , ipiaPrivateDnsName :: Maybe Text
    , ipiaPrivateIpAddress :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery InstancePrivateIpAddress

instance FromXML InstancePrivateIpAddress where
    fromXMLOptions = xmlOptions

instance ToXML InstancePrivateIpAddress where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for InstanceNetworkInterfaceSpecification
data InstanceNetworkInterfaceSpecification = InstanceNetworkInterfaceSpecification
    { inisAssociatePublicIpAddress :: Maybe Bool
      -- ^ Indicates whether to assign a public IP address to an instance in a VPC.
      -- The public IP address is associated with a specific network interface. If
      -- set to true, the following rules apply: Can only be associated with a
      -- single network interface with the device index of 0. You can't associate a
      -- public IP address with a second network interface, and you can't associate
      -- a public IP address if you are launching more than one network interface.
      -- Can only be associated with a new network interface, not an existing one.
      -- Default: If launching into a default subnet, the default value is true. If
      -- launching into a nondefault subnet, the default value is false.
    , inisDeleteOnTermination :: Maybe Bool
    , inisDescription :: Maybe Text
      -- ^ A description. Applies only when creating a network interface.
    , inisDeviceIndex :: Maybe Int
      -- ^ The device index. Applies to both attaching an existing network interface
      -- and when creating a network interface. Condition: If you are specifying a
      -- network interface in the request, you must provide the device index.
    , inisGroups :: [Text]
    , inisNetworkInterfaceId :: Maybe Text
      -- ^ An existing interface to attach to a single instance. Requires n=1
      -- instances.
    , inisPrivateIpAddress :: Maybe Text
      -- ^ The primary private IP address. Applies only when creating a network
      -- interface. Requires n=1 network interfaces in launch.
    , inisPrivateIpAddresses :: [PrivateIpAddressSpecification]
    , inisSecondaryPrivateIpAddressCount :: Maybe Int
    , inisSubnetId :: Maybe Text
      -- ^ The subnet ID. Applies only when creating a network interface.
    } deriving (Eq, Show, Generic)

instance ToQuery InstanceNetworkInterfaceSpecification

instance FromXML InstanceNetworkInterfaceSpecification where
    fromXMLOptions = xmlOptions

instance ToXML InstanceNetworkInterfaceSpecification where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for InstanceNetworkInterfaceAttachment
data InstanceNetworkInterfaceAttachment = InstanceNetworkInterfaceAttachment
    { iniadAttachTime :: Maybe UTCTime
    , iniadAttachmentId :: Maybe Text
    , iniadDeleteOnTermination :: Maybe Bool
    , iniadDeviceIndex :: Maybe Int
    , iniadStatus :: Maybe AttachmentStatus
    } deriving (Eq, Show, Generic)

instance ToQuery InstanceNetworkInterfaceAttachment

instance FromXML InstanceNetworkInterfaceAttachment where
    fromXMLOptions = xmlOptions

instance ToXML InstanceNetworkInterfaceAttachment where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for InstanceNetworkInterfaceAssociation
data InstanceNetworkInterfaceAssociation = InstanceNetworkInterfaceAssociation
    { iniaIpOwnerId :: Maybe Text
    , iniaPublicDnsName :: Maybe Text
    , iniaPublicIp :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery InstanceNetworkInterfaceAssociation

instance FromXML InstanceNetworkInterfaceAssociation where
    fromXMLOptions = xmlOptions

instance ToXML InstanceNetworkInterfaceAssociation where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for InstanceNetworkInterface
data InstanceNetworkInterface = InstanceNetworkInterface
    { iniAssociation :: Maybe InstanceNetworkInterfaceAssociation
    , iniAttachment :: Maybe InstanceNetworkInterfaceAttachment
    , iniDescription :: Maybe Text
    , iniGroupSet :: [GroupIdentifier]
    , iniNetworkInterfaceId :: Maybe Text
    , iniOwnerId :: Maybe Text
    , iniPrivateDnsName :: Maybe Text
    , iniPrivateIpAddress :: Maybe Text
    , iniPrivateIpAddressesSet :: [InstancePrivateIpAddress]
    , iniSourceDestCheck :: Maybe Bool
    , iniStatus :: Maybe NetworkInterfaceStatus
    , iniSubnetId :: Maybe Text
    , iniVpcId :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery InstanceNetworkInterface

instance FromXML InstanceNetworkInterface where
    fromXMLOptions = xmlOptions

instance ToXML InstanceNetworkInterface where
    toXMLOptions = xmlOptions

-- | Represents the monitoring state of an EC2 instance.
data InstanceMonitoring = InstanceMonitoring
    { imInstanceId :: Maybe Text
      -- ^ Instance ID.
    , imMonitoring :: Maybe Monitoring
      -- ^ Monitoring state for the associated instance.
    } deriving (Eq, Show, Generic)

instance ToQuery InstanceMonitoring

instance FromXML InstanceMonitoring where
    fromXMLOptions = xmlOptions

instance ToXML InstanceMonitoring where
    toXMLOptions = xmlOptions

-- | Specifies active licenses in use and attached to an Amazon EC2 instance.
newtype InstanceLicenseSpecification = InstanceLicenseSpecification
    { ilsPool :: Maybe Text
      -- ^ The license pool from which to take a license when starting Amazon EC2
      -- instances in the associated RunInstances request.
    } deriving (Eq, Show, Generic)

instance ToQuery InstanceLicenseSpecification

instance FromXML InstanceLicenseSpecification where
    fromXMLOptions = xmlOptions

instance ToXML InstanceLicenseSpecification where
    toXMLOptions = xmlOptions

-- | Represents an active license in use and attached to an Amazon EC2 instance.
newtype InstanceLicense = InstanceLicense
    { ilPool :: Maybe Text
      -- ^ The license pool from which this license was used (ex: 'windows').
    } deriving (Eq, Show, Generic)

instance ToQuery InstanceLicense

instance FromXML InstanceLicense where
    fromXMLOptions = xmlOptions

instance ToXML InstanceLicense where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for InstanceExportDetails
data InstanceExportDetails = InstanceExportDetails
    { iedInstanceId :: Maybe Text
    , iedTargetEnvironment :: Maybe ExportEnvironment
    } deriving (Eq, Show, Generic)

instance ToQuery InstanceExportDetails

instance FromXML InstanceExportDetails where
    fromXMLOptions = xmlOptions

instance ToXML InstanceExportDetails where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for InstanceCount
data InstanceCount = InstanceCount
    { icInstanceCount :: Maybe Int
    , icState :: Maybe ListingState
    } deriving (Eq, Show, Generic)

instance ToQuery InstanceCount

instance FromXML InstanceCount where
    fromXMLOptions = xmlOptions

instance ToXML InstanceCount where
    toXMLOptions = xmlOptions

-- | Specifies how an instance's block devices should be mapped on a running
-- instance.
data InstanceBlockDeviceMappingSpecification = InstanceBlockDeviceMappingSpecification
    { ibdmsDeviceName :: Maybe Text
      -- ^ The device name (e.g., /dev/sdh) at which the block device is exposed on
      -- the instance.
    , ibdmsEbs :: Maybe EbsInstanceBlockDeviceSpecification
      -- ^ The EBS instance block device specification describing the EBS block device
      -- to map to the specified device name on a running instance.
    , ibdmsNoDevice :: Maybe Text
      -- ^ When set to the empty string, specifies that the device name in this object
      -- should not be mapped to any real device.
    , ibdmsVirtualName :: Maybe Text
      -- ^ The virtual device name.
    } deriving (Eq, Show, Generic)

instance ToQuery InstanceBlockDeviceMappingSpecification

instance FromXML InstanceBlockDeviceMappingSpecification where
    fromXMLOptions = xmlOptions

instance ToXML InstanceBlockDeviceMappingSpecification where
    toXMLOptions = xmlOptions

-- | Describes how block devices are mapped on an Amazon EC2 instance.
data InstanceBlockDeviceMapping = InstanceBlockDeviceMapping
    { ibdmDeviceName :: Maybe Text
      -- ^ The device name (e.g., /dev/sdh) at which the block device is exposed on
      -- the instance.
    , ibdmEbs :: Maybe EbsInstanceBlockDevice
      -- ^ The optional EBS device mapped to the specified device name.
    } deriving (Eq, Show, Generic)

instance ToQuery InstanceBlockDeviceMapping

instance FromXML InstanceBlockDeviceMapping where
    fromXMLOptions = xmlOptions

instance ToXML InstanceBlockDeviceMapping where
    toXMLOptions = xmlOptions

-- | Represents an Amazon EC2 instance.
data Instance = Instance
    { idAmiLaunchIndex :: Maybe Int
      -- ^ The AMI launch index, which can be used to find this instance within the
      -- launch group.
    , idArchitecture :: Maybe ArchitectureValues
      -- ^ The architecture of this instance.
    , idBlockDeviceMapping :: [InstanceBlockDeviceMapping]
      -- ^ Block device mapping set.
    , idClientToken :: Maybe Text
    , idEbsOptimized :: Maybe Bool
    , idHypervisor :: Maybe HypervisorType
    , idIamInstanceProfile :: Maybe IamInstanceProfile
    , idImageId :: Maybe Text
      -- ^ Image ID of the AMI used to launch the instance.
    , idInstanceId :: Maybe Text
      -- ^ Unique ID of the instance launched.
    , idInstanceLifecycle :: Maybe InstanceLifecycleType
    , idInstanceType :: Maybe InstanceType
      -- ^ The instance type. For more information on instance types, please see the
      -- Amazon Elastic Compute Cloud Developer Guide.
    , idKernelId :: Maybe Text
      -- ^ Kernel associated with this instance.
    , idKeyName :: Maybe Text
      -- ^ If this instance was launched with an associated key pair, this displays
      -- the key pair name.
    , idLaunchTime :: Maybe UTCTime
      -- ^ The time this instance launched.
    , idLicense :: Maybe InstanceLicense
      -- ^ Represents an active license in use and attached to an Amazon EC2 instance.
    , idMonitoring :: Maybe Monitoring
      -- ^ Monitoring status for this instance.
    , idNetworkInterfaceSet :: [InstanceNetworkInterface]
    , idPlacement :: Maybe Placement
      -- ^ The location where this instance launched.
    , idPlatform :: Maybe PlatformValues
      -- ^ Platform of the instance (e.g., Windows).
    , idPrivateDnsName :: Maybe Text
      -- ^ The private DNS name assigned to the instance. This DNS name can only be
      -- used inside the Amazon EC2 network. This element remains empty until the
      -- instance enters a running state.
    , idPrivateIpAddress :: Maybe Text
      -- ^ Specifies the private IP address that is assigned to the instance (Amazon
      -- VPC).
    , idProductCodes :: [ProductCode]
      -- ^ Product codes attached to this instance.
    , idDnsName :: Maybe Text
      -- ^ The public DNS name assigned to the instance. This DNS name is contactable
      -- from outside the Amazon EC2 network. This element remains empty until the
      -- instance enters a running state.
    , idIpAddress :: Maybe Text
      -- ^ Specifies the IP address of the instance.
    , idRamdiskId :: Maybe Text
      -- ^ RAM disk associated with this instance.
    , idRootDeviceName :: Maybe Text
      -- ^ The root device name (e.g., /dev/sda1).
    , idRootDeviceType :: Maybe DeviceType
      -- ^ The root device type used by the AMI. The AMI can use an Amazon EBS or
      -- instance store root device.
    , idGroupSet :: [GroupIdentifier]
    , idSourceDestCheck :: Maybe Bool
    , idSpotInstanceRequestId :: Maybe Text
    , idSriovNetSupport :: Maybe Text
    , idInstanceState :: Maybe InstanceState
      -- ^ The current state of the instance.
    , idStateReason :: Maybe StateReason
      -- ^ The reason for the state change.
    , idReason :: Maybe Text
      -- ^ Reason for the most recent state transition. This might be an empty string.
    , idSubnetId :: Maybe Text
      -- ^ Specifies the Amazon VPC subnet ID in which the instance is running.
    , idTagSet :: [Tag]
      -- ^ A list of tags for the Instance.
    , idVirtualizationType :: Maybe VirtualizationType
    , idVpcId :: Maybe Text
      -- ^ Specifies the Amazon VPC in which the instance is running.
    } deriving (Eq, Show, Generic)

instance ToQuery Instance

instance FromXML Instance where
    fromXMLOptions = xmlOptions

instance ToXML Instance where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for ImportVolumeTaskDetails
data ImportVolumeTaskDetails = ImportVolumeTaskDetails
    { ivtdAvailabilityZone :: !Text
    , ivtdBytesConverted :: !Integer
    , ivtdDescription :: Maybe Text
    , ivtdImage :: DiskImageDescription
    , ivtdVolume :: DiskImageVolumeDescription
    } deriving (Eq, Show, Generic)

instance ToQuery ImportVolumeTaskDetails

instance FromXML ImportVolumeTaskDetails where
    fromXMLOptions = xmlOptions

instance ToXML ImportVolumeTaskDetails where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for ImportInstanceVolumeDetailItem
data ImportInstanceVolumeDetailItem = ImportInstanceVolumeDetailItem
    { iivdiAvailabilityZone :: !Text
    , iivdiBytesConverted :: !Integer
    , iivdiDescription :: Maybe Text
    , iivdiImage :: DiskImageDescription
    , iivdiStatus :: !Text
    , iivdiStatusMessage :: Maybe Text
    , iivdiVolume :: DiskImageVolumeDescription
    } deriving (Eq, Show, Generic)

instance ToQuery ImportInstanceVolumeDetailItem

instance FromXML ImportInstanceVolumeDetailItem where
    fromXMLOptions = xmlOptions

instance ToXML ImportInstanceVolumeDetailItem where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for ImportInstanceTaskDetails
data ImportInstanceTaskDetails = ImportInstanceTaskDetails
    { iitdDescription :: Maybe Text
    , iitdInstanceId :: Maybe Text
    , iitdPlatform :: Maybe PlatformValues
    , iitdVolumes :: [ImportInstanceVolumeDetailItem]
    } deriving (Eq, Show, Generic)

instance ToQuery ImportInstanceTaskDetails

instance FromXML ImportInstanceTaskDetails where
    fromXMLOptions = xmlOptions

instance ToXML ImportInstanceTaskDetails where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for ImportInstanceLaunchSpecification
data ImportInstanceLaunchSpecification = ImportInstanceLaunchSpecification
    { iilsAdditionalInfo :: Maybe Text
    , iilsArchitecture :: Maybe ArchitectureValues
    , iilsGroupNames :: [Text]
    , iilsInstanceInitiatedShutdownBehavior :: Maybe ShutdownBehavior
    , iilsInstanceType :: Maybe InstanceType
    , iilsMonitoring :: Maybe Bool
    , iilsPlacement :: Maybe Placement
      -- ^ Describes where an Amazon EC2 instance is running within an Amazon EC2
      -- region.
    , iilsPrivateIpAddress :: Maybe Text
    , iilsSubnetId :: Maybe Text
    , iilsUserData :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery ImportInstanceLaunchSpecification

instance FromXML ImportInstanceLaunchSpecification where
    fromXMLOptions = xmlOptions

instance ToXML ImportInstanceLaunchSpecification where
    toXMLOptions = xmlOptions

-- | Represents an Amazon Machine Image (AMI) that can be run on an Amazon EC2
-- instance.
data Image = Image
    { iArchitecture :: Maybe ArchitectureValues
      -- ^ The architecture of the image.
    , iBlockDeviceMapping :: [BlockDeviceMapping]
      -- ^ Specifies how block devices are exposed to the instance.
    , iDescription :: Maybe Text
      -- ^ The description of the AMI that was provided during image creation.
    , iHypervisor :: Maybe HypervisorType
    , iImageId :: Maybe Text
      -- ^ The unique ID of the AMI.
    , iImageLocation :: Maybe Text
      -- ^ The location of the AMI.
    , iImageOwnerAlias :: Maybe Text
      -- ^ The AWS account alias (e.g., "amazon", "redhat", "self", etc.) or AWS
      -- account ID that owns the AMI.
    , iImageType :: Maybe ImageTypeValues
      -- ^ The type of image (machine, kernel, or ramdisk).
    , iKernelId :: Maybe Text
      -- ^ The kernel associated with the image, if any. Only applicable for machine
      -- images.
    , iName :: Maybe Text
      -- ^ The name of the AMI that was provided during image creation.
    , iImageOwnerId :: Maybe Text
      -- ^ AWS Access Key ID of the image owner.
    , iPlatform :: Maybe PlatformValues
      -- ^ The operating platform of the AMI.
    , iProductCodes :: [ProductCode]
      -- ^ Product codes of the AMI.
    , iIsPublic :: Maybe Bool
      -- ^ True if this image has public launch permissions. False if it only has
      -- implicit and explicit launch permissions.
    , iRamdiskId :: Maybe Text
      -- ^ The RAM disk associated with the image, if any. Only applicable for machine
      -- images.
    , iRootDeviceName :: Maybe Text
      -- ^ The root device name (e.g., /dev/sda1).
    , iRootDeviceType :: Maybe DeviceType
      -- ^ The root device type used by the AMI. The AMI can use an Amazon EBS or
      -- instance store root device.
    , iSriovNetSupport :: Maybe Text
    , iImageState :: Maybe ImageState
      -- ^ Current state of the AMI. If the operation returns available, the image is
      -- successfully registered and available for launching. If the operation
      -- returns deregistered, the image is deregistered and no longer available for
      -- launching.
    , iStateReason :: Maybe StateReason
      -- ^ The reason for the state change.
    , iTagSet :: [Tag]
      -- ^ A list of tags for the Image.
    , iVirtualizationType :: Maybe VirtualizationType
    } deriving (Eq, Show, Generic)

instance ToQuery Image

instance FromXML Image where
    fromXMLOptions = xmlOptions

instance ToXML Image where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for IcmpTypeCode
data IcmpTypeCode = IcmpTypeCode
    { itcCode :: Maybe Int
      -- ^ For the ICMP protocol, the ICMP code. A value of -1 is a wildcard meaning
      -- all codes. Required if specifying icmp for the protocol.
    , itcType :: Maybe Int
      -- ^ For the ICMP protocol, the ICMP type. A value of -1 is a wildcard meaning
      -- all types. Required if specifying icmp for the protocol.
    } deriving (Eq, Show, Generic)

instance ToQuery IcmpTypeCode

instance FromXML IcmpTypeCode where
    fromXMLOptions = xmlOptions

instance ToXML IcmpTypeCode where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for IamInstanceProfileSpecification
data IamInstanceProfileSpecification = IamInstanceProfileSpecification
    { iipsArn :: Maybe Text
    , iipsName :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery IamInstanceProfileSpecification

instance FromXML IamInstanceProfileSpecification where
    fromXMLOptions = xmlOptions

instance ToXML IamInstanceProfileSpecification where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for IamInstanceProfile
data IamInstanceProfile = IamInstanceProfile
    { iipArn :: Maybe Text
    , iipId :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery IamInstanceProfile

instance FromXML IamInstanceProfile where
    fromXMLOptions = xmlOptions

instance ToXML IamInstanceProfile where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for GroupIdentifier
data GroupIdentifier = GroupIdentifier
    { giGroupId :: Maybe Text
    , giGroupName :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery GroupIdentifier

instance FromXML GroupIdentifier where
    fromXMLOptions = xmlOptions

instance ToXML GroupIdentifier where
    toXMLOptions = xmlOptions

-- | A filter used to limit results when describing tags. Multiple values can be
-- specified per filter. A tag must match at least one of the specified values
-- for it to be returned from an operation. Wildcards can be included in
-- filter values; * specifies that zero or more characters must match, and ?
-- specifies that exactly one character must match. Use a backslash to escape
-- special characters. For example, a filter value of \*amazon\?\\ specifies
-- the literal string *amazon?\.
data Filter = Filter
    { fName :: Maybe Text
      -- ^ Specifies the name of the filter.
    , fValues :: [Text]
      -- ^ Contains one or more values for the filter.
    } deriving (Eq, Show, Generic)

instance ToQuery Filter

instance FromXML Filter where
    fromXMLOptions = xmlOptions

instance ToXML Filter where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for ExportToS3TaskSpecification
data ExportToS3TaskSpecification = ExportToS3TaskSpecification
    { ets3tsContainerFormat :: Maybe ContainerFormat
    , ets3tsDiskImageFormat :: Maybe DiskImageFormat
    , ets3tsS3Bucket :: Maybe Text
    , ets3tsS3Prefix :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery ExportToS3TaskSpecification

instance FromXML ExportToS3TaskSpecification where
    fromXMLOptions = xmlOptions

instance ToXML ExportToS3TaskSpecification where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for ExportToS3Task
data ExportToS3Task = ExportToS3Task
    { ets3tContainerFormat :: Maybe ContainerFormat
    , ets3tDiskImageFormat :: Maybe DiskImageFormat
    , ets3tS3Bucket :: Maybe Text
    , ets3tS3Key :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery ExportToS3Task

instance FromXML ExportToS3Task where
    fromXMLOptions = xmlOptions

instance ToXML ExportToS3Task where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for ExportTask
data ExportTask = ExportTask
    { etDescription :: Maybe Text
    , etExportTaskId :: Maybe Text
    , etExportToS3 :: Maybe ExportToS3Task
    , etInstanceExport :: Maybe InstanceExportDetails
    , etState :: Maybe ExportTaskState
    , etStatusMessage :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery ExportTask

instance FromXML ExportTask where
    fromXMLOptions = xmlOptions

instance ToXML ExportTask where
    toXMLOptions = xmlOptions

-- | The EBS instance block device specification describing the EBS block device
-- to map to the specified device name on a running instance.
data EbsInstanceBlockDeviceSpecification = EbsInstanceBlockDeviceSpecification
    { eibdsDeleteOnTermination :: Maybe Bool
      -- ^ Specifies whether the Amazon EBS volume is deleted on instance termination.
    , eibdsVolumeId :: Maybe Text
      -- ^ The ID of the EBS volume that should be mounted as a block device on an
      -- Amazon EC2 instance.
    } deriving (Eq, Show, Generic)

instance ToQuery EbsInstanceBlockDeviceSpecification

instance FromXML EbsInstanceBlockDeviceSpecification where
    fromXMLOptions = xmlOptions

instance ToXML EbsInstanceBlockDeviceSpecification where
    toXMLOptions = xmlOptions

-- | The optional EBS device mapped to the specified device name.
data EbsInstanceBlockDevice = EbsInstanceBlockDevice
    { eibdAttachTime :: Maybe UTCTime
      -- ^ The time at which the EBS volume was attached to the associated instance.
    , eibdDeleteOnTermination :: Maybe Bool
      -- ^ Specifies whether the Amazon EBS volume is deleted on instance termination.
    , eibdStatus :: Maybe AttachmentStatus
      -- ^ The status of the EBS volume.
    , eibdVolumeId :: Maybe Text
      -- ^ The ID of the EBS volume.
    } deriving (Eq, Show, Generic)

instance ToQuery EbsInstanceBlockDevice

instance FromXML EbsInstanceBlockDevice where
    fromXMLOptions = xmlOptions

instance ToXML EbsInstanceBlockDevice where
    toXMLOptions = xmlOptions

-- | Specifies parameters used to automatically setup Amazon EBS volumes when
-- the instance is launched.
data EbsBlockDevice = EbsBlockDevice
    { ebdDeleteOnTermination :: Maybe Bool
      -- ^ Specifies whether the Amazon EBS volume is deleted on instance termination.
    , ebdIops :: Maybe Int
    , ebdSnapshotId :: Maybe Text
      -- ^ The ID of the snapshot from which the volume will be created.
    , ebdVolumeSize :: Maybe Int
      -- ^ The size of the volume, in gigabytes.
    , ebdVolumeType :: Maybe VolumeType
    } deriving (Eq, Show, Generic)

instance ToQuery EbsBlockDevice

instance FromXML EbsBlockDevice where
    fromXMLOptions = xmlOptions

instance ToXML EbsBlockDevice where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for DiskImageVolumeDescription
data DiskImageVolumeDescription = DiskImageVolumeDescription
    { divdId :: !Text
    , divdSize :: Maybe Integer
    } deriving (Eq, Show, Generic)

instance ToQuery DiskImageVolumeDescription

instance FromXML DiskImageVolumeDescription where
    fromXMLOptions = xmlOptions

instance ToXML DiskImageVolumeDescription where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for DiskImageDetail
data DiskImageDetail = DiskImageDetail
    { dideBytes :: !Integer
    , dideFormat :: !DiskImageFormat
    , dideImportManifestUrl :: !Text
    } deriving (Eq, Show, Generic)

instance ToQuery DiskImageDetail

instance FromXML DiskImageDetail where
    fromXMLOptions = xmlOptions

instance ToXML DiskImageDetail where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for DiskImageDescription
data DiskImageDescription = DiskImageDescription
    { diddChecksum :: Maybe Text
    , diddFormat :: !DiskImageFormat
    , diddImportManifestUrl :: !Text
    , diddSize :: !Integer
    } deriving (Eq, Show, Generic)

instance ToQuery DiskImageDescription

instance FromXML DiskImageDescription where
    fromXMLOptions = xmlOptions

instance ToXML DiskImageDescription where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for DiskImage
data DiskImage = DiskImage
    { difDescription :: Maybe Text
    , difImage :: Maybe DiskImageDetail
    , difVolume :: Maybe VolumeDetail
    } deriving (Eq, Show, Generic)

instance ToQuery DiskImage

instance FromXML DiskImage where
    fromXMLOptions = xmlOptions

instance ToXML DiskImage where
    toXMLOptions = xmlOptions

-- | The DhcpOptions data type.
data DhcpOptions = DhcpOptions
    { doDhcpConfigurationSet :: [DhcpConfiguration]
      -- ^ Contains information about the set of DHCP options.
    , doDhcpOptionsId :: Maybe Text
      -- ^ Specifies the ID of the set of DHCP options.
    , doTagSet :: [Tag]
      -- ^ A list of tags for the DhcpOptions.
    } deriving (Eq, Show, Generic)

instance ToQuery DhcpOptions

instance FromXML DhcpOptions where
    fromXMLOptions = xmlOptions

instance ToXML DhcpOptions where
    toXMLOptions = xmlOptions

-- | The DhcpConfiguration data type.
data DhcpConfiguration = DhcpConfiguration
    { dcKey :: Maybe Text
      -- ^ Contains the name of a DHCP option.
    , dcValueSet :: [Text]
      -- ^ Contains a set of values for a DHCP option.
    } deriving (Eq, Show, Generic)

instance ToQuery DhcpConfiguration

instance FromXML DhcpConfiguration where
    fromXMLOptions = xmlOptions

instance ToXML DhcpConfiguration where
    toXMLOptions = xmlOptions

-- | Information about the customer gateway.
data CustomerGateway = CustomerGateway
    { cgBgpAsn :: Maybe Text
      -- ^ Specifies the customer gateway's Border Gateway Protocol (BGP) Autonomous
      -- System Number (ASN).
    , cgCustomerGatewayId :: Maybe Text
      -- ^ Specifies the ID of the customer gateway.
    , cgIpAddress :: Maybe Text
      -- ^ Contains the Internet-routable IP address of the customer gateway's outside
      -- interface.
    , cgState :: Maybe Text
      -- ^ Describes the current state of the customer gateway. Valid values are
      -- pending, available, deleting, and deleted.
    , cgTagSet :: [Tag]
      -- ^ A list of tags for the CustomerGateway.
    , cgType :: Maybe Text
      -- ^ Specifies the type of VPN connection the customer gateway supports.
    } deriving (Eq, Show, Generic)

instance ToQuery CustomerGateway

instance FromXML CustomerGateway where
    fromXMLOptions = xmlOptions

instance ToXML CustomerGateway where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for CreateVolumePermissionModifications
data CreateVolumePermissionModifications = CreateVolumePermissionModifications
    { cvpmAdd :: [CreateVolumePermission]
    , cvpmRemove :: [CreateVolumePermission]
    } deriving (Eq, Show, Generic)

instance ToQuery CreateVolumePermissionModifications

instance FromXML CreateVolumePermissionModifications where
    fromXMLOptions = xmlOptions

instance ToXML CreateVolumePermissionModifications where
    toXMLOptions = xmlOptions

-- | Describes a permission allowing either a user or group to create a new EBS
-- volume from a snapshot.
data CreateVolumePermission = CreateVolumePermission
    { cvpGroup :: Maybe PermissionGroup
      -- ^ The group that is allowed to create volumes from the snapshot (currently
      -- supports "all").
    , cvpUserId :: Maybe Text
      -- ^ The user ID of the user that can create volumes from the snapshot.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateVolumePermission

instance FromXML CreateVolumePermission where
    fromXMLOptions = xmlOptions

instance ToXML CreateVolumePermission where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for ConversionTask
data ConversionTask = ConversionTask
    { ctdConversionTaskId :: !Text
    , ctdExpirationTime :: Maybe Text
    , ctdImportInstance :: Maybe ImportInstanceTaskDetails
    , ctdImportVolume :: Maybe ImportVolumeTaskDetails
    , ctdState :: !ConversionTaskState
    , ctdStatusMessage :: Maybe Text
    , ctdTagSet :: [Tag]
    } deriving (Eq, Show, Generic)

instance ToQuery ConversionTask

instance FromXML ConversionTask where
    fromXMLOptions = xmlOptions

instance ToXML ConversionTask where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for CancelledSpotInstanceRequest
data CancelledSpotInstanceRequest = CancelledSpotInstanceRequest
    { csirdSpotInstanceRequestId :: Maybe Text
    , csirdState :: Maybe CancelSpotInstanceRequestState
    } deriving (Eq, Show, Generic)

instance ToQuery CancelledSpotInstanceRequest

instance FromXML CancelledSpotInstanceRequest where
    fromXMLOptions = xmlOptions

instance ToXML CancelledSpotInstanceRequest where
    toXMLOptions = xmlOptions

-- | If the task fails, a description of the error.
data BundleTaskError = BundleTaskError
    { bteCode :: Maybe Text
      -- ^ Error code.
    , bteMessage :: Maybe Text
      -- ^ Error message.
    } deriving (Eq, Show, Generic)

instance ToQuery BundleTaskError

instance FromXML BundleTaskError where
    fromXMLOptions = xmlOptions

instance ToXML BundleTaskError where
    toXMLOptions = xmlOptions

-- | The canceled bundle task.
data BundleTask = BundleTask
    { btBundleId :: Maybe Text
      -- ^ Unique identifier for this task.
    , btError :: Maybe BundleTaskError
      -- ^ If the task fails, a description of the error.
    , btInstanceId :: Maybe Text
      -- ^ Instance associated with this bundle task.
    , btProgress :: Maybe Text
      -- ^ The level of task completion, in percent (e.g., 20%).
    , btStartTime :: Maybe UTCTime
      -- ^ The time this task started.
    , btState :: Maybe BundleTaskState
      -- ^ The state of this task.
    , btStorage :: Maybe Storage
      -- ^ Amazon S3 storage locations.
    , btUpdateTime :: Maybe UTCTime
      -- ^ The time of the most recent update for the task.
    } deriving (Eq, Show, Generic)

instance ToQuery BundleTask

instance FromXML BundleTask where
    fromXMLOptions = xmlOptions

instance ToXML BundleTask where
    toXMLOptions = xmlOptions

-- | The BlockDeviceMappingItemType data type.
data BlockDeviceMapping = BlockDeviceMapping
    { bdmDeviceName :: Maybe Text
      -- ^ Specifies the device name (e.g., /dev/sdh).
    , bdmEbs :: Maybe EbsBlockDevice
      -- ^ Specifies parameters used to automatically setup Amazon EBS volumes when
      -- the instance is launched.
    , bdmNoDevice :: Maybe Text
      -- ^ Specifies the device name to suppress during instance launch.
    , bdmVirtualName :: Maybe Text
      -- ^ Specifies the virtual device name.
    } deriving (Eq, Show, Generic)

instance ToQuery BlockDeviceMapping

instance FromXML BlockDeviceMapping where
    fromXMLOptions = xmlOptions

instance ToXML BlockDeviceMapping where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for AvailabilityZoneMessage
newtype AvailabilityZoneMessage = AvailabilityZoneMessage
    { azmMessage :: Maybe Text
      -- ^ FIXME: Type documentation for Text
    } deriving (Eq, Show, Generic)

instance ToQuery AvailabilityZoneMessage

instance FromXML AvailabilityZoneMessage where
    fromXMLOptions = xmlOptions

instance ToXML AvailabilityZoneMessage where
    toXMLOptions = xmlOptions

-- | An EC2 availability zone, separate and fault tolerant from other
-- availability zones.
data AvailabilityZone = AvailabilityZone
    { azMessageSet :: [AvailabilityZoneMessage]
      -- ^ A list of messages about the Availability Zone.
    , azRegionName :: Maybe Text
      -- ^ Name of the region in which this zone resides.
    , azZoneState :: Maybe AvailabilityZoneState
      -- ^ State of the Availability Zone.
    , azZoneName :: Maybe Text
      -- ^ Name of the Availability Zone.
    } deriving (Eq, Show, Generic)

instance ToQuery AvailabilityZone

instance FromXML AvailabilityZone where
    fromXMLOptions = xmlOptions

instance ToXML AvailabilityZone where
    toXMLOptions = xmlOptions

-- | String value.
newtype AttributeValue = AttributeValue
    { avdValue :: Maybe Text
      -- ^ String value.
    } deriving (Eq, Show, Generic)

instance ToQuery AttributeValue

instance FromXML AttributeValue where
    fromXMLOptions = xmlOptions

instance ToXML AttributeValue where
    toXMLOptions = xmlOptions

-- | Boolean value.
newtype AttributeBooleanValue = AttributeBooleanValue
    { abvValue :: Maybe Bool
      -- ^ Boolean value.
    } deriving (Eq, Show, Generic)

instance ToQuery AttributeBooleanValue

instance FromXML AttributeBooleanValue where
    fromXMLOptions = xmlOptions

instance ToXML AttributeBooleanValue where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for Address
data Address = Address
    { aAllocationId :: Maybe Text
    , aAssociationId :: Maybe Text
    , aDomain :: Maybe DomainType
    , aInstanceId :: Maybe Text
    , aNetworkInterfaceId :: Maybe Text
    , aNetworkInterfaceOwnerId :: Maybe Text
    , aPrivateIpAddress :: Maybe Text
    , aPublicIp :: Maybe Text
    } deriving (Eq, Show, Generic)

instance ToQuery Address

instance FromXML Address where
    fromXMLOptions = xmlOptions

instance ToXML Address where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for AccountAttributeValue
newtype AccountAttributeValue = AccountAttributeValue
    { aavAttributeValue :: Maybe Text
      -- ^ FIXME: Type documentation for Text
    } deriving (Eq, Show, Generic)

instance ToQuery AccountAttributeValue

instance FromXML AccountAttributeValue where
    fromXMLOptions = xmlOptions

instance ToXML AccountAttributeValue where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for AccountAttribute
data AccountAttribute = AccountAttribute
    { aaeAttributeName :: Maybe Text
    , aaeAttributeValueSet :: [AccountAttributeValue]
    } deriving (Eq, Show, Generic)

instance ToQuery AccountAttribute

instance FromXML AccountAttribute where
    fromXMLOptions = xmlOptions

instance ToXML AccountAttribute where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for VpnStaticRouteSource
data VpnStaticRouteSource
    = VpnStaticRouteSourceStatic
      deriving (Eq, Ord, Generic)

instance Hashable VpnStaticRouteSource

instance FromText VpnStaticRouteSource where
    fromText "Static" = Right VpnStaticRouteSourceStatic
    fromText e = fromTextFail $ "Unrecognised VpnStaticRouteSource: " <> e

instance Read VpnStaticRouteSource where
    readsPrec _ = fromTextRead

instance ToText VpnStaticRouteSource where
    toText VpnStaticRouteSourceStatic = "Static"

instance Show VpnStaticRouteSource where
    show = toTextShow

instance ToQuery VpnStaticRouteSource where
    toQuery = toTextQuery

instance FromXML VpnStaticRouteSource where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML VpnStaticRouteSource where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | Describes the current state of the VPN gateway. Valid values are pending,
-- available, deleting, and deleted.
data VpnState
    = VpnStateAvailable
    | VpnStateDeleted
    | VpnStateDeleting
    | VpnStatePending
      deriving (Eq, Ord, Generic)

instance Hashable VpnState

instance FromText VpnState where
    fromText "available" = Right VpnStateAvailable
    fromText "deleted" = Right VpnStateDeleted
    fromText "deleting" = Right VpnStateDeleting
    fromText "pending" = Right VpnStatePending
    fromText e = fromTextFail $ "Unrecognised VpnState: " <> e

instance Read VpnState where
    readsPrec _ = fromTextRead

instance ToText VpnState where
    toText VpnStateAvailable = "available"
    toText VpnStateDeleted = "deleted"
    toText VpnStateDeleting = "deleting"
    toText VpnStatePending = "pending"

instance Show VpnState where
    show = toTextShow

instance ToQuery VpnState where
    toQuery = toTextQuery

instance FromXML VpnState where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML VpnState where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | Describes the current state of the VPC. The state of the subnet may be
-- either pending or available.
data VpcState
    = VpcStateAvailable
    | VpcStatePending
      deriving (Eq, Ord, Generic)

instance Hashable VpcState

instance FromText VpcState where
    fromText "available" = Right VpcStateAvailable
    fromText "pending" = Right VpcStatePending
    fromText e = fromTextFail $ "Unrecognised VpcState: " <> e

instance Read VpcState where
    readsPrec _ = fromTextRead

instance ToText VpcState where
    toText VpcStateAvailable = "available"
    toText VpcStatePending = "pending"

instance Show VpcState where
    show = toTextShow

instance ToQuery VpcState where
    toQuery = toTextQuery

instance FromXML VpcState where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML VpcState where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for VpcAttributeName
data VpcAttributeName
    = VpcAttributeNameEnableDnsHostnames
    | VpcAttributeNameEnableDnsSupport
      deriving (Eq, Ord, Generic)

instance Hashable VpcAttributeName

instance FromText VpcAttributeName where
    fromText "enableDnsHostnames" = Right VpcAttributeNameEnableDnsHostnames
    fromText "enableDnsSupport" = Right VpcAttributeNameEnableDnsSupport
    fromText e = fromTextFail $ "Unrecognised VpcAttributeName: " <> e

instance Read VpcAttributeName where
    readsPrec _ = fromTextRead

instance ToText VpcAttributeName where
    toText VpcAttributeNameEnableDnsHostnames = "enableDnsHostnames"
    toText VpcAttributeNameEnableDnsSupport = "enableDnsSupport"

instance Show VpcAttributeName where
    show = toTextShow

instance ToQuery VpcAttributeName where
    toQuery = toTextQuery

instance FromXML VpcAttributeName where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML VpcAttributeName where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for VolumeType
data VolumeType
    = VolumeTypeIO1
    | VolumeTypeStandard
      deriving (Eq, Ord, Generic)

instance Hashable VolumeType

instance FromText VolumeType where
    fromText "io1" = Right VolumeTypeIO1
    fromText "standard" = Right VolumeTypeStandard
    fromText e = fromTextFail $ "Unrecognised VolumeType: " <> e

instance Read VolumeType where
    readsPrec _ = fromTextRead

instance ToText VolumeType where
    toText VolumeTypeIO1 = "io1"
    toText VolumeTypeStandard = "standard"

instance Show VolumeType where
    show = toTextShow

instance ToQuery VolumeType where
    toQuery = toTextQuery

instance FromXML VolumeType where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML VolumeType where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for VolumeStatusName
data VolumeStatusName
    = VolumeStatusNameIoEnabled
    | VolumeStatusNameIoPerformance
      deriving (Eq, Ord, Generic)

instance Hashable VolumeStatusName

instance FromText VolumeStatusName where
    fromText "io-enabled" = Right VolumeStatusNameIoEnabled
    fromText "io-performance" = Right VolumeStatusNameIoPerformance
    fromText e = fromTextFail $ "Unrecognised VolumeStatusName: " <> e

instance Read VolumeStatusName where
    readsPrec _ = fromTextRead

instance ToText VolumeStatusName where
    toText VolumeStatusNameIoEnabled = "io-enabled"
    toText VolumeStatusNameIoPerformance = "io-performance"

instance Show VolumeStatusName where
    show = toTextShow

instance ToQuery VolumeStatusName where
    toQuery = toTextQuery

instance FromXML VolumeStatusName where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML VolumeStatusName where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for VolumeStatusInfoStatus
data VolumeStatusInfoStatus
    = VolumeStatusInfoStatusImpaired
    | VolumeStatusInfoStatusInsufficientData
    | VolumeStatusInfoStatusOk
      deriving (Eq, Ord, Generic)

instance Hashable VolumeStatusInfoStatus

instance FromText VolumeStatusInfoStatus where
    fromText "impaired" = Right VolumeStatusInfoStatusImpaired
    fromText "insufficient-data" = Right VolumeStatusInfoStatusInsufficientData
    fromText "ok" = Right VolumeStatusInfoStatusOk
    fromText e = fromTextFail $ "Unrecognised VolumeStatusInfoStatus: " <> e

instance Read VolumeStatusInfoStatus where
    readsPrec _ = fromTextRead

instance ToText VolumeStatusInfoStatus where
    toText VolumeStatusInfoStatusImpaired = "impaired"
    toText VolumeStatusInfoStatusInsufficientData = "insufficient-data"
    toText VolumeStatusInfoStatusOk = "ok"

instance Show VolumeStatusInfoStatus where
    show = toTextShow

instance ToQuery VolumeStatusInfoStatus where
    toQuery = toTextQuery

instance FromXML VolumeStatusInfoStatus where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML VolumeStatusInfoStatus where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | State of this volume (e.g., creating, available).
data VolumeState
    = VolumeStateAvailable
    | VolumeStateCreating
    | VolumeStateDeleted
    | VolumeStateDeleting
    | VolumeStateError
    | VolumeStateInUse
      deriving (Eq, Ord, Generic)

instance Hashable VolumeState

instance FromText VolumeState where
    fromText "available" = Right VolumeStateAvailable
    fromText "creating" = Right VolumeStateCreating
    fromText "deleted" = Right VolumeStateDeleted
    fromText "deleting" = Right VolumeStateDeleting
    fromText "error" = Right VolumeStateError
    fromText "in-use" = Right VolumeStateInUse
    fromText e = fromTextFail $ "Unrecognised VolumeState: " <> e

instance Read VolumeState where
    readsPrec _ = fromTextRead

instance ToText VolumeState where
    toText VolumeStateAvailable = "available"
    toText VolumeStateCreating = "creating"
    toText VolumeStateDeleted = "deleted"
    toText VolumeStateDeleting = "deleting"
    toText VolumeStateError = "error"
    toText VolumeStateInUse = "in-use"

instance Show VolumeState where
    show = toTextShow

instance ToQuery VolumeState where
    toQuery = toTextQuery

instance FromXML VolumeState where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML VolumeState where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for VolumeAttributeName
data VolumeAttributeName
    = VolumeAttributeNameAutoEnableIO
    | VolumeAttributeNameProductCodes
      deriving (Eq, Ord, Generic)

instance Hashable VolumeAttributeName

instance FromText VolumeAttributeName where
    fromText "autoEnableIO" = Right VolumeAttributeNameAutoEnableIO
    fromText "productCodes" = Right VolumeAttributeNameProductCodes
    fromText e = fromTextFail $ "Unrecognised VolumeAttributeName: " <> e

instance Read VolumeAttributeName where
    readsPrec _ = fromTextRead

instance ToText VolumeAttributeName where
    toText VolumeAttributeNameAutoEnableIO = "autoEnableIO"
    toText VolumeAttributeNameProductCodes = "productCodes"

instance Show VolumeAttributeName where
    show = toTextShow

instance ToQuery VolumeAttributeName where
    toQuery = toTextQuery

instance FromXML VolumeAttributeName where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML VolumeAttributeName where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for VolumeAttachmentState
data VolumeAttachmentState
    = VolumeAttached
    | VolumeAttaching
    | VolumeDetached
    | VolumeDetaching
      deriving (Eq, Ord, Generic)

instance Hashable VolumeAttachmentState

instance FromText VolumeAttachmentState where
    fromText "attached" = Right VolumeAttached
    fromText "attaching" = Right VolumeAttaching
    fromText "detached" = Right VolumeDetached
    fromText "detaching" = Right VolumeDetaching
    fromText e = fromTextFail $ "Unrecognised VolumeAttachmentState: " <> e

instance Read VolumeAttachmentState where
    readsPrec _ = fromTextRead

instance ToText VolumeAttachmentState where
    toText VolumeAttached = "attached"
    toText VolumeAttaching = "attaching"
    toText VolumeDetached = "detached"
    toText VolumeDetaching = "detaching"

instance Show VolumeAttachmentState where
    show = toTextShow

instance ToQuery VolumeAttachmentState where
    toQuery = toTextQuery

instance FromXML VolumeAttachmentState where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML VolumeAttachmentState where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for VirtualizationType
data VirtualizationType
    = VirtualizationTypeHvm
    | VirtualizationTypeParavirtual
      deriving (Eq, Ord, Generic)

instance Hashable VirtualizationType

instance FromText VirtualizationType where
    fromText "hvm" = Right VirtualizationTypeHvm
    fromText "paravirtual" = Right VirtualizationTypeParavirtual
    fromText e = fromTextFail $ "Unrecognised VirtualizationType: " <> e

instance Read VirtualizationType where
    readsPrec _ = fromTextRead

instance ToText VirtualizationType where
    toText VirtualizationTypeHvm = "hvm"
    toText VirtualizationTypeParavirtual = "paravirtual"

instance Show VirtualizationType where
    show = toTextShow

instance ToQuery VirtualizationType where
    toQuery = toTextQuery

instance FromXML VirtualizationType where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML VirtualizationType where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The allowed tenancy of instances launched into the VPC. A value of default
-- means instances can be launched with any tenancy; a value of dedicated
-- means all instances launched into the VPC will be launched as dedicated
-- tenancy regardless of the tenancy assigned to the instance at launch.
data Tenancy
    = TenancyDedicated
    | TenancyDefault
      deriving (Eq, Ord, Generic)

instance Hashable Tenancy

instance FromText Tenancy where
    fromText "dedicated" = Right TenancyDedicated
    fromText "default" = Right TenancyDefault
    fromText e = fromTextFail $ "Unrecognised Tenancy: " <> e

instance Read Tenancy where
    readsPrec _ = fromTextRead

instance ToText Tenancy where
    toText TenancyDedicated = "dedicated"
    toText TenancyDefault = "default"

instance Show Tenancy where
    show = toTextShow

instance ToQuery Tenancy where
    toQuery = toTextQuery

instance FromXML Tenancy where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML Tenancy where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for TelemetryStatus
data TelemetryStatus
    = TelemetryStatusDOWN
    | TelemetryStatusUP
      deriving (Eq, Ord, Generic)

instance Hashable TelemetryStatus

instance FromText TelemetryStatus where
    fromText "DOWN" = Right TelemetryStatusDOWN
    fromText "UP" = Right TelemetryStatusUP
    fromText e = fromTextFail $ "Unrecognised TelemetryStatus: " <> e

instance Read TelemetryStatus where
    readsPrec _ = fromTextRead

instance ToText TelemetryStatus where
    toText TelemetryStatusDOWN = "DOWN"
    toText TelemetryStatusUP = "UP"

instance Show TelemetryStatus where
    show = toTextShow

instance ToQuery TelemetryStatus where
    toQuery = toTextQuery

instance FromXML TelemetryStatus where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML TelemetryStatus where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for SummaryStatus
data SummaryStatus
    = SummaryStatusImpaired
    | SummaryStatusInsufficientData
    | SummaryStatusNotApplicable
    | SummaryStatusOk
      deriving (Eq, Ord, Generic)

instance Hashable SummaryStatus

instance FromText SummaryStatus where
    fromText "impaired" = Right SummaryStatusImpaired
    fromText "insufficient-data" = Right SummaryStatusInsufficientData
    fromText "not-applicable" = Right SummaryStatusNotApplicable
    fromText "ok" = Right SummaryStatusOk
    fromText e = fromTextFail $ "Unrecognised SummaryStatus: " <> e

instance Read SummaryStatus where
    readsPrec _ = fromTextRead

instance ToText SummaryStatus where
    toText SummaryStatusImpaired = "impaired"
    toText SummaryStatusInsufficientData = "insufficient-data"
    toText SummaryStatusNotApplicable = "not-applicable"
    toText SummaryStatusOk = "ok"

instance Show SummaryStatus where
    show = toTextShow

instance ToQuery SummaryStatus where
    toQuery = toTextQuery

instance FromXML SummaryStatus where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML SummaryStatus where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | Describes the current state of the subnet. The state of the subnet may be
-- either pending or available.
data SubnetState
    = SubnetStateAvailable
    | SubnetStatePending
      deriving (Eq, Ord, Generic)

instance Hashable SubnetState

instance FromText SubnetState where
    fromText "available" = Right SubnetStateAvailable
    fromText "pending" = Right SubnetStatePending
    fromText e = fromTextFail $ "Unrecognised SubnetState: " <> e

instance Read SubnetState where
    readsPrec _ = fromTextRead

instance ToText SubnetState where
    toText SubnetStateAvailable = "available"
    toText SubnetStatePending = "pending"

instance Show SubnetState where
    show = toTextShow

instance ToQuery SubnetState where
    toQuery = toTextQuery

instance FromXML SubnetState where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML SubnetState where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for StatusType
data StatusType
    = StatusTypeFailed
    | StatusTypeInsufficientData
    | StatusTypePassed
      deriving (Eq, Ord, Generic)

instance Hashable StatusType

instance FromText StatusType where
    fromText "failed" = Right StatusTypeFailed
    fromText "insufficient-data" = Right StatusTypeInsufficientData
    fromText "passed" = Right StatusTypePassed
    fromText e = fromTextFail $ "Unrecognised StatusType: " <> e

instance Read StatusType where
    readsPrec _ = fromTextRead

instance ToText StatusType where
    toText StatusTypeFailed = "failed"
    toText StatusTypeInsufficientData = "insufficient-data"
    toText StatusTypePassed = "passed"

instance Show StatusType where
    show = toTextShow

instance ToQuery StatusType where
    toQuery = toTextQuery

instance FromXML StatusType where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML StatusType where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for StatusName
data StatusName
    = StatusNameReachability
      deriving (Eq, Ord, Generic)

instance Hashable StatusName

instance FromText StatusName where
    fromText "reachability" = Right StatusNameReachability
    fromText e = fromTextFail $ "Unrecognised StatusName: " <> e

instance Read StatusName where
    readsPrec _ = fromTextRead

instance ToText StatusName where
    toText StatusNameReachability = "reachability"

instance Show StatusName where
    show = toTextShow

instance ToQuery StatusName where
    toQuery = toTextQuery

instance FromXML StatusName where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML StatusName where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | Specifies the Spot Instance type.
data SpotInstanceType
    = SpotInstanceTypeOneTime
    | SpotInstanceTypePersistent
      deriving (Eq, Ord, Generic)

instance Hashable SpotInstanceType

instance FromText SpotInstanceType where
    fromText "one-time" = Right SpotInstanceTypeOneTime
    fromText "persistent" = Right SpotInstanceTypePersistent
    fromText e = fromTextFail $ "Unrecognised SpotInstanceType: " <> e

instance Read SpotInstanceType where
    readsPrec _ = fromTextRead

instance ToText SpotInstanceType where
    toText SpotInstanceTypeOneTime = "one-time"
    toText SpotInstanceTypePersistent = "persistent"

instance Show SpotInstanceType where
    show = toTextShow

instance ToQuery SpotInstanceType where
    toQuery = toTextQuery

instance FromXML SpotInstanceType where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML SpotInstanceType where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for SpotInstanceState
data SpotInstanceState
    = SpotInstanceStateActive
    | SpotInstanceStateCancelled
    | SpotInstanceStateClosed
    | SpotInstanceStateFailed
    | SpotInstanceStateOpen
      deriving (Eq, Ord, Generic)

instance Hashable SpotInstanceState

instance FromText SpotInstanceState where
    fromText "active" = Right SpotInstanceStateActive
    fromText "cancelled" = Right SpotInstanceStateCancelled
    fromText "closed" = Right SpotInstanceStateClosed
    fromText "failed" = Right SpotInstanceStateFailed
    fromText "open" = Right SpotInstanceStateOpen
    fromText e = fromTextFail $ "Unrecognised SpotInstanceState: " <> e

instance Read SpotInstanceState where
    readsPrec _ = fromTextRead

instance ToText SpotInstanceState where
    toText SpotInstanceStateActive = "active"
    toText SpotInstanceStateCancelled = "cancelled"
    toText SpotInstanceStateClosed = "closed"
    toText SpotInstanceStateFailed = "failed"
    toText SpotInstanceStateOpen = "open"

instance Show SpotInstanceState where
    show = toTextShow

instance ToQuery SpotInstanceState where
    toQuery = toTextQuery

instance FromXML SpotInstanceState where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML SpotInstanceState where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | Snapshot state (e.g., pending, completed, or error).
data SnapshotState
    = SnapshotStateCompleted
    | SnapshotStateError
    | SnapshotStatePending
      deriving (Eq, Ord, Generic)

instance Hashable SnapshotState

instance FromText SnapshotState where
    fromText "completed" = Right SnapshotStateCompleted
    fromText "error" = Right SnapshotStateError
    fromText "pending" = Right SnapshotStatePending
    fromText e = fromTextFail $ "Unrecognised SnapshotState: " <> e

instance Read SnapshotState where
    readsPrec _ = fromTextRead

instance ToText SnapshotState where
    toText SnapshotStateCompleted = "completed"
    toText SnapshotStateError = "error"
    toText SnapshotStatePending = "pending"

instance Show SnapshotState where
    show = toTextShow

instance ToQuery SnapshotState where
    toQuery = toTextQuery

instance FromXML SnapshotState where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML SnapshotState where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The name of the EBS attribute to describe. Available attribute names:
-- createVolumePermission.
data SnapshotAttributeName
    = SnapshotCreateVolumePermission
    | SnapshotProductCodes
      deriving (Eq, Ord, Generic)

instance Hashable SnapshotAttributeName

instance FromText SnapshotAttributeName where
    fromText "createVolumePermission" = Right SnapshotCreateVolumePermission
    fromText "productCodes" = Right SnapshotProductCodes
    fromText e = fromTextFail $ "Unrecognised SnapshotAttributeName: " <> e

instance Read SnapshotAttributeName where
    readsPrec _ = fromTextRead

instance ToText SnapshotAttributeName where
    toText SnapshotCreateVolumePermission = "createVolumePermission"
    toText SnapshotProductCodes = "productCodes"

instance Show SnapshotAttributeName where
    show = toTextShow

instance ToQuery SnapshotAttributeName where
    toQuery = toTextQuery

instance FromXML SnapshotAttributeName where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML SnapshotAttributeName where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for ShutdownBehavior
data ShutdownBehavior
    = ShutdownBehaviorStop
    | ShutdownBehaviorTerminate
      deriving (Eq, Ord, Generic)

instance Hashable ShutdownBehavior

instance FromText ShutdownBehavior where
    fromText "stop" = Right ShutdownBehaviorStop
    fromText "terminate" = Right ShutdownBehaviorTerminate
    fromText e = fromTextFail $ "Unrecognised ShutdownBehavior: " <> e

instance Read ShutdownBehavior where
    readsPrec _ = fromTextRead

instance ToText ShutdownBehavior where
    toText ShutdownBehaviorStop = "stop"
    toText ShutdownBehaviorTerminate = "terminate"

instance Show ShutdownBehavior where
    show = toTextShow

instance ToQuery ShutdownBehavior where
    toQuery = toTextQuery

instance FromXML ShutdownBehavior where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML ShutdownBehavior where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for RuleAction
data RuleAction
    = RuleActionAllow
    | RuleActionDeny
      deriving (Eq, Ord, Generic)

instance Hashable RuleAction

instance FromText RuleAction where
    fromText "allow" = Right RuleActionAllow
    fromText "deny" = Right RuleActionDeny
    fromText e = fromTextFail $ "Unrecognised RuleAction: " <> e

instance Read RuleAction where
    readsPrec _ = fromTextRead

instance ToText RuleAction where
    toText RuleActionAllow = "allow"
    toText RuleActionDeny = "deny"

instance Show RuleAction where
    show = toTextShow

instance ToQuery RuleAction where
    toQuery = toTextQuery

instance FromXML RuleAction where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML RuleAction where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for RouteState
data RouteState
    = RouteStateActive
    | RouteStateBlackhole
      deriving (Eq, Ord, Generic)

instance Hashable RouteState

instance FromText RouteState where
    fromText "active" = Right RouteStateActive
    fromText "blackhole" = Right RouteStateBlackhole
    fromText e = fromTextFail $ "Unrecognised RouteState: " <> e

instance Read RouteState where
    readsPrec _ = fromTextRead

instance ToText RouteState where
    toText RouteStateActive = "active"
    toText RouteStateBlackhole = "blackhole"

instance Show RouteState where
    show = toTextShow

instance ToQuery RouteState where
    toQuery = toTextQuery

instance FromXML RouteState where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML RouteState where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The type of resource identified by the associated resource ID (ex:
-- instance, AMI, EBS volume, etc).
data ResourceType
    = ResourceCustomerGateway
    | ResourceDhcpOptions
    | ResourceImage
    | ResourceInstance
    | ResourceInternetGateway
    | ResourceNetworkAcl
    | ResourceNetworkInterface
    | ResourceReservedInstances
    | ResourceRouteTable
    | ResourceSecurityGroup
    | ResourceSnapshot
    | ResourceSpotInstancesRequest
    | ResourceSubnet
    | ResourceVolume
    | ResourceVpc
    | ResourceVpnConnection
    | ResourceVpnGateway
      deriving (Eq, Ord, Generic)

instance Hashable ResourceType

instance FromText ResourceType where
    fromText "customer-gateway" = Right ResourceCustomerGateway
    fromText "dhcp-options" = Right ResourceDhcpOptions
    fromText "image" = Right ResourceImage
    fromText "instance" = Right ResourceInstance
    fromText "internet-gateway" = Right ResourceInternetGateway
    fromText "network-acl" = Right ResourceNetworkAcl
    fromText "network-interface" = Right ResourceNetworkInterface
    fromText "reserved-instances" = Right ResourceReservedInstances
    fromText "route-table" = Right ResourceRouteTable
    fromText "security-group" = Right ResourceSecurityGroup
    fromText "snapshot" = Right ResourceSnapshot
    fromText "spot-instances-request" = Right ResourceSpotInstancesRequest
    fromText "subnet" = Right ResourceSubnet
    fromText "volume" = Right ResourceVolume
    fromText "vpc" = Right ResourceVpc
    fromText "vpn-connection" = Right ResourceVpnConnection
    fromText "vpn-gateway" = Right ResourceVpnGateway
    fromText e = fromTextFail $ "Unrecognised ResourceType: " <> e

instance Read ResourceType where
    readsPrec _ = fromTextRead

instance ToText ResourceType where
    toText ResourceCustomerGateway = "customer-gateway"
    toText ResourceDhcpOptions = "dhcp-options"
    toText ResourceImage = "image"
    toText ResourceInstance = "instance"
    toText ResourceInternetGateway = "internet-gateway"
    toText ResourceNetworkAcl = "network-acl"
    toText ResourceNetworkInterface = "network-interface"
    toText ResourceReservedInstances = "reserved-instances"
    toText ResourceRouteTable = "route-table"
    toText ResourceSecurityGroup = "security-group"
    toText ResourceSnapshot = "snapshot"
    toText ResourceSpotInstancesRequest = "spot-instances-request"
    toText ResourceSubnet = "subnet"
    toText ResourceVolume = "volume"
    toText ResourceVpc = "vpc"
    toText ResourceVpnConnection = "vpn-connection"
    toText ResourceVpnGateway = "vpn-gateway"

instance Show ResourceType where
    show = toTextShow

instance ToQuery ResourceType where
    toQuery = toTextQuery

instance FromXML ResourceType where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML ResourceType where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The name of the attribute being reset. Available attribute names:
-- launchPermission.
data ResetImageAttributeName
    = ResetImageAttributeNameLaunchPermission
      deriving (Eq, Ord, Generic)

instance Hashable ResetImageAttributeName

instance FromText ResetImageAttributeName where
    fromText "launchPermission" = Right ResetImageAttributeNameLaunchPermission
    fromText e = fromTextFail $ "Unrecognised ResetImageAttributeName: " <> e

instance Read ResetImageAttributeName where
    readsPrec _ = fromTextRead

instance ToText ResetImageAttributeName where
    toText ResetImageAttributeNameLaunchPermission = "launchPermission"

instance Show ResetImageAttributeName where
    show = toTextShow

instance ToQuery ResetImageAttributeName where
    toQuery = toTextQuery

instance FromXML ResetImageAttributeName where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML ResetImageAttributeName where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The state of the Reserved Instances purchase.
data ReservedInstanceState
    = ReservedInstanceStateActive
    | ReservedInstanceStatePaymentFailed
    | ReservedInstanceStatePaymentPending
    | ReservedInstanceStateRetired
      deriving (Eq, Ord, Generic)

instance Hashable ReservedInstanceState

instance FromText ReservedInstanceState where
    fromText "active" = Right ReservedInstanceStateActive
    fromText "payment-failed" = Right ReservedInstanceStatePaymentFailed
    fromText "payment-pending" = Right ReservedInstanceStatePaymentPending
    fromText "retired" = Right ReservedInstanceStateRetired
    fromText e = fromTextFail $ "Unrecognised ReservedInstanceState: " <> e

instance Read ReservedInstanceState where
    readsPrec _ = fromTextRead

instance ToText ReservedInstanceState where
    toText ReservedInstanceStateActive = "active"
    toText ReservedInstanceStatePaymentFailed = "payment-failed"
    toText ReservedInstanceStatePaymentPending = "payment-pending"
    toText ReservedInstanceStateRetired = "retired"

instance Show ReservedInstanceState where
    show = toTextShow

instance ToQuery ReservedInstanceState where
    toQuery = toTextQuery

instance FromXML ReservedInstanceState where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML ReservedInstanceState where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for ReportStatusType
data ReportStatusType
    = ReportStatusTypeImpaired
    | ReportStatusTypeOk
      deriving (Eq, Ord, Generic)

instance Hashable ReportStatusType

instance FromText ReportStatusType where
    fromText "impaired" = Right ReportStatusTypeImpaired
    fromText "ok" = Right ReportStatusTypeOk
    fromText e = fromTextFail $ "Unrecognised ReportStatusType: " <> e

instance Read ReportStatusType where
    readsPrec _ = fromTextRead

instance ToText ReportStatusType where
    toText ReportStatusTypeImpaired = "impaired"
    toText ReportStatusTypeOk = "ok"

instance Show ReportStatusType where
    show = toTextShow

instance ToQuery ReportStatusType where
    toQuery = toTextQuery

instance FromXML ReportStatusType where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML ReportStatusType where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for ReportInstanceReasonCodes
data ReportInstanceReasonCodes
    = InstanceStuckInState
    | NotAcceptingCredentials
    | Other
    | PasswordNotAvailable
    | PerformanceEbsVolume
    | PerformanceInstanceStore
    | PerformanceNetwork
    | PerformanceOther
    | Unresponsive
      deriving (Eq, Ord, Generic)

instance Hashable ReportInstanceReasonCodes

instance FromText ReportInstanceReasonCodes where
    fromText "instance-stuck-in-state" = Right InstanceStuckInState
    fromText "not-accepting-credentials" = Right NotAcceptingCredentials
    fromText "other" = Right Other
    fromText "password-not-available" = Right PasswordNotAvailable
    fromText "performance-ebs-volume" = Right PerformanceEbsVolume
    fromText "performance-instance-store" = Right PerformanceInstanceStore
    fromText "performance-network" = Right PerformanceNetwork
    fromText "performance-other" = Right PerformanceOther
    fromText "unresponsive" = Right Unresponsive
    fromText e = fromTextFail $ "Unrecognised ReportInstanceReasonCodes: " <> e

instance Read ReportInstanceReasonCodes where
    readsPrec _ = fromTextRead

instance ToText ReportInstanceReasonCodes where
    toText InstanceStuckInState = "instance-stuck-in-state"
    toText NotAcceptingCredentials = "not-accepting-credentials"
    toText Other = "other"
    toText PasswordNotAvailable = "password-not-available"
    toText PerformanceEbsVolume = "performance-ebs-volume"
    toText PerformanceInstanceStore = "performance-instance-store"
    toText PerformanceNetwork = "performance-network"
    toText PerformanceOther = "performance-other"
    toText Unresponsive = "unresponsive"

instance Show ReportInstanceReasonCodes where
    show = toTextShow

instance ToQuery ReportInstanceReasonCodes where
    toQuery = toTextQuery

instance FromXML ReportInstanceReasonCodes where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML ReportInstanceReasonCodes where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The frequency of the recurring charge.
data RecurringChargeFrequency
    = Hourly
      deriving (Eq, Ord, Generic)

instance Hashable RecurringChargeFrequency

instance FromText RecurringChargeFrequency where
    fromText "Hourly" = Right Hourly
    fromText e = fromTextFail $ "Unrecognised RecurringChargeFrequency: " <> e

instance Read RecurringChargeFrequency where
    readsPrec _ = fromTextRead

instance ToText RecurringChargeFrequency where
    toText Hourly = "Hourly"

instance Show RecurringChargeFrequency where
    show = toTextShow

instance ToQuery RecurringChargeFrequency where
    toQuery = toTextQuery

instance FromXML RecurringChargeFrequency where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML RecurringChargeFrequency where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for RIProductDescription
data RIProductDescription
    = Linux_UNIX
    | Linux_UNIX_AmazonVPC
    | Windows
    | Windows_AmazonVPC
      deriving (Eq, Ord, Generic)

instance Hashable RIProductDescription

instance FromText RIProductDescription where
    fromText "Linux/UNIX" = Right Linux_UNIX
    fromText "Linux/UNIX (Amazon VPC)" = Right Linux_UNIX_AmazonVPC
    fromText "Windows" = Right Windows
    fromText "Windows (Amazon VPC)" = Right Windows_AmazonVPC
    fromText e = fromTextFail $ "Unrecognised RIProductDescription: " <> e

instance Read RIProductDescription where
    readsPrec _ = fromTextRead

instance ToText RIProductDescription where
    toText Linux_UNIX = "Linux/UNIX"
    toText Linux_UNIX_AmazonVPC = "Linux/UNIX (Amazon VPC)"
    toText Windows = "Windows"
    toText Windows_AmazonVPC = "Windows (Amazon VPC)"

instance Show RIProductDescription where
    show = toTextShow

instance ToQuery RIProductDescription where
    toQuery = toTextQuery

instance FromXML RIProductDescription where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML RIProductDescription where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for ProductCodeValues
data ProductCodeValues
    = Devpay
    | Marketplace
      deriving (Eq, Ord, Generic)

instance Hashable ProductCodeValues

instance FromText ProductCodeValues where
    fromText "devpay" = Right Devpay
    fromText "marketplace" = Right Marketplace
    fromText e = fromTextFail $ "Unrecognised ProductCodeValues: " <> e

instance Read ProductCodeValues where
    readsPrec _ = fromTextRead

instance ToText ProductCodeValues where
    toText Devpay = "devpay"
    toText Marketplace = "marketplace"

instance Show ProductCodeValues where
    show = toTextShow

instance ToQuery ProductCodeValues where
    toQuery = toTextQuery

instance FromXML ProductCodeValues where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML ProductCodeValues where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for PlatformValues
data PlatformValues
    = PlatformValuesWindows
      deriving (Eq, Ord, Generic)

instance Hashable PlatformValues

instance FromText PlatformValues where
    fromText "Windows" = Right PlatformValuesWindows
    fromText e = fromTextFail $ "Unrecognised PlatformValues: " <> e

instance Read PlatformValues where
    readsPrec _ = fromTextRead

instance ToText PlatformValues where
    toText PlatformValuesWindows = "Windows"

instance Show PlatformValues where
    show = toTextShow

instance ToQuery PlatformValues where
    toQuery = toTextQuery

instance FromXML PlatformValues where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML PlatformValues where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The PlacementGroup strategy.
data PlacementStrategy
    = PlacementStrategyCluster
      deriving (Eq, Ord, Generic)

instance Hashable PlacementStrategy

instance FromText PlacementStrategy where
    fromText "cluster" = Right PlacementStrategyCluster
    fromText e = fromTextFail $ "Unrecognised PlacementStrategy: " <> e

instance Read PlacementStrategy where
    readsPrec _ = fromTextRead

instance ToText PlacementStrategy where
    toText PlacementStrategyCluster = "cluster"

instance Show PlacementStrategy where
    show = toTextShow

instance ToQuery PlacementStrategy where
    toQuery = toTextQuery

instance FromXML PlacementStrategy where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML PlacementStrategy where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The state of this PlacementGroup.
data PlacementGroupState
    = PlacementGroupStateAvailable
    | PlacementGroupStateDeleted
    | PlacementGroupStateDeleting
    | PlacementGroupStatePending
      deriving (Eq, Ord, Generic)

instance Hashable PlacementGroupState

instance FromText PlacementGroupState where
    fromText "available" = Right PlacementGroupStateAvailable
    fromText "deleted" = Right PlacementGroupStateDeleted
    fromText "deleting" = Right PlacementGroupStateDeleting
    fromText "pending" = Right PlacementGroupStatePending
    fromText e = fromTextFail $ "Unrecognised PlacementGroupState: " <> e

instance Read PlacementGroupState where
    readsPrec _ = fromTextRead

instance ToText PlacementGroupState where
    toText PlacementGroupStateAvailable = "available"
    toText PlacementGroupStateDeleted = "deleted"
    toText PlacementGroupStateDeleting = "deleting"
    toText PlacementGroupStatePending = "pending"

instance Show PlacementGroupState where
    show = toTextShow

instance ToQuery PlacementGroupState where
    toQuery = toTextQuery

instance FromXML PlacementGroupState where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML PlacementGroupState where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The group that is allowed to create volumes from the snapshot (currently
-- supports "all").
data PermissionGroup
    = PermissionGroupAll
      deriving (Eq, Ord, Generic)

instance Hashable PermissionGroup

instance FromText PermissionGroup where
    fromText "all" = Right PermissionGroupAll
    fromText e = fromTextFail $ "Unrecognised PermissionGroup: " <> e

instance Read PermissionGroup where
    readsPrec _ = fromTextRead

instance ToText PermissionGroup where
    toText PermissionGroupAll = "all"

instance Show PermissionGroup where
    show = toTextShow

instance ToQuery PermissionGroup where
    toQuery = toTextQuery

instance FromXML PermissionGroup where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML PermissionGroup where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The Reserved Instance offering type.
data OfferingTypeValues
    = HeavyUtilization
    | LightUtilization
    | MediumUtilization
      deriving (Eq, Ord, Generic)

instance Hashable OfferingTypeValues

instance FromText OfferingTypeValues where
    fromText "Heavy Utilization" = Right HeavyUtilization
    fromText "Light Utilization" = Right LightUtilization
    fromText "Medium Utilization" = Right MediumUtilization
    fromText e = fromTextFail $ "Unrecognised OfferingTypeValues: " <> e

instance Read OfferingTypeValues where
    readsPrec _ = fromTextRead

instance ToText OfferingTypeValues where
    toText HeavyUtilization = "Heavy Utilization"
    toText LightUtilization = "Light Utilization"
    toText MediumUtilization = "Medium Utilization"

instance Show OfferingTypeValues where
    show = toTextShow

instance ToQuery OfferingTypeValues where
    toQuery = toTextQuery

instance FromXML OfferingTypeValues where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML OfferingTypeValues where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for NetworkInterfaceStatus
data NetworkInterfaceStatus
    = NetworkInterfaceStatusAttaching
    | NetworkInterfaceStatusAvailable
    | NetworkInterfaceStatusDetaching
    | NetworkInterfaceStatusInUse
      deriving (Eq, Ord, Generic)

instance Hashable NetworkInterfaceStatus

instance FromText NetworkInterfaceStatus where
    fromText "attaching" = Right NetworkInterfaceStatusAttaching
    fromText "available" = Right NetworkInterfaceStatusAvailable
    fromText "detaching" = Right NetworkInterfaceStatusDetaching
    fromText "in-use" = Right NetworkInterfaceStatusInUse
    fromText e = fromTextFail $ "Unrecognised NetworkInterfaceStatus: " <> e

instance Read NetworkInterfaceStatus where
    readsPrec _ = fromTextRead

instance ToText NetworkInterfaceStatus where
    toText NetworkInterfaceStatusAttaching = "attaching"
    toText NetworkInterfaceStatusAvailable = "available"
    toText NetworkInterfaceStatusDetaching = "detaching"
    toText NetworkInterfaceStatusInUse = "in-use"

instance Show NetworkInterfaceStatus where
    show = toTextShow

instance ToQuery NetworkInterfaceStatus where
    toQuery = toTextQuery

instance FromXML NetworkInterfaceStatus where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML NetworkInterfaceStatus where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The state of monitoring on an Amazon EC2 instance (ex: enabled, disabled).
data MonitoringState
    = MonitoringStateDisabled
    | MonitoringStateEnabled
    | MonitoringStatePending
      deriving (Eq, Ord, Generic)

instance Hashable MonitoringState

instance FromText MonitoringState where
    fromText "disabled" = Right MonitoringStateDisabled
    fromText "enabled" = Right MonitoringStateEnabled
    fromText "pending" = Right MonitoringStatePending
    fromText e = fromTextFail $ "Unrecognised MonitoringState: " <> e

instance Read MonitoringState where
    readsPrec _ = fromTextRead

instance ToText MonitoringState where
    toText MonitoringStateDisabled = "disabled"
    toText MonitoringStateEnabled = "enabled"
    toText MonitoringStatePending = "pending"

instance Show MonitoringState where
    show = toTextShow

instance ToQuery MonitoringState where
    toQuery = toTextQuery

instance FromXML MonitoringState where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML MonitoringState where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for ListingStatus
data ListingStatus
    = ListingStatusActive
    | ListingStatusCancelled
    | ListingStatusClosed
    | ListingStatusPending
      deriving (Eq, Ord, Generic)

instance Hashable ListingStatus

instance FromText ListingStatus where
    fromText "active" = Right ListingStatusActive
    fromText "cancelled" = Right ListingStatusCancelled
    fromText "closed" = Right ListingStatusClosed
    fromText "pending" = Right ListingStatusPending
    fromText e = fromTextFail $ "Unrecognised ListingStatus: " <> e

instance Read ListingStatus where
    readsPrec _ = fromTextRead

instance ToText ListingStatus where
    toText ListingStatusActive = "active"
    toText ListingStatusCancelled = "cancelled"
    toText ListingStatusClosed = "closed"
    toText ListingStatusPending = "pending"

instance Show ListingStatus where
    show = toTextShow

instance ToQuery ListingStatus where
    toQuery = toTextQuery

instance FromXML ListingStatus where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML ListingStatus where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for ListingState
data ListingState
    = ListingStateAvailable
    | ListingStateCancelled
    | ListingStatePending
    | ListingStateSold
      deriving (Eq, Ord, Generic)

instance Hashable ListingState

instance FromText ListingState where
    fromText "available" = Right ListingStateAvailable
    fromText "cancelled" = Right ListingStateCancelled
    fromText "pending" = Right ListingStatePending
    fromText "sold" = Right ListingStateSold
    fromText e = fromTextFail $ "Unrecognised ListingState: " <> e

instance Read ListingState where
    readsPrec _ = fromTextRead

instance ToText ListingState where
    toText ListingStateAvailable = "available"
    toText ListingStateCancelled = "cancelled"
    toText ListingStatePending = "pending"
    toText ListingStateSold = "sold"

instance Show ListingState where
    show = toTextShow

instance ToQuery ListingState where
    toQuery = toTextQuery

instance FromXML ListingState where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML ListingState where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for InstanceType
data InstanceType
    = C1_Medium
    | C1_Xlarge
    | C3_2Xlarge
    | C3_4Xlarge
    | C3_8Xlarge
    | C3_Large
    | C3_Xlarge
    | CC1_4Xlarge
    | CC2_8Xlarge
    | CG1_4Xlarge
    | CR1_8Xlarge
    | G2_2Xlarge
    | HI1_4Xlarge
    | HS1_8Xlarge
    | I2_2Xlarge
    | I2_4Xlarge
    | I2_8Xlarge
    | I2_Xlarge
    | M1_Large
    | M1_Medium
    | M1_Small
    | M1_Xlarge
    | M2_2Xlarge
    | M2_4Xlarge
    | M2_Xlarge
    | M3_2Xlarge
    | M3_Xlarge
    | T1_Micro
      deriving (Eq, Ord, Generic)

instance Hashable InstanceType

instance FromText InstanceType where
    fromText "c1.medium" = Right C1_Medium
    fromText "c1.xlarge" = Right C1_Xlarge
    fromText "c3.2xlarge" = Right C3_2Xlarge
    fromText "c3.4xlarge" = Right C3_4Xlarge
    fromText "c3.8xlarge" = Right C3_8Xlarge
    fromText "c3.large" = Right C3_Large
    fromText "c3.xlarge" = Right C3_Xlarge
    fromText "cc1.4xlarge" = Right CC1_4Xlarge
    fromText "cc2.8xlarge" = Right CC2_8Xlarge
    fromText "cg1.4xlarge" = Right CG1_4Xlarge
    fromText "cr1.8xlarge" = Right CR1_8Xlarge
    fromText "g2.2xlarge" = Right G2_2Xlarge
    fromText "hi1.4xlarge" = Right HI1_4Xlarge
    fromText "hs1.8xlarge" = Right HS1_8Xlarge
    fromText "i2.2xlarge" = Right I2_2Xlarge
    fromText "i2.4xlarge" = Right I2_4Xlarge
    fromText "i2.8xlarge" = Right I2_8Xlarge
    fromText "i2.xlarge" = Right I2_Xlarge
    fromText "m1.large" = Right M1_Large
    fromText "m1.medium" = Right M1_Medium
    fromText "m1.small" = Right M1_Small
    fromText "m1.xlarge" = Right M1_Xlarge
    fromText "m2.2xlarge" = Right M2_2Xlarge
    fromText "m2.4xlarge" = Right M2_4Xlarge
    fromText "m2.xlarge" = Right M2_Xlarge
    fromText "m3.2xlarge" = Right M3_2Xlarge
    fromText "m3.xlarge" = Right M3_Xlarge
    fromText "t1.micro" = Right T1_Micro
    fromText e = fromTextFail $ "Unrecognised InstanceType: " <> e

instance Read InstanceType where
    readsPrec _ = fromTextRead

instance ToText InstanceType where
    toText C1_Medium = "c1.medium"
    toText C1_Xlarge = "c1.xlarge"
    toText C3_2Xlarge = "c3.2xlarge"
    toText C3_4Xlarge = "c3.4xlarge"
    toText C3_8Xlarge = "c3.8xlarge"
    toText C3_Large = "c3.large"
    toText C3_Xlarge = "c3.xlarge"
    toText CC1_4Xlarge = "cc1.4xlarge"
    toText CC2_8Xlarge = "cc2.8xlarge"
    toText CG1_4Xlarge = "cg1.4xlarge"
    toText CR1_8Xlarge = "cr1.8xlarge"
    toText G2_2Xlarge = "g2.2xlarge"
    toText HI1_4Xlarge = "hi1.4xlarge"
    toText HS1_8Xlarge = "hs1.8xlarge"
    toText I2_2Xlarge = "i2.2xlarge"
    toText I2_4Xlarge = "i2.4xlarge"
    toText I2_8Xlarge = "i2.8xlarge"
    toText I2_Xlarge = "i2.xlarge"
    toText M1_Large = "m1.large"
    toText M1_Medium = "m1.medium"
    toText M1_Small = "m1.small"
    toText M1_Xlarge = "m1.xlarge"
    toText M2_2Xlarge = "m2.2xlarge"
    toText M2_4Xlarge = "m2.4xlarge"
    toText M2_Xlarge = "m2.xlarge"
    toText M3_2Xlarge = "m3.2xlarge"
    toText M3_Xlarge = "m3.xlarge"
    toText T1_Micro = "t1.micro"

instance Show InstanceType where
    show = toTextShow

instance ToQuery InstanceType where
    toQuery = toTextQuery

instance FromXML InstanceType where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML InstanceType where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The current state of the instance.
data InstanceStateName
    = InstanceStateNamePending
    | InstanceStateNameRunning
    | InstanceStateNameShuttingDown
    | InstanceStateNameStopped
    | InstanceStateNameStopping
    | InstanceStateNameTerminated
      deriving (Eq, Ord, Generic)

instance Hashable InstanceStateName

instance FromText InstanceStateName where
    fromText "pending" = Right InstanceStateNamePending
    fromText "running" = Right InstanceStateNameRunning
    fromText "shutting-down" = Right InstanceStateNameShuttingDown
    fromText "stopped" = Right InstanceStateNameStopped
    fromText "stopping" = Right InstanceStateNameStopping
    fromText "terminated" = Right InstanceStateNameTerminated
    fromText e = fromTextFail $ "Unrecognised InstanceStateName: " <> e

instance Read InstanceStateName where
    readsPrec _ = fromTextRead

instance ToText InstanceStateName where
    toText InstanceStateNamePending = "pending"
    toText InstanceStateNameRunning = "running"
    toText InstanceStateNameShuttingDown = "shutting-down"
    toText InstanceStateNameStopped = "stopped"
    toText InstanceStateNameStopping = "stopping"
    toText InstanceStateNameTerminated = "terminated"

instance Show InstanceStateName where
    show = toTextShow

instance ToQuery InstanceStateName where
    toQuery = toTextQuery

instance FromXML InstanceStateName where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML InstanceStateName where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for InstanceLifecycleType
data InstanceLifecycleType
    = InstanceLifecycleTypeSpot
      deriving (Eq, Ord, Generic)

instance Hashable InstanceLifecycleType

instance FromText InstanceLifecycleType where
    fromText "spot" = Right InstanceLifecycleTypeSpot
    fromText e = fromTextFail $ "Unrecognised InstanceLifecycleType: " <> e

instance Read InstanceLifecycleType where
    readsPrec _ = fromTextRead

instance ToText InstanceLifecycleType where
    toText InstanceLifecycleTypeSpot = "spot"

instance Show InstanceLifecycleType where
    show = toTextShow

instance ToQuery InstanceLifecycleType where
    toQuery = toTextQuery

instance FromXML InstanceLifecycleType where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML InstanceLifecycleType where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The name of the attribute being reset. Available attribute names: kernel,
-- ramdisk.
data InstanceAttributeName
    = InstanceAttributeNameBlockDeviceMapping
    | InstanceAttributeNameDisableApiTermination
    | InstanceAttributeNameEbsOptimized
    | InstanceAttributeNameGroupSet
    | InstanceAttributeNameInstanceInitiatedShutdownBehavior
    | InstanceAttributeNameInstanceType
    | InstanceAttributeNameKernel
    | InstanceAttributeNameProductCodes
    | InstanceAttributeNameRamdisk
    | InstanceAttributeNameRootDeviceName
    | InstanceAttributeNameSourceDestCheck
    | InstanceAttributeNameUserData
      deriving (Eq, Ord, Generic)

instance Hashable InstanceAttributeName

instance FromText InstanceAttributeName where
    fromText "blockDeviceMapping" = Right InstanceAttributeNameBlockDeviceMapping
    fromText "disableApiTermination" = Right InstanceAttributeNameDisableApiTermination
    fromText "ebsOptimized" = Right InstanceAttributeNameEbsOptimized
    fromText "groupSet" = Right InstanceAttributeNameGroupSet
    fromText "instanceInitiatedShutdownBehavior" = Right InstanceAttributeNameInstanceInitiatedShutdownBehavior
    fromText "instanceType" = Right InstanceAttributeNameInstanceType
    fromText "kernel" = Right InstanceAttributeNameKernel
    fromText "productCodes" = Right InstanceAttributeNameProductCodes
    fromText "ramdisk" = Right InstanceAttributeNameRamdisk
    fromText "rootDeviceName" = Right InstanceAttributeNameRootDeviceName
    fromText "sourceDestCheck" = Right InstanceAttributeNameSourceDestCheck
    fromText "userData" = Right InstanceAttributeNameUserData
    fromText e = fromTextFail $ "Unrecognised InstanceAttributeName: " <> e

instance Read InstanceAttributeName where
    readsPrec _ = fromTextRead

instance ToText InstanceAttributeName where
    toText InstanceAttributeNameBlockDeviceMapping = "blockDeviceMapping"
    toText InstanceAttributeNameDisableApiTermination = "disableApiTermination"
    toText InstanceAttributeNameEbsOptimized = "ebsOptimized"
    toText InstanceAttributeNameGroupSet = "groupSet"
    toText InstanceAttributeNameInstanceInitiatedShutdownBehavior = "instanceInitiatedShutdownBehavior"
    toText InstanceAttributeNameInstanceType = "instanceType"
    toText InstanceAttributeNameKernel = "kernel"
    toText InstanceAttributeNameProductCodes = "productCodes"
    toText InstanceAttributeNameRamdisk = "ramdisk"
    toText InstanceAttributeNameRootDeviceName = "rootDeviceName"
    toText InstanceAttributeNameSourceDestCheck = "sourceDestCheck"
    toText InstanceAttributeNameUserData = "userData"

instance Show InstanceAttributeName where
    show = toTextShow

instance ToQuery InstanceAttributeName where
    toQuery = toTextQuery

instance FromXML InstanceAttributeName where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML InstanceAttributeName where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The type of image (machine, kernel, or ramdisk).
data ImageTypeValues
    = ImageTypeValuesKernel
    | ImageTypeValuesMachine
    | ImageTypeValuesRamdisk
      deriving (Eq, Ord, Generic)

instance Hashable ImageTypeValues

instance FromText ImageTypeValues where
    fromText "kernel" = Right ImageTypeValuesKernel
    fromText "machine" = Right ImageTypeValuesMachine
    fromText "ramdisk" = Right ImageTypeValuesRamdisk
    fromText e = fromTextFail $ "Unrecognised ImageTypeValues: " <> e

instance Read ImageTypeValues where
    readsPrec _ = fromTextRead

instance ToText ImageTypeValues where
    toText ImageTypeValuesKernel = "kernel"
    toText ImageTypeValuesMachine = "machine"
    toText ImageTypeValuesRamdisk = "ramdisk"

instance Show ImageTypeValues where
    show = toTextShow

instance ToQuery ImageTypeValues where
    toQuery = toTextQuery

instance FromXML ImageTypeValues where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML ImageTypeValues where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | Current state of the AMI. If the operation returns available, the image is
-- successfully registered and available for launching. If the operation
-- returns deregistered, the image is deregistered and no longer available for
-- launching.
data ImageState
    = ImageStateAvailable
    | ImageStateDeregistered
      deriving (Eq, Ord, Generic)

instance Hashable ImageState

instance FromText ImageState where
    fromText "available" = Right ImageStateAvailable
    fromText "deregistered" = Right ImageStateDeregistered
    fromText e = fromTextFail $ "Unrecognised ImageState: " <> e

instance Read ImageState where
    readsPrec _ = fromTextRead

instance ToText ImageState where
    toText ImageStateAvailable = "available"
    toText ImageStateDeregistered = "deregistered"

instance Show ImageState where
    show = toTextShow

instance ToQuery ImageState where
    toQuery = toTextQuery

instance FromXML ImageState where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML ImageState where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The name of the attribute to describe. Available attribute names:
-- productCodes, kernel, ramdisk, launchPermisson, blockDeviceMapping.
data ImageAttributeName
    = ImageAttributeNameBlockDeviceMapping
    | ImageAttributeNameDescription
    | ImageAttributeNameKernel
    | ImageAttributeNameLaunchPermission
    | ImageAttributeNameProductCodes
    | ImageAttributeNameRamdisk
      deriving (Eq, Ord, Generic)

instance Hashable ImageAttributeName

instance FromText ImageAttributeName where
    fromText "blockDeviceMapping" = Right ImageAttributeNameBlockDeviceMapping
    fromText "description" = Right ImageAttributeNameDescription
    fromText "kernel" = Right ImageAttributeNameKernel
    fromText "launchPermission" = Right ImageAttributeNameLaunchPermission
    fromText "productCodes" = Right ImageAttributeNameProductCodes
    fromText "ramdisk" = Right ImageAttributeNameRamdisk
    fromText e = fromTextFail $ "Unrecognised ImageAttributeName: " <> e

instance Read ImageAttributeName where
    readsPrec _ = fromTextRead

instance ToText ImageAttributeName where
    toText ImageAttributeNameBlockDeviceMapping = "blockDeviceMapping"
    toText ImageAttributeNameDescription = "description"
    toText ImageAttributeNameKernel = "kernel"
    toText ImageAttributeNameLaunchPermission = "launchPermission"
    toText ImageAttributeNameProductCodes = "productCodes"
    toText ImageAttributeNameRamdisk = "ramdisk"

instance Show ImageAttributeName where
    show = toTextShow

instance ToQuery ImageAttributeName where
    toQuery = toTextQuery

instance FromXML ImageAttributeName where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML ImageAttributeName where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for HypervisorType
data HypervisorType
    = Ovm
    | Xen
      deriving (Eq, Ord, Generic)

instance Hashable HypervisorType

instance FromText HypervisorType where
    fromText "ovm" = Right Ovm
    fromText "xen" = Right Xen
    fromText e = fromTextFail $ "Unrecognised HypervisorType: " <> e

instance Read HypervisorType where
    readsPrec _ = fromTextRead

instance ToText HypervisorType where
    toText Ovm = "ovm"
    toText Xen = "xen"

instance Show HypervisorType where
    show = toTextShow

instance ToQuery HypervisorType where
    toQuery = toTextQuery

instance FromXML HypervisorType where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML HypervisorType where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The type of VPN connection this VPN gateway supports.
data GatewayType
    = GatewayTypeIpsec_1
      deriving (Eq, Ord, Generic)

instance Hashable GatewayType

instance FromText GatewayType where
    fromText "ipsec.1" = Right GatewayTypeIpsec_1
    fromText e = fromTextFail $ "Unrecognised GatewayType: " <> e

instance Read GatewayType where
    readsPrec _ = fromTextRead

instance ToText GatewayType where
    toText GatewayTypeIpsec_1 = "ipsec.1"

instance Show GatewayType where
    show = toTextShow

instance ToQuery GatewayType where
    toQuery = toTextQuery

instance FromXML GatewayType where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML GatewayType where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for ExportTaskState
data ExportTaskState
    = ExportTaskStateActive
    | ExportTaskStateCancelled
    | ExportTaskStateCancelling
    | ExportTaskStateCompleted
      deriving (Eq, Ord, Generic)

instance Hashable ExportTaskState

instance FromText ExportTaskState where
    fromText "active" = Right ExportTaskStateActive
    fromText "cancelled" = Right ExportTaskStateCancelled
    fromText "cancelling" = Right ExportTaskStateCancelling
    fromText "completed" = Right ExportTaskStateCompleted
    fromText e = fromTextFail $ "Unrecognised ExportTaskState: " <> e

instance Read ExportTaskState where
    readsPrec _ = fromTextRead

instance ToText ExportTaskState where
    toText ExportTaskStateActive = "active"
    toText ExportTaskStateCancelled = "cancelled"
    toText ExportTaskStateCancelling = "cancelling"
    toText ExportTaskStateCompleted = "completed"

instance Show ExportTaskState where
    show = toTextShow

instance ToQuery ExportTaskState where
    toQuery = toTextQuery

instance FromXML ExportTaskState where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML ExportTaskState where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for ExportEnvironment
data ExportEnvironment
    = Citrix
    | Microsoft
    | Vmware
      deriving (Eq, Ord, Generic)

instance Hashable ExportEnvironment

instance FromText ExportEnvironment where
    fromText "citrix" = Right Citrix
    fromText "microsoft" = Right Microsoft
    fromText "vmware" = Right Vmware
    fromText e = fromTextFail $ "Unrecognised ExportEnvironment: " <> e

instance Read ExportEnvironment where
    readsPrec _ = fromTextRead

instance ToText ExportEnvironment where
    toText Citrix = "citrix"
    toText Microsoft = "microsoft"
    toText Vmware = "vmware"

instance Show ExportEnvironment where
    show = toTextShow

instance ToQuery ExportEnvironment where
    toQuery = toTextQuery

instance FromXML ExportEnvironment where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML ExportEnvironment where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The associated code of the event. Valid values: instance-reboot,
-- system-reboot, instance-retirement.
data EventCode
    = InstanceReboot
    | InstanceRetirement
    | InstanceStop
    | SystemMaintenance
    | SystemReboot
      deriving (Eq, Ord, Generic)

instance Hashable EventCode

instance FromText EventCode where
    fromText "instance-reboot" = Right InstanceReboot
    fromText "instance-retirement" = Right InstanceRetirement
    fromText "instance-stop" = Right InstanceStop
    fromText "system-maintenance" = Right SystemMaintenance
    fromText "system-reboot" = Right SystemReboot
    fromText e = fromTextFail $ "Unrecognised EventCode: " <> e

instance Read EventCode where
    readsPrec _ = fromTextRead

instance ToText EventCode where
    toText InstanceReboot = "instance-reboot"
    toText InstanceRetirement = "instance-retirement"
    toText InstanceStop = "instance-stop"
    toText SystemMaintenance = "system-maintenance"
    toText SystemReboot = "system-reboot"

instance Show EventCode where
    show = toTextShow

instance ToQuery EventCode where
    toQuery = toTextQuery

instance FromXML EventCode where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML EventCode where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for DomainType
data DomainType
    = DomainTypeStandard
    | DomainTypeVpc
      deriving (Eq, Ord, Generic)

instance Hashable DomainType

instance FromText DomainType where
    fromText "standard" = Right DomainTypeStandard
    fromText "vpc" = Right DomainTypeVpc
    fromText e = fromTextFail $ "Unrecognised DomainType: " <> e

instance Read DomainType where
    readsPrec _ = fromTextRead

instance ToText DomainType where
    toText DomainTypeStandard = "standard"
    toText DomainTypeVpc = "vpc"

instance Show DomainType where
    show = toTextShow

instance ToQuery DomainType where
    toQuery = toTextQuery

instance FromXML DomainType where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML DomainType where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for DiskImageFormat
data DiskImageFormat
    = RAW
    | VHD
    | VMDK
      deriving (Eq, Ord, Generic)

instance Hashable DiskImageFormat

instance FromText DiskImageFormat where
    fromText "RAW" = Right RAW
    fromText "VHD" = Right VHD
    fromText "VMDK" = Right VMDK
    fromText e = fromTextFail $ "Unrecognised DiskImageFormat: " <> e

instance Read DiskImageFormat where
    readsPrec _ = fromTextRead

instance ToText DiskImageFormat where
    toText RAW = "RAW"
    toText VHD = "VHD"
    toText VMDK = "VMDK"

instance Show DiskImageFormat where
    show = toTextShow

instance ToQuery DiskImageFormat where
    toQuery = toTextQuery

instance FromXML DiskImageFormat where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML DiskImageFormat where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The root device type used by the AMI. The AMI can use an Amazon EBS or
-- instance store root device.
data DeviceType
    = DeviceTypeEbs
    | DeviceTypeInstanceStore
      deriving (Eq, Ord, Generic)

instance Hashable DeviceType

instance FromText DeviceType where
    fromText "ebs" = Right DeviceTypeEbs
    fromText "instance-store" = Right DeviceTypeInstanceStore
    fromText e = fromTextFail $ "Unrecognised DeviceType: " <> e

instance Read DeviceType where
    readsPrec _ = fromTextRead

instance ToText DeviceType where
    toText DeviceTypeEbs = "ebs"
    toText DeviceTypeInstanceStore = "instance-store"

instance Show DeviceType where
    show = toTextShow

instance ToQuery DeviceType where
    toQuery = toTextQuery

instance FromXML DeviceType where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML DeviceType where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | Specifies the state of the Spot Instance request.
data DatafeedSubscriptionState
    = DatafeedSubscriptionStateActive
    | DatafeedSubscriptionStateInactive
      deriving (Eq, Ord, Generic)

instance Hashable DatafeedSubscriptionState

instance FromText DatafeedSubscriptionState where
    fromText "Active" = Right DatafeedSubscriptionStateActive
    fromText "Inactive" = Right DatafeedSubscriptionStateInactive
    fromText e = fromTextFail $ "Unrecognised DatafeedSubscriptionState: " <> e

instance Read DatafeedSubscriptionState where
    readsPrec _ = fromTextRead

instance ToText DatafeedSubscriptionState where
    toText DatafeedSubscriptionStateActive = "Active"
    toText DatafeedSubscriptionStateInactive = "Inactive"

instance Show DatafeedSubscriptionState where
    show = toTextShow

instance ToQuery DatafeedSubscriptionState where
    toQuery = toTextQuery

instance FromXML DatafeedSubscriptionState where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML DatafeedSubscriptionState where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for CurrencyCodeValues
data CurrencyCodeValues
    = USD
      deriving (Eq, Ord, Generic)

instance Hashable CurrencyCodeValues

instance FromText CurrencyCodeValues where
    fromText "USD" = Right USD
    fromText e = fromTextFail $ "Unrecognised CurrencyCodeValues: " <> e

instance Read CurrencyCodeValues where
    readsPrec _ = fromTextRead

instance ToText CurrencyCodeValues where
    toText USD = "USD"

instance Show CurrencyCodeValues where
    show = toTextShow

instance ToQuery CurrencyCodeValues where
    toQuery = toTextQuery

instance FromXML CurrencyCodeValues where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML CurrencyCodeValues where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for ConversionTaskState
data ConversionTaskState
    = ConversionTaskStateActive
    | ConversionTaskStateCancelled
    | ConversionTaskStateCancelling
    | ConversionTaskStateCompleted
      deriving (Eq, Ord, Generic)

instance Hashable ConversionTaskState

instance FromText ConversionTaskState where
    fromText "active" = Right ConversionTaskStateActive
    fromText "cancelled" = Right ConversionTaskStateCancelled
    fromText "cancelling" = Right ConversionTaskStateCancelling
    fromText "completed" = Right ConversionTaskStateCompleted
    fromText e = fromTextFail $ "Unrecognised ConversionTaskState: " <> e

instance Read ConversionTaskState where
    readsPrec _ = fromTextRead

instance ToText ConversionTaskState where
    toText ConversionTaskStateActive = "active"
    toText ConversionTaskStateCancelled = "cancelled"
    toText ConversionTaskStateCancelling = "cancelling"
    toText ConversionTaskStateCompleted = "completed"

instance Show ConversionTaskState where
    show = toTextShow

instance ToQuery ConversionTaskState where
    toQuery = toTextQuery

instance FromXML ConversionTaskState where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML ConversionTaskState where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for ContainerFormat
data ContainerFormat
    = Ova
      deriving (Eq, Ord, Generic)

instance Hashable ContainerFormat

instance FromText ContainerFormat where
    fromText "ova" = Right Ova
    fromText e = fromTextFail $ "Unrecognised ContainerFormat: " <> e

instance Read ContainerFormat where
    readsPrec _ = fromTextRead

instance ToText ContainerFormat where
    toText Ova = "ova"

instance Show ContainerFormat where
    show = toTextShow

instance ToQuery ContainerFormat where
    toQuery = toTextQuery

instance FromXML ContainerFormat where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML ContainerFormat where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for CancelSpotInstanceRequestState
data CancelSpotInstanceRequestState
    = CancelSpotInstanceRequestStateActive
    | CancelSpotInstanceRequestStateCancelled
    | CancelSpotInstanceRequestStateClosed
    | CancelSpotInstanceRequestStateCompleted
    | CancelSpotInstanceRequestStateOpen
      deriving (Eq, Ord, Generic)

instance Hashable CancelSpotInstanceRequestState

instance FromText CancelSpotInstanceRequestState where
    fromText "active" = Right CancelSpotInstanceRequestStateActive
    fromText "cancelled" = Right CancelSpotInstanceRequestStateCancelled
    fromText "closed" = Right CancelSpotInstanceRequestStateClosed
    fromText "completed" = Right CancelSpotInstanceRequestStateCompleted
    fromText "open" = Right CancelSpotInstanceRequestStateOpen
    fromText e = fromTextFail $ "Unrecognised CancelSpotInstanceRequestState: " <> e

instance Read CancelSpotInstanceRequestState where
    readsPrec _ = fromTextRead

instance ToText CancelSpotInstanceRequestState where
    toText CancelSpotInstanceRequestStateActive = "active"
    toText CancelSpotInstanceRequestStateCancelled = "cancelled"
    toText CancelSpotInstanceRequestStateClosed = "closed"
    toText CancelSpotInstanceRequestStateCompleted = "completed"
    toText CancelSpotInstanceRequestStateOpen = "open"

instance Show CancelSpotInstanceRequestState where
    show = toTextShow

instance ToQuery CancelSpotInstanceRequestState where
    toQuery = toTextQuery

instance FromXML CancelSpotInstanceRequestState where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML CancelSpotInstanceRequestState where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The state of this task.
data BundleTaskState
    = BundleTaskStateBundling
    | BundleTaskStateCancelling
    | BundleTaskStateComplete
    | BundleTaskStateFailed
    | BundleTaskStatePending
    | BundleTaskStateStoring
    | BundleTaskStateWaitingForShutdown
      deriving (Eq, Ord, Generic)

instance Hashable BundleTaskState

instance FromText BundleTaskState where
    fromText "bundling" = Right BundleTaskStateBundling
    fromText "cancelling" = Right BundleTaskStateCancelling
    fromText "complete" = Right BundleTaskStateComplete
    fromText "failed" = Right BundleTaskStateFailed
    fromText "pending" = Right BundleTaskStatePending
    fromText "storing" = Right BundleTaskStateStoring
    fromText "waiting-for-shutdown" = Right BundleTaskStateWaitingForShutdown
    fromText e = fromTextFail $ "Unrecognised BundleTaskState: " <> e

instance Read BundleTaskState where
    readsPrec _ = fromTextRead

instance ToText BundleTaskState where
    toText BundleTaskStateBundling = "bundling"
    toText BundleTaskStateCancelling = "cancelling"
    toText BundleTaskStateComplete = "complete"
    toText BundleTaskStateFailed = "failed"
    toText BundleTaskStatePending = "pending"
    toText BundleTaskStateStoring = "storing"
    toText BundleTaskStateWaitingForShutdown = "waiting-for-shutdown"

instance Show BundleTaskState where
    show = toTextShow

instance ToQuery BundleTaskState where
    toQuery = toTextQuery

instance FromXML BundleTaskState where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML BundleTaskState where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | State of the Availability Zone.
data AvailabilityZoneState
    = AvailabilityZoneStateAvailable
      deriving (Eq, Ord, Generic)

instance Hashable AvailabilityZoneState

instance FromText AvailabilityZoneState where
    fromText "available" = Right AvailabilityZoneStateAvailable
    fromText e = fromTextFail $ "Unrecognised AvailabilityZoneState: " <> e

instance Read AvailabilityZoneState where
    readsPrec _ = fromTextRead

instance ToText AvailabilityZoneState where
    toText AvailabilityZoneStateAvailable = "available"

instance Show AvailabilityZoneState where
    show = toTextShow

instance ToQuery AvailabilityZoneState where
    toQuery = toTextQuery

instance FromXML AvailabilityZoneState where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML AvailabilityZoneState where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for AttachmentStatus
data AttachmentStatus
    = AttachmentStatusAttached
    | AttachmentStatusAttaching
    | AttachmentStatusDetached
    | AttachmentStatusDetaching
      deriving (Eq, Ord, Generic)

instance Hashable AttachmentStatus

instance FromText AttachmentStatus where
    fromText "attached" = Right AttachmentStatusAttached
    fromText "attaching" = Right AttachmentStatusAttaching
    fromText "detached" = Right AttachmentStatusDetached
    fromText "detaching" = Right AttachmentStatusDetaching
    fromText e = fromTextFail $ "Unrecognised AttachmentStatus: " <> e

instance Read AttachmentStatus where
    readsPrec _ = fromTextRead

instance ToText AttachmentStatus where
    toText AttachmentStatusAttached = "attached"
    toText AttachmentStatusAttaching = "attaching"
    toText AttachmentStatusDetached = "detached"
    toText AttachmentStatusDetaching = "detaching"

instance Show AttachmentStatus where
    show = toTextShow

instance ToQuery AttachmentStatus where
    toQuery = toTextQuery

instance FromXML AttachmentStatus where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML AttachmentStatus where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for ArchitectureValues
data ArchitectureValues
    = I386
    | X86_64
      deriving (Eq, Ord, Generic)

instance Hashable ArchitectureValues

instance FromText ArchitectureValues where
    fromText "i386" = Right I386
    fromText "x86_64" = Right X86_64
    fromText e = fromTextFail $ "Unrecognised ArchitectureValues: " <> e

instance Read ArchitectureValues where
    readsPrec _ = fromTextRead

instance ToText ArchitectureValues where
    toText I386 = "i386"
    toText X86_64 = "x86_64"

instance Show ArchitectureValues where
    show = toTextShow

instance ToQuery ArchitectureValues where
    toQuery = toTextQuery

instance FromXML ArchitectureValues where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML ArchitectureValues where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | FIXME: Type documentation for AccountAttributeName
data AccountAttributeName
    = DefaultVpc
    | SupportedPlatforms
      deriving (Eq, Ord, Generic)

instance Hashable AccountAttributeName

instance FromText AccountAttributeName where
    fromText "default-vpc" = Right DefaultVpc
    fromText "supported-platforms" = Right SupportedPlatforms
    fromText e = fromTextFail $ "Unrecognised AccountAttributeName: " <> e

instance Read AccountAttributeName where
    readsPrec _ = fromTextRead

instance ToText AccountAttributeName where
    toText DefaultVpc = "default-vpc"
    toText SupportedPlatforms = "supported-platforms"

instance Show AccountAttributeName where
    show = toTextShow

instance ToQuery AccountAttributeName where
    toQuery = toTextQuery

instance FromXML AccountAttributeName where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML AccountAttributeName where
    toXMLOptions = xmlOptions
    toXML        = toTextXML
