{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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

import Data.ByteString      (ByteString)
import Data.Monoid
import Data.List.NonEmpty
import Data.Text            (Text)
import Data.Time
import GHC.Generics
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service

-- | FIXME: Type documentation for VpnStaticRoute
data VpnStaticRoute = VpnStaticRoute
    { vsrDestinationCidrBlock :: Maybe Text
      -- ^ FIXME: Missing documentation
    , vsrSource :: Maybe VpnStaticRouteSource
      -- ^ FIXME: Missing documentation
    , vsrState :: Maybe VpnState
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery VpnStaticRoute

instance FromXML VpnStaticRoute where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for VpnGateway
data VpnGateway = VpnGateway
    { vgAvailabilityZone :: Maybe Text
      -- ^ Specifies the Availability Zone where the VPN gateway was created.
    , vgState :: Maybe VpnState
      -- ^ Describes the current state of the VPN gateway. Valid values are pending,
      -- available, deleting, and deleted.
    , vgTags :: [Tag]
      -- ^ A list of tags for the VpnGateway.
    , vgType :: Maybe GatewayType
      -- ^ Specifies the type of VPN connection the VPN gateway supports.
    , vgVpcAttachments :: [VpcAttachment]
      -- ^ Contains information about the VPCs attached to the VPN gateway.
    , vgVpnGatewayId :: Maybe Text
      -- ^ Specifies the ID of the VPN gateway.
    } deriving (Eq, Show, Generic)

instance ToQuery VpnGateway

instance FromXML VpnGateway where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for VpnConnectionOptionsSpecification
newtype VpnConnectionOptionsSpecification = VpnConnectionOptionsSpecification
    { vcosStaticRoutesOnly :: Bool
      -- ^ FIXME: Type documentation for Bool
    } deriving (Eq, Show, Generic)

instance ToQuery VpnConnectionOptionsSpecification

instance FromXML VpnConnectionOptionsSpecification where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for VpnConnectionOptions
newtype VpnConnectionOptions = VpnConnectionOptions
    { vcoStaticRoutesOnly :: Bool
      -- ^ FIXME: Type documentation for Bool
    } deriving (Eq, Show, Generic)

instance ToQuery VpnConnectionOptions

instance FromXML VpnConnectionOptions where
    fromXMLOptions = xmlOptions

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
      -- ^ FIXME: Missing documentation
    , vcRoutes :: [VpnStaticRoute]
      -- ^ FIXME: Missing documentation
    , vcState :: Maybe VpnState
      -- ^ Describes the current state of the VPN connection. Valid values are
      -- pending, available, deleting, and deleted.
    , vcTags :: [Tag]
      -- ^ A list of tags for the VpnConnection.
    , vcType :: Maybe GatewayType
      -- ^ Specifies the type of VPN connection.
    , vcVgwTelemetry :: [VgwTelemetry]
      -- ^ FIXME: Missing documentation
    , vcVpnConnectionId :: Maybe Text
      -- ^ Specifies the ID of the VPN gateway at the VPC end of the VPN connection.
    , vcVpnGatewayId :: Maybe Text
      -- ^ Specfies the ID of the VPN gateway at the VPC end of the VPN connection.
    } deriving (Eq, Show, Generic)

instance ToQuery VpnConnection

instance FromXML VpnConnection where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for VpcAttachment
data VpcAttachment = VpcAttachment
    { vbState :: Maybe AttachmentStatus
      -- ^ FIXME: Missing documentation
    , vbVpcId :: Maybe Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery VpcAttachment

instance FromXML VpcAttachment where
    fromXMLOptions = xmlOptions

-- | The Vpc data type.
data Vpc = Vpc
    { zCidrBlock :: Maybe Text
      -- ^ Specifies the CIDR block the VPC covers.
    , zDhcpOptionsId :: Maybe Text
      -- ^ Specifies the ID of the set of DHCP options associated with the VPC.
      -- Contains a value of default if the default options are associated with the
      -- VPC.
    , zInstanceTenancy :: Maybe Tenancy
      -- ^ The allowed tenancy of instances launched into the VPC.
    , zIsDefault :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , zState :: Maybe VpcState
      -- ^ Describes the current state of the VPC. The state of the subnet may be
      -- either pending or available.
    , zTags :: [Tag]
      -- ^ A list of tags for the VPC.
    , zVpcId :: Maybe Text
      -- ^ Specifies the ID of the VPC.
    } deriving (Eq, Show, Generic)

instance ToQuery Vpc

instance FromXML Vpc where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for VolumeStatusItem
data VolumeStatusItem = VolumeStatusItem
    { vsjActions :: [VolumeStatusAction]
      -- ^ FIXME: Missing documentation
    , vsjAvailabilityZone :: Maybe Text
      -- ^ FIXME: Missing documentation
    , vsjEvents :: [VolumeStatusEvent]
      -- ^ FIXME: Missing documentation
    , vsjVolumeId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , vsjVolumeStatus :: Maybe VolumeStatusInfo
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery VolumeStatusItem

instance FromXML VolumeStatusItem where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for VolumeStatusInfo
data VolumeStatusInfo = VolumeStatusInfo
    { vsiDetails :: [VolumeStatusDetails]
      -- ^ FIXME: Missing documentation
    , vsiStatus :: Maybe VolumeStatusInfoStatus
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery VolumeStatusInfo

instance FromXML VolumeStatusInfo where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for VolumeStatusEvent
data VolumeStatusEvent = VolumeStatusEvent
    { vseDescription :: Maybe Text
      -- ^ FIXME: Missing documentation
    , vseEventId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , vseEventType :: Maybe Text
      -- ^ FIXME: Missing documentation
    , vseNotAfter :: Maybe UTCTime
      -- ^ FIXME: Missing documentation
    , vseNotBefore :: Maybe UTCTime
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery VolumeStatusEvent

instance FromXML VolumeStatusEvent where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for VolumeStatusDetails
data VolumeStatusDetails = VolumeStatusDetails
    { vsdName :: Maybe VolumeStatusName
      -- ^ FIXME: Missing documentation
    , vsdStatus :: Maybe Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery VolumeStatusDetails

instance FromXML VolumeStatusDetails where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for VolumeStatusAction
data VolumeStatusAction = VolumeStatusAction
    { vsaCode :: Maybe Text
      -- ^ FIXME: Missing documentation
    , vsaDescription :: Maybe Text
      -- ^ FIXME: Missing documentation
    , vsaEventId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , vsaEventType :: Maybe Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery VolumeStatusAction

instance FromXML VolumeStatusAction where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for VolumeDetail
newtype VolumeDetail = VolumeDetail
    { vdSize :: Integer
      -- ^ FIXME: Type documentation for Integer
    } deriving (Eq, Show, Generic)

instance ToQuery VolumeDetail

instance FromXML VolumeDetail where
    fromXMLOptions = xmlOptions

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
      -- ^ FIXME: Missing documentation
    , vaState :: Maybe VolumeAttachmentState
      -- ^ FIXME: Missing documentation
    , vaVolumeId :: Maybe Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery VolumeAttachment

instance FromXML VolumeAttachment where
    fromXMLOptions = xmlOptions

-- | Represents an Amazon Elastic Block Storage (EBS) volume.
data Volume = Volume
    { yAttachments :: [VolumeAttachment]
      -- ^ Information on what this volume is attached to.
    , yAvailabilityZone :: Maybe Text
      -- ^ Availability zone in which this volume was created.
    , yCreateTime :: Maybe UTCTime
      -- ^ Timestamp when volume creation was initiated.
    , yIops :: Maybe Int
      -- ^ FIXME: Missing documentation
    , ySize :: Maybe Int
      -- ^ The size of this volume, in gigabytes.
    , ySnapshotId :: Maybe Text
      -- ^ Optional snapshot from which this volume was created.
    , yState :: Maybe VolumeState
      -- ^ State of this volume (e.g., creating, available).
    , yTags :: [Tag]
      -- ^ A list of tags for the Volume.
    , yVolumeId :: Maybe Text
      -- ^ The unique ID of this volume.
    , yVolumeType :: Maybe VolumeType
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery Volume

instance FromXML Volume where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for VgwTelemetry
data VgwTelemetry = VgwTelemetry
    { vtAcceptedRouteCount :: Maybe Int
      -- ^ FIXME: Missing documentation
    , vtLastStatusChange :: Maybe UTCTime
      -- ^ FIXME: Missing documentation
    , vtOutsideIpAddress :: Maybe Text
      -- ^ FIXME: Missing documentation
    , vtStatus :: Maybe TelemetryStatus
      -- ^ FIXME: Missing documentation
    , vtStatusMessage :: Maybe Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery VgwTelemetry

instance FromXML VgwTelemetry where
    fromXMLOptions = xmlOptions

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

-- | Represents metadata to associate with Amazon EC2 resources. Each tag
-- consists of a user-defined key and value. Use tags to categorize EC2
-- resources, such as by purpose, owner, or environment.
data Tag = Tag
    { xKey :: Maybe Text
      -- ^ The tag's key.
    , xValue :: Maybe Text
      -- ^ The tag's value.
    } deriving (Eq, Show, Generic)

instance ToQuery Tag

instance FromXML Tag where
    fromXMLOptions = xmlOptions

-- | The Subnet data type.
data Subnet = Subnet
    { wAvailabilityZone :: Maybe Text
      -- ^ Specifies the Availability Zone the subnet is in.
    , wAvailableIpAddressCount :: Maybe Int
      -- ^ Specifies the number of unused IP addresses in the subnet. The IP addresses
      -- for any stopped instances are considered unavailable.
    , wCidrBlock :: Maybe Text
      -- ^ Specifies the CIDR block assigned to the subnet.
    , wDefaultForAz :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , wMapPublicIpOnLaunch :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , wState :: Maybe SubnetState
      -- ^ Describes the current state of the subnet. The state of the subnet may be
      -- either pending or available.
    , wSubnetId :: Maybe Text
      -- ^ Specifies the ID of the subnet.
    , wTags :: [Tag]
      -- ^ A list of tags for the Subnet.
    , wVpcId :: Maybe Text
      -- ^ Contains the ID of the VPC the subnet is in.
    } deriving (Eq, Show, Generic)

instance ToQuery Subnet

instance FromXML Subnet where
    fromXMLOptions = xmlOptions

-- | Amazon S3 storage locations.
newtype Storage = Storage
    { vS3 :: S3Storage
      -- ^ The details of S3 storage for bundling a Windows instance.
    } deriving (Eq, Show, Generic)

instance ToQuery Storage

instance FromXML Storage where
    fromXMLOptions = xmlOptions

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

-- | FIXME: Type documentation for SpotPrice
data SpotPrice = SpotPrice
    { sqAvailabilityZone :: Maybe Text
      -- ^ FIXME: Missing documentation
    , sqInstanceType :: Maybe InstanceType
      -- ^ FIXME: Missing documentation
    , sqProductDescription :: Maybe RIProductDescription
      -- ^ FIXME: Missing documentation
    , sqSpotPrice :: Maybe Text
      -- ^ FIXME: Missing documentation
    , sqTimestamp :: Maybe UTCTime
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery SpotPrice

instance FromXML SpotPrice where
    fromXMLOptions = xmlOptions

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

-- | FIXME: Type documentation for SpotInstanceStatus
data SpotInstanceStatus = SpotInstanceStatus
    { siuCode :: Maybe Text
      -- ^ FIXME: Missing documentation
    , siuMessage :: Maybe Text
      -- ^ FIXME: Missing documentation
    , siuUpdateTime :: Maybe UTCTime
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery SpotInstanceStatus

instance FromXML SpotInstanceStatus where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for SpotInstanceStateFault
data SpotInstanceStateFault = SpotInstanceStateFault
    { sisfCode :: Maybe Text
      -- ^ FIXME: Missing documentation
    , sisfMessage :: Maybe Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery SpotInstanceStateFault

instance FromXML SpotInstanceStateFault where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for SpotInstanceRequest
data SpotInstanceRequest = SpotInstanceRequest
    { sitAvailabilityZoneGroup :: Maybe Text
      -- ^ FIXME: Missing documentation
    , sitCreateTime :: Maybe UTCTime
      -- ^ FIXME: Missing documentation
    , sitFault :: Maybe SpotInstanceStateFault
      -- ^ FIXME: Missing documentation
    , sitInstanceId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , sitLaunchGroup :: Maybe Text
      -- ^ FIXME: Missing documentation
    , sitLaunchSpecification :: Maybe LaunchSpecification
      -- ^ The LaunchSpecificationType data type.
    , sitLaunchedAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone in which the bid is launched.
    , sitProductDescription :: Maybe RIProductDescription
      -- ^ FIXME: Missing documentation
    , sitSpotInstanceRequestId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , sitSpotPrice :: Maybe Text
      -- ^ FIXME: Missing documentation
    , sitState :: Maybe SpotInstanceState
      -- ^ FIXME: Missing documentation
    , sitStatus :: Maybe SpotInstanceStatus
      -- ^ FIXME: Missing documentation
    , sitTags :: [Tag]
      -- ^ A list of tags for this spot instance request.
    , sitType :: Maybe SpotInstanceType
      -- ^ FIXME: Missing documentation
    , sitValidFrom :: Maybe UTCTime
      -- ^ FIXME: Missing documentation
    , sitValidUntil :: Maybe UTCTime
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery SpotInstanceRequest

instance FromXML SpotInstanceRequest where
    fromXMLOptions = xmlOptions

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

-- | Represents a snapshot of an Amazon EC2 EBS volume.
data Snapshot = Snapshot
    { uDescription :: Maybe Text
      -- ^ Description of the snapshot.
    , uOwnerAlias :: Maybe Text
      -- ^ The AWS account alias (e.g., "amazon", "redhat", "self", etc.) or AWS
      -- account ID that owns the AMI.
    , uOwnerId :: Maybe Text
      -- ^ AWS Access Key ID of the user who owns the snapshot.
    , uProgress :: Maybe Text
      -- ^ The progress of the snapshot, in percentage.
    , uSnapshotId :: Maybe Text
      -- ^ The unique ID of this snapshot.
    , uStartTime :: Maybe UTCTime
      -- ^ Time stamp when the snapshot was initiated.
    , uState :: Maybe SnapshotState
      -- ^ Snapshot state (e.g., pending, completed, or error).
    , uTags :: [Tag]
      -- ^ A list of tags for the Snapshot.
    , uVolumeId :: Maybe Text
      -- ^ The ID of the volume from which this snapshot was created.
    , uVolumeSize :: Maybe Int
      -- ^ The size of the volume, in gigabytes.
    } deriving (Eq, Show, Generic)

instance ToQuery Snapshot

instance FromXML Snapshot where
    fromXMLOptions = xmlOptions

-- | An Amazon EC2 security group, describing how EC2 instances in this group
-- can receive network traffic.
data SecurityGroup = SecurityGroup
    { sgDescription :: Maybe Text
      -- ^ The description of this security group.
    , sgGroupId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , sgGroupName :: Maybe Text
      -- ^ The name of this security group.
    , sgIpPermissions :: [IpPermission]
      -- ^ The permissions enabled for this security group.
    , sgIpPermissionsEgress :: [IpPermission]
      -- ^ FIXME: Missing documentation
    , sgOwnerId :: Maybe Text
      -- ^ The AWS Access Key ID of the owner of the security group.
    , sgTags :: [Tag]
      -- ^ FIXME: Missing documentation
    , sgVpcId :: Maybe Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery SecurityGroup

instance FromXML SecurityGroup where
    fromXMLOptions = xmlOptions

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

-- | Enables monitoring for the instance.
newtype RunInstancesMonitoringEnabled = RunInstancesMonitoringEnabled
    { rimeEnabled :: Bool
      -- ^ FIXME: Type documentation for Bool
    } deriving (Eq, Show, Generic)

instance ToQuery RunInstancesMonitoringEnabled

instance FromXML RunInstancesMonitoringEnabled where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for RouteTableAssociation
data RouteTableAssociation = RouteTableAssociation
    { rtaMain :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , rtaRouteTableAssociationId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , rtaRouteTableId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , rtaSubnetId :: Maybe Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery RouteTableAssociation

instance FromXML RouteTableAssociation where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for RouteTable
data RouteTable = RouteTable
    { rtAssociations :: [RouteTableAssociation]
      -- ^ FIXME: Missing documentation
    , rtPropagatingVgws :: [PropagatingVgw]
      -- ^ FIXME: Missing documentation
    , rtRouteTableId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , rtRoutes :: [Route]
      -- ^ FIXME: Missing documentation
    , rtTags :: [Tag]
      -- ^ FIXME: Missing documentation
    , rtVpcId :: Maybe Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery RouteTable

instance FromXML RouteTable where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for Route
data Route = Route
    { tDestinationCidrBlock :: Maybe Text
      -- ^ FIXME: Missing documentation
    , tGatewayId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , tInstanceId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , tInstanceOwnerId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , tNetworkInterfaceId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , tState :: Maybe RouteState
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery Route

instance FromXML Route where
    fromXMLOptions = xmlOptions

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
      -- ^ FIXME: Missing documentation
    , rioOfferingType :: Maybe OfferingTypeValues
      -- ^ The Reserved Instance offering type.
    , rioPricingDetails :: [PricingDetail]
      -- ^ FIXME: Missing documentation
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

-- | Information about a specific modification request to your Reserved
-- Instances.
data ReservedInstancesModification = ReservedInstancesModification
    { rimClientToken :: Maybe Text
      -- ^ The idempotency token for the modification request.
    , rimCreateDate :: Maybe UTCTime
      -- ^ The time the modification request was created.
    , rimEffectiveDate :: Maybe UTCTime
      -- ^ The time the modification becomes effective.
    , rimModificationResults :: [ReservedInstancesModificationResult]
      -- ^ The resulting information about the modified Reserved Instances.
    , rimReservedInstancesIds :: [ReservedInstancesId]
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

-- | FIXME: Type documentation for ReservedInstancesListing
data ReservedInstancesListing = ReservedInstancesListing
    { rilClientToken :: Maybe Text
      -- ^ FIXME: Missing documentation
    , rilCreateDate :: Maybe UTCTime
      -- ^ FIXME: Missing documentation
    , rilInstanceCounts :: [InstanceCount]
      -- ^ FIXME: Missing documentation
    , rilPriceSchedules :: [PriceSchedule]
      -- ^ FIXME: Missing documentation
    , rilReservedInstancesId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , rilReservedInstancesListingId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , rilStatus :: Maybe ListingStatus
      -- ^ FIXME: Missing documentation
    , rilStatusMessage :: Maybe Text
      -- ^ FIXME: Missing documentation
    , rilTags :: [Tag]
      -- ^ FIXME: Missing documentation
    , rilUpdateDate :: Maybe UTCTime
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery ReservedInstancesListing

instance FromXML ReservedInstancesListing where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for ReservedInstancesId
newtype ReservedInstancesId = ReservedInstancesId
    { riiReservedInstancesId :: Text
      -- ^ FIXME: Type documentation for Text
    } deriving (Eq, Show, Generic)

instance ToQuery ReservedInstancesId

instance FromXML ReservedInstancesId where
    fromXMLOptions = xmlOptions

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

-- | A group of Amazon EC2 Reserved Instances purchased by this account.
data ReservedInstances = ReservedInstances
    { riAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone in which the Reserved Instances can be used.
    , riCurrencyCode :: Maybe CurrencyCodeValues
      -- ^ The currency of the reserved instance. Specified using ISO 4217 standard
      -- (e.g., USD, JPY).
    , riDuration :: Maybe Integer
      -- ^ The duration of the Reserved Instances, in seconds.
    , riEnd :: Maybe UTCTime
      -- ^ FIXME: Missing documentation
    , riFixedPrice :: Maybe Double
      -- ^ The purchase price of the Reserved Instances.
    , riInstanceCount :: Maybe Int
      -- ^ The number of Reserved Instances purchased.
    , riInstanceTenancy :: Maybe Tenancy
      -- ^ The tenancy of the reserved instance (ex: default or dedicated).
    , riInstanceType :: Maybe InstanceType
      -- ^ The instance type on which the Reserved Instances can be used.
    , riOfferingType :: Maybe OfferingTypeValues
      -- ^ The Reserved Instance offering type.
    , riProductDescription :: Maybe RIProductDescription
      -- ^ The Reserved Instances product description (ex: Windows or Unix/Linux).
    , riRecurringCharges :: [RecurringCharge]
      -- ^ The recurring charge tag assigned to the resource.
    , riReservedInstancesId :: Maybe Text
      -- ^ The unique ID of the Reserved Instances purchase.
    , riStart :: Maybe UTCTime
      -- ^ The date and time the Reserved Instances started.
    , riState :: Maybe ReservedInstanceState
      -- ^ The state of the Reserved Instances purchase.
    , riTags :: [Tag]
      -- ^ A list of tags for the ReservedInstances.
    , riUsagePrice :: Maybe Double
      -- ^ The usage price of the Reserved Instances, per hour.
    } deriving (Eq, Show, Generic)

instance ToQuery ReservedInstances

instance FromXML ReservedInstances where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for ReservedInstanceLimitPrice
data ReservedInstanceLimitPrice = ReservedInstanceLimitPrice
    { rilpAmount :: Maybe Double
      -- ^ FIXME: Missing documentation
    , rilpCurrencyCode :: Maybe CurrencyCodeValues
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery ReservedInstanceLimitPrice

instance FromXML ReservedInstanceLimitPrice where
    fromXMLOptions = xmlOptions

-- | An Amazon EC2 reservation of requested EC2 instances.
data Reservation = Reservation
    { sGroups :: [GroupIdentifier]
      -- ^ The list of security groups requested for the instances in this
      -- reservation.
    , sInstances :: [Instance]
      -- ^ The list of Amazon EC2 instances included in this reservation.
    , sOwnerId :: Maybe Text
      -- ^ The AWS Access Key ID of the user who owns the reservation.
    , sRequesterId :: Maybe Text
      -- ^ The unique ID of the user who requested the instances in this reservation.
    , sReservationId :: Maybe Text
      -- ^ The unique ID of this reservation.
    } deriving (Eq, Show, Generic)

instance ToQuery Reservation

instance FromXML Reservation where
    fromXMLOptions = xmlOptions

-- | Represents an Amazon EC2 region. EC2 regions are completely isolated from
-- each other.
data Region = Region
    { rEndpoint :: Maybe Text
      -- ^ Region service endpoint.
    , rRegionName :: Maybe Text
      -- ^ Name of the region.
    } deriving (Eq, Show, Generic)

instance ToQuery Region

instance FromXML Region where
    fromXMLOptions = xmlOptions

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

-- | FIXME: Type documentation for PropagatingVgw
newtype PropagatingVgw = PropagatingVgw
    { pvGatewayId :: Text
      -- ^ FIXME: Type documentation for Text
    } deriving (Eq, Show, Generic)

instance ToQuery PropagatingVgw

instance FromXML PropagatingVgw where
    fromXMLOptions = xmlOptions

-- | An AWS DevPay product code.
data ProductCode = ProductCode
    { pcProductCodeId :: Maybe Text
      -- ^ The unique ID of an AWS DevPay product code.
    , pcProductCodeType :: Maybe ProductCodeValues
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery ProductCode

instance FromXML ProductCode where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for PrivateIpAddressSpecification
data PrivateIpAddressSpecification = PrivateIpAddressSpecification
    { piasPrimary :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , piasPrivateIpAddress :: !Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery PrivateIpAddressSpecification

instance FromXML PrivateIpAddressSpecification where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for PricingDetail
data PricingDetail = PricingDetail
    { pdCount :: Maybe Int
      -- ^ FIXME: Missing documentation
    , pdPrice :: Maybe Double
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery PricingDetail

instance FromXML PricingDetail where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for PriceScheduleSpecification
data PriceScheduleSpecification = PriceScheduleSpecification
    { pssCurrencyCode :: Maybe CurrencyCodeValues
      -- ^ FIXME: Missing documentation
    , pssPrice :: Maybe Double
      -- ^ FIXME: Missing documentation
    , pssTerm :: Maybe Integer
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery PriceScheduleSpecification

instance FromXML PriceScheduleSpecification where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for PriceSchedule
data PriceSchedule = PriceSchedule
    { psActive :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , psCurrencyCode :: Maybe CurrencyCodeValues
      -- ^ FIXME: Missing documentation
    , psPrice :: Maybe Double
      -- ^ FIXME: Missing documentation
    , psTerm :: Maybe Integer
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery PriceSchedule

instance FromXML PriceSchedule where
    fromXMLOptions = xmlOptions

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

-- | FIXME: Type documentation for NetworkInterfacePrivateIpAddress
data NetworkInterfacePrivateIpAddress = NetworkInterfacePrivateIpAddress
    { nipiaAssociation :: Maybe NetworkInterfaceAssociation
      -- ^ FIXME: Missing documentation
    , nipiaPrimary :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , nipiaPrivateDnsName :: Maybe Text
      -- ^ FIXME: Missing documentation
    , nipiaPrivateIpAddress :: Maybe Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery NetworkInterfacePrivateIpAddress

instance FromXML NetworkInterfacePrivateIpAddress where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for NetworkInterfaceAttachmentChanges
data NetworkInterfaceAttachmentChanges = NetworkInterfaceAttachmentChanges
    { niacAttachmentId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , niacDeleteOnTermination :: Maybe Bool
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery NetworkInterfaceAttachmentChanges

instance FromXML NetworkInterfaceAttachmentChanges where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for NetworkInterfaceAttachment
data NetworkInterfaceAttachment = NetworkInterfaceAttachment
    { nibAttachTime :: Maybe UTCTime
      -- ^ FIXME: Missing documentation
    , nibAttachmentId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , nibDeleteOnTermination :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , nibDeviceIndex :: Maybe Int
      -- ^ FIXME: Missing documentation
    , nibInstanceId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , nibInstanceOwnerId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , nibStatus :: Maybe AttachmentStatus
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery NetworkInterfaceAttachment

instance FromXML NetworkInterfaceAttachment where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for NetworkInterfaceAssociation
data NetworkInterfaceAssociation = NetworkInterfaceAssociation
    { niaAllocationId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , niaAssociationId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , niaIpOwnerId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , niaPublicIp :: Maybe Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery NetworkInterfaceAssociation

instance FromXML NetworkInterfaceAssociation where
    fromXMLOptions = xmlOptions

-- | Specifies the characteristics of a network interface.
data NetworkInterface = NetworkInterface
    { niAssociation :: Maybe NetworkInterfaceAssociation
      -- ^ FIXME: Missing documentation
    , niAttachment :: Maybe NetworkInterfaceAttachment
      -- ^ FIXME: Missing documentation
    , niAvailabilityZone :: Maybe Text
      -- ^ FIXME: Missing documentation
    , niDescription :: Maybe Text
      -- ^ FIXME: Missing documentation
    , niGroups :: [GroupIdentifier]
      -- ^ FIXME: Missing documentation
    , niMacAddress :: Maybe Text
      -- ^ FIXME: Missing documentation
    , niNetworkInterfaceId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , niOwnerId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , niPrivateDnsName :: Maybe Text
      -- ^ FIXME: Missing documentation
    , niPrivateIpAddress :: Maybe Text
      -- ^ FIXME: Missing documentation
    , niPrivateIpAddresses :: [NetworkInterfacePrivateIpAddress]
      -- ^ FIXME: Missing documentation
    , niRequesterId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , niRequesterManaged :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , niSourceDestCheck :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , niStatus :: Maybe NetworkInterfaceStatus
      -- ^ FIXME: Missing documentation
    , niSubnetId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , niTagSet :: [Tag]
      -- ^ FIXME: Missing documentation
    , niVpcId :: Maybe Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery NetworkInterface

instance FromXML NetworkInterface where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for NetworkAclEntry
data NetworkAclEntry = NetworkAclEntry
    { naeCidrBlock :: Maybe Text
      -- ^ FIXME: Missing documentation
    , naeEgress :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , naeIcmpTypeCode :: Maybe IcmpTypeCode
      -- ^ FIXME: Missing documentation
    , naePortRange :: Maybe PortRange
      -- ^ FIXME: Missing documentation
    , naeProtocol :: Maybe Text
      -- ^ FIXME: Missing documentation
    , naeRuleAction :: Maybe RuleAction
      -- ^ FIXME: Missing documentation
    , naeRuleNumber :: Maybe Int
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery NetworkAclEntry

instance FromXML NetworkAclEntry where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for NetworkAclAssociation
data NetworkAclAssociation = NetworkAclAssociation
    { naaNetworkAclAssociationId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , naaNetworkAclId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , naaSubnetId :: Maybe Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery NetworkAclAssociation

instance FromXML NetworkAclAssociation where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for NetworkAcl
data NetworkAcl = NetworkAcl
    { naAssociations :: [NetworkAclAssociation]
      -- ^ FIXME: Missing documentation
    , naEntries :: [NetworkAclEntry]
      -- ^ FIXME: Missing documentation
    , naIsDefault :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , naNetworkAclId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , naTags :: [Tag]
      -- ^ FIXME: Missing documentation
    , naVpcId :: Maybe Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery NetworkAcl

instance FromXML NetworkAcl where
    fromXMLOptions = xmlOptions

-- | Monitoring status for this instance.
newtype Monitoring = Monitoring
    { mState :: MonitoringState
      -- ^ The state of monitoring on an Amazon EC2 instance (ex: enabled, disabled).
    } deriving (Eq, Show, Generic)

instance ToQuery Monitoring

instance FromXML Monitoring where
    fromXMLOptions = xmlOptions

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

-- | A software license that can be associated with an Amazon EC2 instance when
-- launched (ex. a Microsoft Windows license).
data License = License
    { lCapacities :: [LicenseCapacity]
      -- ^ The capacities available for this license, indicating how many licenses are
      -- in use, how many are available, how many Amazon EC2 instances can be
      -- supported, etc.
    , lLicenseId :: Maybe Text
      -- ^ The unique ID identifying the license.
    , lPool :: Maybe Text
      -- ^ The name of the pool in which the license is kept.
    , lTags :: [Tag]
      -- ^ A list of tags for the License.
    , lType :: Maybe Text
      -- ^ The license type (ex. "Microsoft/Windows/Standard").
    } deriving (Eq, Show, Generic)

instance ToQuery License

instance FromXML License where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for LaunchSpecificationMonitoring
newtype LaunchSpecificationMonitoring = LaunchSpecificationMonitoring
    { lsmEnabled :: Bool
      -- ^ Enables monitoring for the instance.
    } deriving (Eq, Show, Generic)

instance ToQuery LaunchSpecificationMonitoring

instance FromXML LaunchSpecificationMonitoring where
    fromXMLOptions = xmlOptions

-- | Specifies additional launch instance information.
data LaunchSpecification = LaunchSpecification
    { lsAddressingType :: Maybe Text
      -- ^ Deprecated.
    , lsBlockDeviceMappings :: [BlockDeviceMapping]
      -- ^ Specifies how block devices are exposed to the instance. Each mapping is
      -- made up of a virtualName and a deviceName.
    , lsEbsOptimized :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , lsIamInstanceProfile :: Maybe IamInstanceProfileSpecification
      -- ^ FIXME: Missing documentation
    , lsImageId :: Maybe Text
      -- ^ The AMI ID.
    , lsInstanceType :: Maybe InstanceType
      -- ^ Specifies the instance type.
    , lsKernelId :: Maybe Text
      -- ^ Specifies the ID of the kernel to select.
    , lsKeyName :: Maybe Text
      -- ^ The name of the key pair.
    , lsMonitoring :: Maybe LaunchSpecificationMonitoring
      -- ^ FIXME: Missing documentation
    , lsNetworkInterfaces :: [InstanceNetworkInterfaceSpecification]
      -- ^ FIXME: Missing documentation
    , lsPlacement :: Maybe SpotPlacement
      -- ^ Defines a placement item.
    , lsRamdiskId :: Maybe Text
      -- ^ Specifies the ID of the RAM disk to select. Some kernels require additional
      -- drivers at launch. Check the kernel requirements for information on whether
      -- or not you need to specify a RAM disk and search for the kernel ID.
    , lsSecurityGroupIds :: [Text]
      -- ^ FIXME: Missing documentation
    , lsSecurityGroups :: [Text]
      -- ^ FIXME: Missing documentation
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

-- | FIXME: Type documentation for LaunchPermissionModifications
data LaunchPermissionModifications = LaunchPermissionModifications
    { lpmAdd :: [LaunchPermission]
      -- ^ FIXME: Missing documentation
    , lpmRemove :: [LaunchPermission]
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery LaunchPermissionModifications

instance FromXML LaunchPermissionModifications where
    fromXMLOptions = xmlOptions

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

-- | Contains a list of CIDR IP ranges.
newtype IpRange = IpRange
    { irCidrIp :: Text
      -- ^ The list of CIDR IP ranges.
    } deriving (Eq, Show, Generic)

instance ToQuery IpRange

instance FromXML IpRange where
    fromXMLOptions = xmlOptions

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

-- | FIXME: Type documentation for InternetGatewayAttachment
data InternetGatewayAttachment = InternetGatewayAttachment
    { igaState :: Maybe AttachmentStatus
      -- ^ FIXME: Missing documentation
    , igaVpcId :: Maybe Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery InternetGatewayAttachment

instance FromXML InternetGatewayAttachment where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for InternetGateway
data InternetGateway = InternetGateway
    { igAttachments :: [InternetGatewayAttachment]
      -- ^ FIXME: Missing documentation
    , igInternetGatewayId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , igTags :: [Tag]
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery InternetGateway

instance FromXML InternetGateway where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for InstanceStatusSummary
data InstanceStatusSummary = InstanceStatusSummary
    { issDetails :: [InstanceStatusDetails]
      -- ^ FIXME: Missing documentation
    , issStatus :: Maybe SummaryStatus
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery InstanceStatusSummary

instance FromXML InstanceStatusSummary where
    fromXMLOptions = xmlOptions

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

-- | FIXME: Type documentation for InstanceStatusDetails
data InstanceStatusDetails = InstanceStatusDetails
    { isdImpairedSince :: Maybe UTCTime
      -- ^ FIXME: Missing documentation
    , isdName :: Maybe StatusName
      -- ^ FIXME: Missing documentation
    , isdStatus :: Maybe StatusType
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery InstanceStatusDetails

instance FromXML InstanceStatusDetails where
    fromXMLOptions = xmlOptions

-- | Represents the status of an Amazon EC2 instance.
data InstanceStatus = InstanceStatus
    { itAvailabilityZone :: Maybe Text
      -- ^ The Amazon EC2 instance's availability zone.
    , itEvents :: [InstanceStatusEvent]
      -- ^ Events that affect the status of the associated Amazon EC2 instance.
    , itInstanceId :: Maybe Text
      -- ^ The ID of the Amazon EC2 instance.
    , itInstanceState :: Maybe InstanceState
      -- ^ Represents the state of an Amazon EC2 instance.
    , itInstanceStatus :: Maybe InstanceStatusSummary
      -- ^ FIXME: Missing documentation
    , itSystemStatus :: Maybe InstanceStatusSummary
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery InstanceStatus

instance FromXML InstanceStatus where
    fromXMLOptions = xmlOptions

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

-- | FIXME: Type documentation for InstancePrivateIpAddress
data InstancePrivateIpAddress = InstancePrivateIpAddress
    { ipiaAssociation :: Maybe InstanceNetworkInterfaceAssociation
      -- ^ FIXME: Missing documentation
    , ipiaPrimary :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , ipiaPrivateDnsName :: Maybe Text
      -- ^ FIXME: Missing documentation
    , ipiaPrivateIpAddress :: Maybe Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery InstancePrivateIpAddress

instance FromXML InstancePrivateIpAddress where
    fromXMLOptions = xmlOptions

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
      -- ^ FIXME: Missing documentation
    , inisDescription :: Maybe Text
      -- ^ A description. Applies only when creating a network interface.
    , inisDeviceIndex :: Maybe Int
      -- ^ The device index. Applies to both attaching an existing network interface
      -- and when creating a network interface. Condition: If you are specifying a
      -- network interface in the request, you must provide the device index.
    , inisGroups :: [Text]
      -- ^ FIXME: Missing documentation
    , inisNetworkInterfaceId :: Maybe Text
      -- ^ An existing interface to attach to a single instance. Requires n=1
      -- instances.
    , inisPrivateIpAddress :: Maybe Text
      -- ^ The primary private IP address. Applies only when creating a network
      -- interface. Requires n=1 network interfaces in launch.
    , inisPrivateIpAddresses :: [PrivateIpAddressSpecification]
      -- ^ FIXME: Missing documentation
    , inisSecondaryPrivateIpAddressCount :: Maybe Int
      -- ^ FIXME: Missing documentation
    , inisSubnetId :: Maybe Text
      -- ^ The subnet ID. Applies only when creating a network interface.
    } deriving (Eq, Show, Generic)

instance ToQuery InstanceNetworkInterfaceSpecification

instance FromXML InstanceNetworkInterfaceSpecification where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for InstanceNetworkInterfaceAttachment
data InstanceNetworkInterfaceAttachment = InstanceNetworkInterfaceAttachment
    { inibAttachTime :: Maybe UTCTime
      -- ^ FIXME: Missing documentation
    , inibAttachmentId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , inibDeleteOnTermination :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , inibDeviceIndex :: Maybe Int
      -- ^ FIXME: Missing documentation
    , inibStatus :: Maybe AttachmentStatus
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery InstanceNetworkInterfaceAttachment

instance FromXML InstanceNetworkInterfaceAttachment where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for InstanceNetworkInterfaceAssociation
data InstanceNetworkInterfaceAssociation = InstanceNetworkInterfaceAssociation
    { iniaIpOwnerId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , iniaPublicDnsName :: Maybe Text
      -- ^ FIXME: Missing documentation
    , iniaPublicIp :: Maybe Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery InstanceNetworkInterfaceAssociation

instance FromXML InstanceNetworkInterfaceAssociation where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for InstanceNetworkInterface
data InstanceNetworkInterface = InstanceNetworkInterface
    { iniAssociation :: Maybe InstanceNetworkInterfaceAssociation
      -- ^ FIXME: Missing documentation
    , iniAttachment :: Maybe InstanceNetworkInterfaceAttachment
      -- ^ FIXME: Missing documentation
    , iniDescription :: Maybe Text
      -- ^ FIXME: Missing documentation
    , iniGroups :: [GroupIdentifier]
      -- ^ FIXME: Missing documentation
    , iniNetworkInterfaceId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , iniOwnerId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , iniPrivateDnsName :: Maybe Text
      -- ^ FIXME: Missing documentation
    , iniPrivateIpAddress :: Maybe Text
      -- ^ FIXME: Missing documentation
    , iniPrivateIpAddresses :: [InstancePrivateIpAddress]
      -- ^ FIXME: Missing documentation
    , iniSourceDestCheck :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , iniStatus :: Maybe NetworkInterfaceStatus
      -- ^ FIXME: Missing documentation
    , iniSubnetId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , iniVpcId :: Maybe Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery InstanceNetworkInterface

instance FromXML InstanceNetworkInterface where
    fromXMLOptions = xmlOptions

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

-- | Specifies active licenses in use and attached to an Amazon EC2 instance.
newtype InstanceLicenseSpecification = InstanceLicenseSpecification
    { ilsPool :: Text
      -- ^ The license pool from which to take a license when starting Amazon EC2
      -- instances in the associated RunInstances request.
    } deriving (Eq, Show, Generic)

instance ToQuery InstanceLicenseSpecification

instance FromXML InstanceLicenseSpecification where
    fromXMLOptions = xmlOptions

-- | Represents an active license in use and attached to an Amazon EC2 instance.
newtype InstanceLicense = InstanceLicense
    { ilPool :: Text
      -- ^ The license pool from which this license was used (ex: 'windows').
    } deriving (Eq, Show, Generic)

instance ToQuery InstanceLicense

instance FromXML InstanceLicense where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for InstanceExportDetails
data InstanceExportDetails = InstanceExportDetails
    { iedInstanceId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , iedTargetEnvironment :: Maybe ExportEnvironment
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery InstanceExportDetails

instance FromXML InstanceExportDetails where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for InstanceCount
data InstanceCount = InstanceCount
    { icInstanceCount :: Maybe Int
      -- ^ FIXME: Missing documentation
    , icState :: Maybe ListingState
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery InstanceCount

instance FromXML InstanceCount where
    fromXMLOptions = xmlOptions

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

-- | Represents an Amazon EC2 instance.
data Instance = Instance
    { jAmiLaunchIndex :: Maybe Int
      -- ^ The AMI launch index, which can be used to find this instance within the
      -- launch group.
    , jArchitecture :: Maybe ArchitectureValues
      -- ^ The architecture of this instance.
    , jBlockDeviceMappings :: [InstanceBlockDeviceMapping]
      -- ^ Block device mapping set.
    , jClientToken :: Maybe Text
      -- ^ FIXME: Missing documentation
    , jEbsOptimized :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , jHypervisor :: Maybe HypervisorType
      -- ^ FIXME: Missing documentation
    , jIamInstanceProfile :: Maybe IamInstanceProfile
      -- ^ FIXME: Missing documentation
    , jImageId :: Maybe Text
      -- ^ Image ID of the AMI used to launch the instance.
    , jInstanceId :: Maybe Text
      -- ^ Unique ID of the instance launched.
    , jInstanceLifecycle :: Maybe InstanceLifecycleType
      -- ^ FIXME: Missing documentation
    , jInstanceType :: Maybe InstanceType
      -- ^ The instance type. For more information on instance types, please see the
      -- Amazon Elastic Compute Cloud Developer Guide.
    , jKernelId :: Maybe Text
      -- ^ Kernel associated with this instance.
    , jKeyName :: Maybe Text
      -- ^ If this instance was launched with an associated key pair, this displays
      -- the key pair name.
    , jLaunchTime :: Maybe UTCTime
      -- ^ The time this instance launched.
    , jLicense :: Maybe InstanceLicense
      -- ^ Represents an active license in use and attached to an Amazon EC2 instance.
    , jMonitoring :: Maybe Monitoring
      -- ^ Monitoring status for this instance.
    , jNetworkInterfaces :: [InstanceNetworkInterface]
      -- ^ FIXME: Missing documentation
    , jPlacement :: Maybe Placement
      -- ^ The location where this instance launched.
    , jPlatform :: Maybe PlatformValues
      -- ^ Platform of the instance (e.g., Windows).
    , jPrivateDnsName :: Maybe Text
      -- ^ The private DNS name assigned to the instance. This DNS name can only be
      -- used inside the Amazon EC2 network. This element remains empty until the
      -- instance enters a running state.
    , jPrivateIpAddress :: Maybe Text
      -- ^ Specifies the private IP address that is assigned to the instance (Amazon
      -- VPC).
    , jProductCodes :: [ProductCode]
      -- ^ Product codes attached to this instance.
    , jPublicDnsName :: Maybe Text
      -- ^ The public DNS name assigned to the instance. This DNS name is contactable
      -- from outside the Amazon EC2 network. This element remains empty until the
      -- instance enters a running state.
    , jPublicIpAddress :: Maybe Text
      -- ^ Specifies the IP address of the instance.
    , jRamdiskId :: Maybe Text
      -- ^ RAM disk associated with this instance.
    , jRootDeviceName :: Maybe Text
      -- ^ The root device name (e.g., /dev/sda1).
    , jRootDeviceType :: Maybe DeviceType
      -- ^ The root device type used by the AMI. The AMI can use an Amazon EBS or
      -- instance store root device.
    , jSecurityGroups :: [GroupIdentifier]
      -- ^ FIXME: Missing documentation
    , jSourceDestCheck :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , jSpotInstanceRequestId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , jSriovNetSupport :: Maybe Text
      -- ^ FIXME: Missing documentation
    , jState :: Maybe InstanceState
      -- ^ The current state of the instance.
    , jStateReason :: Maybe StateReason
      -- ^ The reason for the state change.
    , jStateTransitionReason :: Maybe Text
      -- ^ Reason for the most recent state transition. This might be an empty string.
    , jSubnetId :: Maybe Text
      -- ^ Specifies the Amazon VPC subnet ID in which the instance is running.
    , jTags :: [Tag]
      -- ^ A list of tags for the Instance.
    , jVirtualizationType :: Maybe VirtualizationType
      -- ^ FIXME: Missing documentation
    , jVpcId :: Maybe Text
      -- ^ Specifies the Amazon VPC in which the instance is running.
    } deriving (Eq, Show, Generic)

instance ToQuery Instance

instance FromXML Instance where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for ImportVolumeTaskDetails
data ImportVolumeTaskDetails = ImportVolumeTaskDetails
    { ivtdAvailabilityZone :: !Text
      -- ^ FIXME: Missing documentation
    , ivtdBytesConverted :: !Integer
      -- ^ FIXME: Missing documentation
    , ivtdDescription :: Maybe Text
      -- ^ FIXME: Missing documentation
    , ivtdImage :: !DiskImageDescription
      -- ^ FIXME: Missing documentation
    , ivtdVolume :: !DiskImageVolumeDescription
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery ImportVolumeTaskDetails

instance FromXML ImportVolumeTaskDetails where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for ImportInstanceVolumeDetailItem
data ImportInstanceVolumeDetailItem = ImportInstanceVolumeDetailItem
    { iivdiAvailabilityZone :: !Text
      -- ^ FIXME: Missing documentation
    , iivdiBytesConverted :: !Integer
      -- ^ FIXME: Missing documentation
    , iivdiDescription :: Maybe Text
      -- ^ FIXME: Missing documentation
    , iivdiImage :: !DiskImageDescription
      -- ^ FIXME: Missing documentation
    , iivdiStatus :: !Text
      -- ^ FIXME: Missing documentation
    , iivdiStatusMessage :: Maybe Text
      -- ^ FIXME: Missing documentation
    , iivdiVolume :: !DiskImageVolumeDescription
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery ImportInstanceVolumeDetailItem

instance FromXML ImportInstanceVolumeDetailItem where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for ImportInstanceTaskDetails
data ImportInstanceTaskDetails = ImportInstanceTaskDetails
    { iitdDescription :: Maybe Text
      -- ^ FIXME: Missing documentation
    , iitdInstanceId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , iitdPlatform :: Maybe PlatformValues
      -- ^ FIXME: Missing documentation
    , iitdVolumes :: [ImportInstanceVolumeDetailItem]
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery ImportInstanceTaskDetails

instance FromXML ImportInstanceTaskDetails where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for ImportInstanceLaunchSpecification
data ImportInstanceLaunchSpecification = ImportInstanceLaunchSpecification
    { iilsAdditionalInfo :: Maybe Text
      -- ^ FIXME: Missing documentation
    , iilsArchitecture :: Maybe ArchitectureValues
      -- ^ FIXME: Missing documentation
    , iilsGroupNames :: [Text]
      -- ^ FIXME: Missing documentation
    , iilsInstanceInitiatedShutdownBehavior :: Maybe ShutdownBehavior
      -- ^ FIXME: Missing documentation
    , iilsInstanceType :: Maybe InstanceType
      -- ^ FIXME: Missing documentation
    , iilsMonitoring :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , iilsPlacement :: Maybe Placement
      -- ^ Describes where an Amazon EC2 instance is running within an Amazon EC2
      -- region.
    , iilsPrivateIpAddress :: Maybe Text
      -- ^ FIXME: Missing documentation
    , iilsSubnetId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , iilsUserData :: Maybe Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery ImportInstanceLaunchSpecification

instance FromXML ImportInstanceLaunchSpecification where
    fromXMLOptions = xmlOptions

-- | Represents an Amazon Machine Image (AMI) that can be run on an Amazon EC2
-- instance.
data Image = Image
    { iArchitecture :: Maybe ArchitectureValues
      -- ^ The architecture of the image.
    , iBlockDeviceMappings :: [BlockDeviceMapping]
      -- ^ Specifies how block devices are exposed to the instance.
    , iDescription :: Maybe Text
      -- ^ The description of the AMI that was provided during image creation.
    , iHypervisor :: Maybe HypervisorType
      -- ^ FIXME: Missing documentation
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
    , iOwnerId :: Maybe Text
      -- ^ AWS Access Key ID of the image owner.
    , iPlatform :: Maybe PlatformValues
      -- ^ The operating platform of the AMI.
    , iProductCodes :: [ProductCode]
      -- ^ Product codes of the AMI.
    , iPublic :: Maybe Bool
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
      -- ^ FIXME: Missing documentation
    , iState :: Maybe ImageState
      -- ^ Current state of the AMI. If the operation returns available, the image is
      -- successfully registered and available for launching. If the operation
      -- returns deregistered, the image is deregistered and no longer available for
      -- launching.
    , iStateReason :: Maybe StateReason
      -- ^ The reason for the state change.
    , iTags :: [Tag]
      -- ^ A list of tags for the Image.
    , iVirtualizationType :: Maybe VirtualizationType
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery Image

instance FromXML Image where
    fromXMLOptions = xmlOptions

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

-- | FIXME: Type documentation for IamInstanceProfileSpecification
data IamInstanceProfileSpecification = IamInstanceProfileSpecification
    { iipsArn :: Maybe Text
      -- ^ FIXME: Missing documentation
    , iipsName :: Maybe Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery IamInstanceProfileSpecification

instance FromXML IamInstanceProfileSpecification where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for IamInstanceProfile
data IamInstanceProfile = IamInstanceProfile
    { iipArn :: Maybe Text
      -- ^ FIXME: Missing documentation
    , iipId :: Maybe Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery IamInstanceProfile

instance FromXML IamInstanceProfile where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for GroupIdentifier
data GroupIdentifier = GroupIdentifier
    { giGroupId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , giGroupName :: Maybe Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery GroupIdentifier

instance FromXML GroupIdentifier where
    fromXMLOptions = xmlOptions

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

-- | FIXME: Type documentation for ExportToS3TaskSpecification
data ExportToS3TaskSpecification = ExportToS3TaskSpecification
    { ets3tsContainerFormat :: Maybe ContainerFormat
      -- ^ FIXME: Missing documentation
    , ets3tsDiskImageFormat :: Maybe DiskImageFormat
      -- ^ FIXME: Missing documentation
    , ets3tsS3Bucket :: Maybe Text
      -- ^ FIXME: Missing documentation
    , ets3tsS3Prefix :: Maybe Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery ExportToS3TaskSpecification

instance FromXML ExportToS3TaskSpecification where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for ExportToS3Task
data ExportToS3Task = ExportToS3Task
    { ets3tContainerFormat :: Maybe ContainerFormat
      -- ^ FIXME: Missing documentation
    , ets3tDiskImageFormat :: Maybe DiskImageFormat
      -- ^ FIXME: Missing documentation
    , ets3tS3Bucket :: Maybe Text
      -- ^ FIXME: Missing documentation
    , ets3tS3Key :: Maybe Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery ExportToS3Task

instance FromXML ExportToS3Task where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for ExportTask
data ExportTask = ExportTask
    { etDescription :: Maybe Text
      -- ^ FIXME: Missing documentation
    , etExportTaskId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , etExportToS3Task :: Maybe ExportToS3Task
      -- ^ FIXME: Missing documentation
    , etInstanceExportDetails :: Maybe InstanceExportDetails
      -- ^ FIXME: Missing documentation
    , etState :: Maybe ExportTaskState
      -- ^ FIXME: Missing documentation
    , etStatusMessage :: Maybe Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery ExportTask

instance FromXML ExportTask where
    fromXMLOptions = xmlOptions

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

-- | Specifies parameters used to automatically setup Amazon EBS volumes when
-- the instance is launched.
data EbsBlockDevice = EbsBlockDevice
    { ebdDeleteOnTermination :: Maybe Bool
      -- ^ Specifies whether the Amazon EBS volume is deleted on instance termination.
    , ebdIops :: Maybe Int
      -- ^ FIXME: Missing documentation
    , ebdSnapshotId :: Maybe Text
      -- ^ The ID of the snapshot from which the volume will be created.
    , ebdVolumeSize :: Maybe Int
      -- ^ The size of the volume, in gigabytes.
    , ebdVolumeType :: Maybe VolumeType
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery EbsBlockDevice

instance FromXML EbsBlockDevice where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for DiskImageVolumeDescription
data DiskImageVolumeDescription = DiskImageVolumeDescription
    { divdId :: !Text
      -- ^ FIXME: Missing documentation
    , divdSize :: Maybe Integer
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery DiskImageVolumeDescription

instance FromXML DiskImageVolumeDescription where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for DiskImageDetail
data DiskImageDetail = DiskImageDetail
    { dieBytes :: !Integer
      -- ^ FIXME: Missing documentation
    , dieFormat :: !DiskImageFormat
      -- ^ FIXME: Missing documentation
    , dieImportManifestUrl :: !Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery DiskImageDetail

instance FromXML DiskImageDetail where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for DiskImageDescription
data DiskImageDescription = DiskImageDescription
    { didChecksum :: Maybe Text
      -- ^ FIXME: Missing documentation
    , didFormat :: !DiskImageFormat
      -- ^ FIXME: Missing documentation
    , didImportManifestUrl :: !Text
      -- ^ FIXME: Missing documentation
    , didSize :: !Integer
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery DiskImageDescription

instance FromXML DiskImageDescription where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for DiskImage
data DiskImage = DiskImage
    { diDescription :: Maybe Text
      -- ^ FIXME: Missing documentation
    , diImage :: Maybe DiskImageDetail
      -- ^ FIXME: Missing documentation
    , diVolume :: Maybe VolumeDetail
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery DiskImage

instance FromXML DiskImage where
    fromXMLOptions = xmlOptions

-- | The DhcpOptions data type.
data DhcpOptions = DhcpOptions
    { doDhcpConfigurations :: [DhcpConfiguration]
      -- ^ Contains information about the set of DHCP options.
    , doDhcpOptionsId :: Maybe Text
      -- ^ Specifies the ID of the set of DHCP options.
    , doTags :: [Tag]
      -- ^ A list of tags for the DhcpOptions.
    } deriving (Eq, Show, Generic)

instance ToQuery DhcpOptions

instance FromXML DhcpOptions where
    fromXMLOptions = xmlOptions

-- | The DhcpConfiguration data type.
data DhcpConfiguration = DhcpConfiguration
    { dcKey :: Maybe Text
      -- ^ Contains the name of a DHCP option.
    , dcValues :: [Text]
      -- ^ Contains a set of values for a DHCP option.
    } deriving (Eq, Show, Generic)

instance ToQuery DhcpConfiguration

instance FromXML DhcpConfiguration where
    fromXMLOptions = xmlOptions

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
    , cgTags :: [Tag]
      -- ^ A list of tags for the CustomerGateway.
    , cgType :: Maybe Text
      -- ^ Specifies the type of VPN connection the customer gateway supports.
    } deriving (Eq, Show, Generic)

instance ToQuery CustomerGateway

instance FromXML CustomerGateway where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for CreateVolumePermissionModifications
data CreateVolumePermissionModifications = CreateVolumePermissionModifications
    { cvpmAdd :: [CreateVolumePermission]
      -- ^ FIXME: Missing documentation
    , cvpmRemove :: [CreateVolumePermission]
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery CreateVolumePermissionModifications

instance FromXML CreateVolumePermissionModifications where
    fromXMLOptions = xmlOptions

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

-- | FIXME: Type documentation for ConversionTask
data ConversionTask = ConversionTask
    { ctConversionTaskId :: !Text
      -- ^ FIXME: Missing documentation
    , ctExpirationTime :: Maybe Text
      -- ^ FIXME: Missing documentation
    , ctImportInstance :: Maybe ImportInstanceTaskDetails
      -- ^ FIXME: Missing documentation
    , ctImportVolume :: Maybe ImportVolumeTaskDetails
      -- ^ FIXME: Missing documentation
    , ctState :: !ConversionTaskState
      -- ^ FIXME: Missing documentation
    , ctStatusMessage :: Maybe Text
      -- ^ FIXME: Missing documentation
    , ctTags :: [Tag]
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery ConversionTask

instance FromXML ConversionTask where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for CancelledSpotInstanceRequest
data CancelledSpotInstanceRequest = CancelledSpotInstanceRequest
    { csirSpotInstanceRequestId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , csirState :: Maybe CancelSpotInstanceRequestState
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery CancelledSpotInstanceRequest

instance FromXML CancelledSpotInstanceRequest where
    fromXMLOptions = xmlOptions

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

-- | The canceled bundle task.
data BundleTask = BundleTask
    { btBundleId :: Maybe Text
      -- ^ Unique identifier for this task.
    , btBundleTaskError :: Maybe BundleTaskError
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

-- | FIXME: Type documentation for AvailabilityZoneMessage
newtype AvailabilityZoneMessage = AvailabilityZoneMessage
    { azmMessage :: Text
      -- ^ FIXME: Type documentation for Text
    } deriving (Eq, Show, Generic)

instance ToQuery AvailabilityZoneMessage

instance FromXML AvailabilityZoneMessage where
    fromXMLOptions = xmlOptions

-- | An EC2 availability zone, separate and fault tolerant from other
-- availability zones.
data AvailabilityZone = AvailabilityZone
    { azMessages :: [AvailabilityZoneMessage]
      -- ^ A list of messages about the Availability Zone.
    , azRegionName :: Maybe Text
      -- ^ Name of the region in which this zone resides.
    , azState :: Maybe AvailabilityZoneState
      -- ^ State of the Availability Zone.
    , azZoneName :: Maybe Text
      -- ^ Name of the Availability Zone.
    } deriving (Eq, Show, Generic)

instance ToQuery AvailabilityZone

instance FromXML AvailabilityZone where
    fromXMLOptions = xmlOptions

-- | String value.
newtype AttributeValue = AttributeValue
    { avValue :: Text
      -- ^ String value.
    } deriving (Eq, Show, Generic)

instance ToQuery AttributeValue

instance FromXML AttributeValue where
    fromXMLOptions = xmlOptions

-- | Boolean value.
newtype AttributeBooleanValue = AttributeBooleanValue
    { abvValue :: Bool
      -- ^ Boolean value.
    } deriving (Eq, Show, Generic)

instance ToQuery AttributeBooleanValue

instance FromXML AttributeBooleanValue where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for Address
data Address = Address
    { aAllocationId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , aAssociationId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , aDomain :: Maybe DomainType
      -- ^ FIXME: Missing documentation
    , aInstanceId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , aNetworkInterfaceId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , aNetworkInterfaceOwnerId :: Maybe Text
      -- ^ FIXME: Missing documentation
    , aPrivateIpAddress :: Maybe Text
      -- ^ FIXME: Missing documentation
    , aPublicIp :: Maybe Text
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery Address

instance FromXML Address where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for AccountAttributeValue
newtype AccountAttributeValue = AccountAttributeValue
    { aavAttributeValue :: Text
      -- ^ FIXME: Type documentation for Text
    } deriving (Eq, Show, Generic)

instance ToQuery AccountAttributeValue

instance FromXML AccountAttributeValue where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for AccountAttribute
data AccountAttribute = AccountAttribute
    { aaAttributeName :: Maybe Text
      -- ^ FIXME: Missing documentation
    , aaAttributeValues :: [AccountAttributeValue]
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery AccountAttribute

instance FromXML AccountAttribute where
    fromXMLOptions = xmlOptions

-- | FIXME: Type documentation for VpnStaticRouteSource
data VpnStaticRouteSource
    = Static
      deriving (Eq, Ord, Generic)

instance FromText VpnStaticRouteSource where
    fromText "Static" = Right Static
    fromText e = failFromText $ "Unrecognised VpnStaticRouteSource: " <> e

instance Read VpnStaticRouteSource where
    readsPrec _ = readFromText

instance ToText VpnStaticRouteSource where
    toText Static = "Static"

instance Show VpnStaticRouteSource where
    show = showToText

instance ToQuery VpnStaticRouteSource where
    toQuery = primToQuery

instance FromXML VpnStaticRouteSource where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML VpnStaticRouteSource where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | Describes the current state of the VPN gateway. Valid values are pending,
-- available, deleting, and deleted.
data VpnState
    = Available
    | Deleted
    | Deleting
    | Pending
      deriving (Eq, Ord, Generic)

instance FromText VpnState where
    fromText "available" = Right Available
    fromText "deleted" = Right Deleted
    fromText "deleting" = Right Deleting
    fromText "pending" = Right Pending
    fromText e = failFromText $ "Unrecognised VpnState: " <> e

instance Read VpnState where
    readsPrec _ = readFromText

instance ToText VpnState where
    toText Available = "available"
    toText Deleted = "deleted"
    toText Deleting = "deleting"
    toText Pending = "pending"

instance Show VpnState where
    show = showToText

instance ToQuery VpnState where
    toQuery = primToQuery

instance FromXML VpnState where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML VpnState where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | Describes the current state of the VPC. The state of the subnet may be
-- either pending or available.
data VpcState
    = Available
    | Pending
      deriving (Eq, Ord, Generic)

instance FromText VpcState where
    fromText "available" = Right Available
    fromText "pending" = Right Pending
    fromText e = failFromText $ "Unrecognised VpcState: " <> e

instance Read VpcState where
    readsPrec _ = readFromText

instance ToText VpcState where
    toText Available = "available"
    toText Pending = "pending"

instance Show VpcState where
    show = showToText

instance ToQuery VpcState where
    toQuery = primToQuery

instance FromXML VpcState where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML VpcState where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for VpcAttributeName
data VpcAttributeName
    = Enablednshostnames
    | Enablednssupport
      deriving (Eq, Ord, Generic)

instance FromText VpcAttributeName where
    fromText "enableDnsHostnames" = Right Enablednshostnames
    fromText "enableDnsSupport" = Right Enablednssupport
    fromText e = failFromText $ "Unrecognised VpcAttributeName: " <> e

instance Read VpcAttributeName where
    readsPrec _ = readFromText

instance ToText VpcAttributeName where
    toText Enablednshostnames = "enableDnsHostnames"
    toText Enablednssupport = "enableDnsSupport"

instance Show VpcAttributeName where
    show = showToText

instance ToQuery VpcAttributeName where
    toQuery = primToQuery

instance FromXML VpcAttributeName where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML VpcAttributeName where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for VolumeType
data VolumeType
    = Io1
    | Standard
      deriving (Eq, Ord, Generic)

instance FromText VolumeType where
    fromText "io1" = Right Io1
    fromText "standard" = Right Standard
    fromText e = failFromText $ "Unrecognised VolumeType: " <> e

instance Read VolumeType where
    readsPrec _ = readFromText

instance ToText VolumeType where
    toText Io1 = "io1"
    toText Standard = "standard"

instance Show VolumeType where
    show = showToText

instance ToQuery VolumeType where
    toQuery = primToQuery

instance FromXML VolumeType where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML VolumeType where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for VolumeStatusName
data VolumeStatusName
    = IoEnabled
    | IoPerformance
      deriving (Eq, Ord, Generic)

instance FromText VolumeStatusName where
    fromText "io-enabled" = Right IoEnabled
    fromText "io-performance" = Right IoPerformance
    fromText e = failFromText $ "Unrecognised VolumeStatusName: " <> e

instance Read VolumeStatusName where
    readsPrec _ = readFromText

instance ToText VolumeStatusName where
    toText IoEnabled = "io-enabled"
    toText IoPerformance = "io-performance"

instance Show VolumeStatusName where
    show = showToText

instance ToQuery VolumeStatusName where
    toQuery = primToQuery

instance FromXML VolumeStatusName where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML VolumeStatusName where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for VolumeStatusInfoStatus
data VolumeStatusInfoStatus
    = Impaired
    | InsufficientData
    | Ok
      deriving (Eq, Ord, Generic)

instance FromText VolumeStatusInfoStatus where
    fromText "impaired" = Right Impaired
    fromText "insufficient-data" = Right InsufficientData
    fromText "ok" = Right Ok
    fromText e = failFromText $ "Unrecognised VolumeStatusInfoStatus: " <> e

instance Read VolumeStatusInfoStatus where
    readsPrec _ = readFromText

instance ToText VolumeStatusInfoStatus where
    toText Impaired = "impaired"
    toText InsufficientData = "insufficient-data"
    toText Ok = "ok"

instance Show VolumeStatusInfoStatus where
    show = showToText

instance ToQuery VolumeStatusInfoStatus where
    toQuery = primToQuery

instance FromXML VolumeStatusInfoStatus where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML VolumeStatusInfoStatus where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | State of this volume (e.g., creating, available).
data VolumeState
    = Available
    | Creating
    | Deleted
    | Deleting
    | Error
    | InUse
      deriving (Eq, Ord, Generic)

instance FromText VolumeState where
    fromText "available" = Right Available
    fromText "creating" = Right Creating
    fromText "deleted" = Right Deleted
    fromText "deleting" = Right Deleting
    fromText "error" = Right Error
    fromText "in-use" = Right InUse
    fromText e = failFromText $ "Unrecognised VolumeState: " <> e

instance Read VolumeState where
    readsPrec _ = readFromText

instance ToText VolumeState where
    toText Available = "available"
    toText Creating = "creating"
    toText Deleted = "deleted"
    toText Deleting = "deleting"
    toText Error = "error"
    toText InUse = "in-use"

instance Show VolumeState where
    show = showToText

instance ToQuery VolumeState where
    toQuery = primToQuery

instance FromXML VolumeState where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML VolumeState where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for VolumeAttributeName
data VolumeAttributeName
    = Autoenableio
    | Productcodes
      deriving (Eq, Ord, Generic)

instance FromText VolumeAttributeName where
    fromText "autoEnableIO" = Right Autoenableio
    fromText "productCodes" = Right Productcodes
    fromText e = failFromText $ "Unrecognised VolumeAttributeName: " <> e

instance Read VolumeAttributeName where
    readsPrec _ = readFromText

instance ToText VolumeAttributeName where
    toText Autoenableio = "autoEnableIO"
    toText Productcodes = "productCodes"

instance Show VolumeAttributeName where
    show = showToText

instance ToQuery VolumeAttributeName where
    toQuery = primToQuery

instance FromXML VolumeAttributeName where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML VolumeAttributeName where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for VolumeAttachmentState
data VolumeAttachmentState
    = Attached
    | Attaching
    | Detached
    | Detaching
      deriving (Eq, Ord, Generic)

instance FromText VolumeAttachmentState where
    fromText "attached" = Right Attached
    fromText "attaching" = Right Attaching
    fromText "detached" = Right Detached
    fromText "detaching" = Right Detaching
    fromText e = failFromText $ "Unrecognised VolumeAttachmentState: " <> e

instance Read VolumeAttachmentState where
    readsPrec _ = readFromText

instance ToText VolumeAttachmentState where
    toText Attached = "attached"
    toText Attaching = "attaching"
    toText Detached = "detached"
    toText Detaching = "detaching"

instance Show VolumeAttachmentState where
    show = showToText

instance ToQuery VolumeAttachmentState where
    toQuery = primToQuery

instance FromXML VolumeAttachmentState where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML VolumeAttachmentState where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for VirtualizationType
data VirtualizationType
    = Hvm
    | Paravirtual
      deriving (Eq, Ord, Generic)

instance FromText VirtualizationType where
    fromText "hvm" = Right Hvm
    fromText "paravirtual" = Right Paravirtual
    fromText e = failFromText $ "Unrecognised VirtualizationType: " <> e

instance Read VirtualizationType where
    readsPrec _ = readFromText

instance ToText VirtualizationType where
    toText Hvm = "hvm"
    toText Paravirtual = "paravirtual"

instance Show VirtualizationType where
    show = showToText

instance ToQuery VirtualizationType where
    toQuery = primToQuery

instance FromXML VirtualizationType where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML VirtualizationType where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | The allowed tenancy of instances launched into the VPC. A value of default
-- means instances can be launched with any tenancy; a value of dedicated
-- means all instances launched into the VPC will be launched as dedicated
-- tenancy regardless of the tenancy assigned to the instance at launch.
data Tenancy
    = Dedicated
    | Default
      deriving (Eq, Ord, Generic)

instance FromText Tenancy where
    fromText "dedicated" = Right Dedicated
    fromText "default" = Right Default
    fromText e = failFromText $ "Unrecognised Tenancy: " <> e

instance Read Tenancy where
    readsPrec _ = readFromText

instance ToText Tenancy where
    toText Dedicated = "dedicated"
    toText Default = "default"

instance Show Tenancy where
    show = showToText

instance ToQuery Tenancy where
    toQuery = primToQuery

instance FromXML Tenancy where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML Tenancy where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for TelemetryStatus
data TelemetryStatus
    = Down
    | Up
      deriving (Eq, Ord, Generic)

instance FromText TelemetryStatus where
    fromText "DOWN" = Right Down
    fromText "UP" = Right Up
    fromText e = failFromText $ "Unrecognised TelemetryStatus: " <> e

instance Read TelemetryStatus where
    readsPrec _ = readFromText

instance ToText TelemetryStatus where
    toText Down = "DOWN"
    toText Up = "UP"

instance Show TelemetryStatus where
    show = showToText

instance ToQuery TelemetryStatus where
    toQuery = primToQuery

instance FromXML TelemetryStatus where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML TelemetryStatus where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for SummaryStatus
data SummaryStatus
    = Impaired
    | InsufficientData
    | NotApplicable
    | Ok
      deriving (Eq, Ord, Generic)

instance FromText SummaryStatus where
    fromText "impaired" = Right Impaired
    fromText "insufficient-data" = Right InsufficientData
    fromText "not-applicable" = Right NotApplicable
    fromText "ok" = Right Ok
    fromText e = failFromText $ "Unrecognised SummaryStatus: " <> e

instance Read SummaryStatus where
    readsPrec _ = readFromText

instance ToText SummaryStatus where
    toText Impaired = "impaired"
    toText InsufficientData = "insufficient-data"
    toText NotApplicable = "not-applicable"
    toText Ok = "ok"

instance Show SummaryStatus where
    show = showToText

instance ToQuery SummaryStatus where
    toQuery = primToQuery

instance FromXML SummaryStatus where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML SummaryStatus where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | Describes the current state of the subnet. The state of the subnet may be
-- either pending or available.
data SubnetState
    = Available
    | Pending
      deriving (Eq, Ord, Generic)

instance FromText SubnetState where
    fromText "available" = Right Available
    fromText "pending" = Right Pending
    fromText e = failFromText $ "Unrecognised SubnetState: " <> e

instance Read SubnetState where
    readsPrec _ = readFromText

instance ToText SubnetState where
    toText Available = "available"
    toText Pending = "pending"

instance Show SubnetState where
    show = showToText

instance ToQuery SubnetState where
    toQuery = primToQuery

instance FromXML SubnetState where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML SubnetState where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | The type of virtualization.
data String
    = Hvm
    | Paravirtual
      deriving (Eq, Ord, Generic)

instance FromText String where
    fromText "hvm" = Right Hvm
    fromText "paravirtual" = Right Paravirtual
    fromText e = failFromText $ "Unrecognised String: " <> e

instance Read String where
    readsPrec _ = readFromText

instance ToText String where
    toText Hvm = "hvm"
    toText Paravirtual = "paravirtual"

instance Show String where
    show = showToText

instance ToQuery String where
    toQuery = primToQuery

instance FromXML String where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML String where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for StatusType
data StatusType
    = Failed
    | InsufficientData
    | Passed
      deriving (Eq, Ord, Generic)

instance FromText StatusType where
    fromText "failed" = Right Failed
    fromText "insufficient-data" = Right InsufficientData
    fromText "passed" = Right Passed
    fromText e = failFromText $ "Unrecognised StatusType: " <> e

instance Read StatusType where
    readsPrec _ = readFromText

instance ToText StatusType where
    toText Failed = "failed"
    toText InsufficientData = "insufficient-data"
    toText Passed = "passed"

instance Show StatusType where
    show = showToText

instance ToQuery StatusType where
    toQuery = primToQuery

instance FromXML StatusType where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML StatusType where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for StatusName
data StatusName
    = Reachability
      deriving (Eq, Ord, Generic)

instance FromText StatusName where
    fromText "reachability" = Right Reachability
    fromText e = failFromText $ "Unrecognised StatusName: " <> e

instance Read StatusName where
    readsPrec _ = readFromText

instance ToText StatusName where
    toText Reachability = "reachability"

instance Show StatusName where
    show = showToText

instance ToQuery StatusName where
    toQuery = primToQuery

instance FromXML StatusName where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML StatusName where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | Specifies the Spot Instance type.
data SpotInstanceType
    = OneTime
    | Persistent
      deriving (Eq, Ord, Generic)

instance FromText SpotInstanceType where
    fromText "one-time" = Right OneTime
    fromText "persistent" = Right Persistent
    fromText e = failFromText $ "Unrecognised SpotInstanceType: " <> e

instance Read SpotInstanceType where
    readsPrec _ = readFromText

instance ToText SpotInstanceType where
    toText OneTime = "one-time"
    toText Persistent = "persistent"

instance Show SpotInstanceType where
    show = showToText

instance ToQuery SpotInstanceType where
    toQuery = primToQuery

instance FromXML SpotInstanceType where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML SpotInstanceType where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for SpotInstanceState
data SpotInstanceState
    = Active
    | Cancelled
    | Closed
    | Failed
    | Open
      deriving (Eq, Ord, Generic)

instance FromText SpotInstanceState where
    fromText "active" = Right Active
    fromText "cancelled" = Right Cancelled
    fromText "closed" = Right Closed
    fromText "failed" = Right Failed
    fromText "open" = Right Open
    fromText e = failFromText $ "Unrecognised SpotInstanceState: " <> e

instance Read SpotInstanceState where
    readsPrec _ = readFromText

instance ToText SpotInstanceState where
    toText Active = "active"
    toText Cancelled = "cancelled"
    toText Closed = "closed"
    toText Failed = "failed"
    toText Open = "open"

instance Show SpotInstanceState where
    show = showToText

instance ToQuery SpotInstanceState where
    toQuery = primToQuery

instance FromXML SpotInstanceState where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML SpotInstanceState where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | Snapshot state (e.g., pending, completed, or error).
data SnapshotState
    = Completed
    | Error
    | Pending
      deriving (Eq, Ord, Generic)

instance FromText SnapshotState where
    fromText "completed" = Right Completed
    fromText "error" = Right Error
    fromText "pending" = Right Pending
    fromText e = failFromText $ "Unrecognised SnapshotState: " <> e

instance Read SnapshotState where
    readsPrec _ = readFromText

instance ToText SnapshotState where
    toText Completed = "completed"
    toText Error = "error"
    toText Pending = "pending"

instance Show SnapshotState where
    show = showToText

instance ToQuery SnapshotState where
    toQuery = primToQuery

instance FromXML SnapshotState where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML SnapshotState where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | The name of the EBS attribute to describe. Available attribute names:
-- createVolumePermission.
data SnapshotAttributeName
    = Createvolumepermission
    | Productcodes
      deriving (Eq, Ord, Generic)

instance FromText SnapshotAttributeName where
    fromText "createVolumePermission" = Right Createvolumepermission
    fromText "productCodes" = Right Productcodes
    fromText e = failFromText $ "Unrecognised SnapshotAttributeName: " <> e

instance Read SnapshotAttributeName where
    readsPrec _ = readFromText

instance ToText SnapshotAttributeName where
    toText Createvolumepermission = "createVolumePermission"
    toText Productcodes = "productCodes"

instance Show SnapshotAttributeName where
    show = showToText

instance ToQuery SnapshotAttributeName where
    toQuery = primToQuery

instance FromXML SnapshotAttributeName where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML SnapshotAttributeName where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for ShutdownBehavior
data ShutdownBehavior
    = Stop
    | Terminate
      deriving (Eq, Ord, Generic)

instance FromText ShutdownBehavior where
    fromText "stop" = Right Stop
    fromText "terminate" = Right Terminate
    fromText e = failFromText $ "Unrecognised ShutdownBehavior: " <> e

instance Read ShutdownBehavior where
    readsPrec _ = readFromText

instance ToText ShutdownBehavior where
    toText Stop = "stop"
    toText Terminate = "terminate"

instance Show ShutdownBehavior where
    show = showToText

instance ToQuery ShutdownBehavior where
    toQuery = primToQuery

instance FromXML ShutdownBehavior where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML ShutdownBehavior where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for RuleAction
data RuleAction
    = Allow
    | Deny
      deriving (Eq, Ord, Generic)

instance FromText RuleAction where
    fromText "allow" = Right Allow
    fromText "deny" = Right Deny
    fromText e = failFromText $ "Unrecognised RuleAction: " <> e

instance Read RuleAction where
    readsPrec _ = readFromText

instance ToText RuleAction where
    toText Allow = "allow"
    toText Deny = "deny"

instance Show RuleAction where
    show = showToText

instance ToQuery RuleAction where
    toQuery = primToQuery

instance FromXML RuleAction where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML RuleAction where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for RouteState
data RouteState
    = Active
    | Blackhole
      deriving (Eq, Ord, Generic)

instance FromText RouteState where
    fromText "active" = Right Active
    fromText "blackhole" = Right Blackhole
    fromText e = failFromText $ "Unrecognised RouteState: " <> e

instance Read RouteState where
    readsPrec _ = readFromText

instance ToText RouteState where
    toText Active = "active"
    toText Blackhole = "blackhole"

instance Show RouteState where
    show = showToText

instance ToQuery RouteState where
    toQuery = primToQuery

instance FromXML RouteState where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML RouteState where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | The type of resource identified by the associated resource ID (ex:
-- instance, AMI, EBS volume, etc).
data ResourceType
    = CustomerGateway
    | DhcpOptions
    | Image
    | Instance
    | InternetGateway
    | NetworkAcl
    | NetworkInterface
    | ReservedInstances
    | RouteTable
    | SecurityGroup
    | Snapshot
    | SpotInstancesRequest
    | Subnet
    | Volume
    | Vpc
    | VpnConnection
    | VpnGateway
      deriving (Eq, Ord, Generic)

instance FromText ResourceType where
    fromText "customer-gateway" = Right CustomerGateway
    fromText "dhcp-options" = Right DhcpOptions
    fromText "image" = Right Image
    fromText "instance" = Right Instance
    fromText "internet-gateway" = Right InternetGateway
    fromText "network-acl" = Right NetworkAcl
    fromText "network-interface" = Right NetworkInterface
    fromText "reserved-instances" = Right ReservedInstances
    fromText "route-table" = Right RouteTable
    fromText "security-group" = Right SecurityGroup
    fromText "snapshot" = Right Snapshot
    fromText "spot-instances-request" = Right SpotInstancesRequest
    fromText "subnet" = Right Subnet
    fromText "volume" = Right Volume
    fromText "vpc" = Right Vpc
    fromText "vpn-connection" = Right VpnConnection
    fromText "vpn-gateway" = Right VpnGateway
    fromText e = failFromText $ "Unrecognised ResourceType: " <> e

instance Read ResourceType where
    readsPrec _ = readFromText

instance ToText ResourceType where
    toText CustomerGateway = "customer-gateway"
    toText DhcpOptions = "dhcp-options"
    toText Image = "image"
    toText Instance = "instance"
    toText InternetGateway = "internet-gateway"
    toText NetworkAcl = "network-acl"
    toText NetworkInterface = "network-interface"
    toText ReservedInstances = "reserved-instances"
    toText RouteTable = "route-table"
    toText SecurityGroup = "security-group"
    toText Snapshot = "snapshot"
    toText SpotInstancesRequest = "spot-instances-request"
    toText Subnet = "subnet"
    toText Volume = "volume"
    toText Vpc = "vpc"
    toText VpnConnection = "vpn-connection"
    toText VpnGateway = "vpn-gateway"

instance Show ResourceType where
    show = showToText

instance ToQuery ResourceType where
    toQuery = primToQuery

instance FromXML ResourceType where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML ResourceType where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | The name of the attribute being reset. Available attribute names:
-- launchPermission.
data ResetImageAttributeName
    = Launchpermission
      deriving (Eq, Ord, Generic)

instance FromText ResetImageAttributeName where
    fromText "launchPermission" = Right Launchpermission
    fromText e = failFromText $ "Unrecognised ResetImageAttributeName: " <> e

instance Read ResetImageAttributeName where
    readsPrec _ = readFromText

instance ToText ResetImageAttributeName where
    toText Launchpermission = "launchPermission"

instance Show ResetImageAttributeName where
    show = showToText

instance ToQuery ResetImageAttributeName where
    toQuery = primToQuery

instance FromXML ResetImageAttributeName where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML ResetImageAttributeName where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | The state of the Reserved Instances purchase.
data ReservedInstanceState
    = Active
    | PaymentFailed
    | PaymentPending
    | Retired
      deriving (Eq, Ord, Generic)

instance FromText ReservedInstanceState where
    fromText "active" = Right Active
    fromText "payment-failed" = Right PaymentFailed
    fromText "payment-pending" = Right PaymentPending
    fromText "retired" = Right Retired
    fromText e = failFromText $ "Unrecognised ReservedInstanceState: " <> e

instance Read ReservedInstanceState where
    readsPrec _ = readFromText

instance ToText ReservedInstanceState where
    toText Active = "active"
    toText PaymentFailed = "payment-failed"
    toText PaymentPending = "payment-pending"
    toText Retired = "retired"

instance Show ReservedInstanceState where
    show = showToText

instance ToQuery ReservedInstanceState where
    toQuery = primToQuery

instance FromXML ReservedInstanceState where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML ReservedInstanceState where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for ReportStatusType
data ReportStatusType
    = Impaired
    | Ok
      deriving (Eq, Ord, Generic)

instance FromText ReportStatusType where
    fromText "impaired" = Right Impaired
    fromText "ok" = Right Ok
    fromText e = failFromText $ "Unrecognised ReportStatusType: " <> e

instance Read ReportStatusType where
    readsPrec _ = readFromText

instance ToText ReportStatusType where
    toText Impaired = "impaired"
    toText Ok = "ok"

instance Show ReportStatusType where
    show = showToText

instance ToQuery ReportStatusType where
    toQuery = primToQuery

instance FromXML ReportStatusType where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML ReportStatusType where
    toXMLOptions = xmlOptions
    toXML        = primToXML

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
    fromText e = failFromText $ "Unrecognised ReportInstanceReasonCodes: " <> e

instance Read ReportInstanceReasonCodes where
    readsPrec _ = readFromText

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
    show = showToText

instance ToQuery ReportInstanceReasonCodes where
    toQuery = primToQuery

instance FromXML ReportInstanceReasonCodes where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML ReportInstanceReasonCodes where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | The frequency of the recurring charge.
data RecurringChargeFrequency
    = Hourly
      deriving (Eq, Ord, Generic)

instance FromText RecurringChargeFrequency where
    fromText "Hourly" = Right Hourly
    fromText e = failFromText $ "Unrecognised RecurringChargeFrequency: " <> e

instance Read RecurringChargeFrequency where
    readsPrec _ = readFromText

instance ToText RecurringChargeFrequency where
    toText Hourly = "Hourly"

instance Show RecurringChargeFrequency where
    show = showToText

instance ToQuery RecurringChargeFrequency where
    toQuery = primToQuery

instance FromXML RecurringChargeFrequency where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML RecurringChargeFrequency where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for RIProductDescription
data RIProductDescription
    = Linux_Unix
    | Linux_Unix_AmazonVpc
    | Windows
    | Windows_AmazonVpc
      deriving (Eq, Ord, Generic)

instance FromText RIProductDescription where
    fromText "Linux/UNIX" = Right Linux_Unix
    fromText "Linux/UNIX (Amazon VPC)" = Right Linux_Unix_AmazonVpc
    fromText "Windows" = Right Windows
    fromText "Windows (Amazon VPC)" = Right Windows_AmazonVpc
    fromText e = failFromText $ "Unrecognised RIProductDescription: " <> e

instance Read RIProductDescription where
    readsPrec _ = readFromText

instance ToText RIProductDescription where
    toText Linux_Unix = "Linux/UNIX"
    toText Linux_Unix_AmazonVpc = "Linux/UNIX (Amazon VPC)"
    toText Windows = "Windows"
    toText Windows_AmazonVpc = "Windows (Amazon VPC)"

instance Show RIProductDescription where
    show = showToText

instance ToQuery RIProductDescription where
    toQuery = primToQuery

instance FromXML RIProductDescription where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML RIProductDescription where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for ProductCodeValues
data ProductCodeValues
    = Devpay
    | Marketplace
      deriving (Eq, Ord, Generic)

instance FromText ProductCodeValues where
    fromText "devpay" = Right Devpay
    fromText "marketplace" = Right Marketplace
    fromText e = failFromText $ "Unrecognised ProductCodeValues: " <> e

instance Read ProductCodeValues where
    readsPrec _ = readFromText

instance ToText ProductCodeValues where
    toText Devpay = "devpay"
    toText Marketplace = "marketplace"

instance Show ProductCodeValues where
    show = showToText

instance ToQuery ProductCodeValues where
    toQuery = primToQuery

instance FromXML ProductCodeValues where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML ProductCodeValues where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for PlatformValues
data PlatformValues
    = Windows
      deriving (Eq, Ord, Generic)

instance FromText PlatformValues where
    fromText "Windows" = Right Windows
    fromText e = failFromText $ "Unrecognised PlatformValues: " <> e

instance Read PlatformValues where
    readsPrec _ = readFromText

instance ToText PlatformValues where
    toText Windows = "Windows"

instance Show PlatformValues where
    show = showToText

instance ToQuery PlatformValues where
    toQuery = primToQuery

instance FromXML PlatformValues where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML PlatformValues where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | The PlacementGroup strategy.
data PlacementStrategy
    = Cluster
      deriving (Eq, Ord, Generic)

instance FromText PlacementStrategy where
    fromText "cluster" = Right Cluster
    fromText e = failFromText $ "Unrecognised PlacementStrategy: " <> e

instance Read PlacementStrategy where
    readsPrec _ = readFromText

instance ToText PlacementStrategy where
    toText Cluster = "cluster"

instance Show PlacementStrategy where
    show = showToText

instance ToQuery PlacementStrategy where
    toQuery = primToQuery

instance FromXML PlacementStrategy where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML PlacementStrategy where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | The state of this PlacementGroup.
data PlacementGroupState
    = Available
    | Deleted
    | Deleting
    | Pending
      deriving (Eq, Ord, Generic)

instance FromText PlacementGroupState where
    fromText "available" = Right Available
    fromText "deleted" = Right Deleted
    fromText "deleting" = Right Deleting
    fromText "pending" = Right Pending
    fromText e = failFromText $ "Unrecognised PlacementGroupState: " <> e

instance Read PlacementGroupState where
    readsPrec _ = readFromText

instance ToText PlacementGroupState where
    toText Available = "available"
    toText Deleted = "deleted"
    toText Deleting = "deleting"
    toText Pending = "pending"

instance Show PlacementGroupState where
    show = showToText

instance ToQuery PlacementGroupState where
    toQuery = primToQuery

instance FromXML PlacementGroupState where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML PlacementGroupState where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | The group that is allowed to create volumes from the snapshot (currently
-- supports "all").
data PermissionGroup
    = All
      deriving (Eq, Ord, Generic)

instance FromText PermissionGroup where
    fromText "all" = Right All
    fromText e = failFromText $ "Unrecognised PermissionGroup: " <> e

instance Read PermissionGroup where
    readsPrec _ = readFromText

instance ToText PermissionGroup where
    toText All = "all"

instance Show PermissionGroup where
    show = showToText

instance ToQuery PermissionGroup where
    toQuery = primToQuery

instance FromXML PermissionGroup where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML PermissionGroup where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | The Reserved Instance offering type.
data OfferingTypeValues
    = HeavyUtilization
    | LightUtilization
    | MediumUtilization
      deriving (Eq, Ord, Generic)

instance FromText OfferingTypeValues where
    fromText "Heavy Utilization" = Right HeavyUtilization
    fromText "Light Utilization" = Right LightUtilization
    fromText "Medium Utilization" = Right MediumUtilization
    fromText e = failFromText $ "Unrecognised OfferingTypeValues: " <> e

instance Read OfferingTypeValues where
    readsPrec _ = readFromText

instance ToText OfferingTypeValues where
    toText HeavyUtilization = "Heavy Utilization"
    toText LightUtilization = "Light Utilization"
    toText MediumUtilization = "Medium Utilization"

instance Show OfferingTypeValues where
    show = showToText

instance ToQuery OfferingTypeValues where
    toQuery = primToQuery

instance FromXML OfferingTypeValues where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML OfferingTypeValues where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for NetworkInterfaceStatus
data NetworkInterfaceStatus
    = Attaching
    | Available
    | Detaching
    | InUse
      deriving (Eq, Ord, Generic)

instance FromText NetworkInterfaceStatus where
    fromText "attaching" = Right Attaching
    fromText "available" = Right Available
    fromText "detaching" = Right Detaching
    fromText "in-use" = Right InUse
    fromText e = failFromText $ "Unrecognised NetworkInterfaceStatus: " <> e

instance Read NetworkInterfaceStatus where
    readsPrec _ = readFromText

instance ToText NetworkInterfaceStatus where
    toText Attaching = "attaching"
    toText Available = "available"
    toText Detaching = "detaching"
    toText InUse = "in-use"

instance Show NetworkInterfaceStatus where
    show = showToText

instance ToQuery NetworkInterfaceStatus where
    toQuery = primToQuery

instance FromXML NetworkInterfaceStatus where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML NetworkInterfaceStatus where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | The state of monitoring on an Amazon EC2 instance (ex: enabled, disabled).
data MonitoringState
    = Disabled
    | Enabled
    | Pending
      deriving (Eq, Ord, Generic)

instance FromText MonitoringState where
    fromText "disabled" = Right Disabled
    fromText "enabled" = Right Enabled
    fromText "pending" = Right Pending
    fromText e = failFromText $ "Unrecognised MonitoringState: " <> e

instance Read MonitoringState where
    readsPrec _ = readFromText

instance ToText MonitoringState where
    toText Disabled = "disabled"
    toText Enabled = "enabled"
    toText Pending = "pending"

instance Show MonitoringState where
    show = showToText

instance ToQuery MonitoringState where
    toQuery = primToQuery

instance FromXML MonitoringState where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML MonitoringState where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for ListingStatus
data ListingStatus
    = Active
    | Cancelled
    | Closed
    | Pending
      deriving (Eq, Ord, Generic)

instance FromText ListingStatus where
    fromText "active" = Right Active
    fromText "cancelled" = Right Cancelled
    fromText "closed" = Right Closed
    fromText "pending" = Right Pending
    fromText e = failFromText $ "Unrecognised ListingStatus: " <> e

instance Read ListingStatus where
    readsPrec _ = readFromText

instance ToText ListingStatus where
    toText Active = "active"
    toText Cancelled = "cancelled"
    toText Closed = "closed"
    toText Pending = "pending"

instance Show ListingStatus where
    show = showToText

instance ToQuery ListingStatus where
    toQuery = primToQuery

instance FromXML ListingStatus where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML ListingStatus where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for ListingState
data ListingState
    = Available
    | Cancelled
    | Pending
    | Sold
      deriving (Eq, Ord, Generic)

instance FromText ListingState where
    fromText "available" = Right Available
    fromText "cancelled" = Right Cancelled
    fromText "pending" = Right Pending
    fromText "sold" = Right Sold
    fromText e = failFromText $ "Unrecognised ListingState: " <> e

instance Read ListingState where
    readsPrec _ = readFromText

instance ToText ListingState where
    toText Available = "available"
    toText Cancelled = "cancelled"
    toText Pending = "pending"
    toText Sold = "sold"

instance Show ListingState where
    show = showToText

instance ToQuery ListingState where
    toQuery = primToQuery

instance FromXML ListingState where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML ListingState where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for InstanceType
data InstanceType
    = C1_Medium
    | C1_Xlarge
    | C3_2Xlarge
    | C3_4Xlarge
    | C3_8Xlarge
    | C3_Large
    | C3_Xlarge
    | Cc1_4Xlarge
    | Cc2_8Xlarge
    | Cg1_4Xlarge
    | Cr1_8Xlarge
    | G2_2Xlarge
    | Hi1_4Xlarge
    | Hs1_8Xlarge
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

instance FromText InstanceType where
    fromText "c1.medium" = Right C1_Medium
    fromText "c1.xlarge" = Right C1_Xlarge
    fromText "c3.2xlarge" = Right C3_2Xlarge
    fromText "c3.4xlarge" = Right C3_4Xlarge
    fromText "c3.8xlarge" = Right C3_8Xlarge
    fromText "c3.large" = Right C3_Large
    fromText "c3.xlarge" = Right C3_Xlarge
    fromText "cc1.4xlarge" = Right Cc1_4Xlarge
    fromText "cc2.8xlarge" = Right Cc2_8Xlarge
    fromText "cg1.4xlarge" = Right Cg1_4Xlarge
    fromText "cr1.8xlarge" = Right Cr1_8Xlarge
    fromText "g2.2xlarge" = Right G2_2Xlarge
    fromText "hi1.4xlarge" = Right Hi1_4Xlarge
    fromText "hs1.8xlarge" = Right Hs1_8Xlarge
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
    fromText e = failFromText $ "Unrecognised InstanceType: " <> e

instance Read InstanceType where
    readsPrec _ = readFromText

instance ToText InstanceType where
    toText C1_Medium = "c1.medium"
    toText C1_Xlarge = "c1.xlarge"
    toText C3_2Xlarge = "c3.2xlarge"
    toText C3_4Xlarge = "c3.4xlarge"
    toText C3_8Xlarge = "c3.8xlarge"
    toText C3_Large = "c3.large"
    toText C3_Xlarge = "c3.xlarge"
    toText Cc1_4Xlarge = "cc1.4xlarge"
    toText Cc2_8Xlarge = "cc2.8xlarge"
    toText Cg1_4Xlarge = "cg1.4xlarge"
    toText Cr1_8Xlarge = "cr1.8xlarge"
    toText G2_2Xlarge = "g2.2xlarge"
    toText Hi1_4Xlarge = "hi1.4xlarge"
    toText Hs1_8Xlarge = "hs1.8xlarge"
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
    show = showToText

instance ToQuery InstanceType where
    toQuery = primToQuery

instance FromXML InstanceType where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML InstanceType where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | The current state of the instance.
data InstanceStateName
    = Pending
    | Running
    | ShuttingDown
    | Stopped
    | Stopping
    | Terminated
      deriving (Eq, Ord, Generic)

instance FromText InstanceStateName where
    fromText "pending" = Right Pending
    fromText "running" = Right Running
    fromText "shutting-down" = Right ShuttingDown
    fromText "stopped" = Right Stopped
    fromText "stopping" = Right Stopping
    fromText "terminated" = Right Terminated
    fromText e = failFromText $ "Unrecognised InstanceStateName: " <> e

instance Read InstanceStateName where
    readsPrec _ = readFromText

instance ToText InstanceStateName where
    toText Pending = "pending"
    toText Running = "running"
    toText ShuttingDown = "shutting-down"
    toText Stopped = "stopped"
    toText Stopping = "stopping"
    toText Terminated = "terminated"

instance Show InstanceStateName where
    show = showToText

instance ToQuery InstanceStateName where
    toQuery = primToQuery

instance FromXML InstanceStateName where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML InstanceStateName where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for InstanceLifecycleType
data InstanceLifecycleType
    = Spot
      deriving (Eq, Ord, Generic)

instance FromText InstanceLifecycleType where
    fromText "spot" = Right Spot
    fromText e = failFromText $ "Unrecognised InstanceLifecycleType: " <> e

instance Read InstanceLifecycleType where
    readsPrec _ = readFromText

instance ToText InstanceLifecycleType where
    toText Spot = "spot"

instance Show InstanceLifecycleType where
    show = showToText

instance ToQuery InstanceLifecycleType where
    toQuery = primToQuery

instance FromXML InstanceLifecycleType where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML InstanceLifecycleType where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | The name of the attribute being reset. Available attribute names: kernel,
-- ramdisk.
data InstanceAttributeName
    = Blockdevicemapping
    | Disableapitermination
    | Ebsoptimized
    | Groupset
    | Instanceinitiatedshutdownbehavior
    | Instancetype
    | Kernel
    | Productcodes
    | Ramdisk
    | Rootdevicename
    | Sourcedestcheck
    | Userdata
      deriving (Eq, Ord, Generic)

instance FromText InstanceAttributeName where
    fromText "blockDeviceMapping" = Right Blockdevicemapping
    fromText "disableApiTermination" = Right Disableapitermination
    fromText "ebsOptimized" = Right Ebsoptimized
    fromText "groupSet" = Right Groupset
    fromText "instanceInitiatedShutdownBehavior" = Right Instanceinitiatedshutdownbehavior
    fromText "instanceType" = Right Instancetype
    fromText "kernel" = Right Kernel
    fromText "productCodes" = Right Productcodes
    fromText "ramdisk" = Right Ramdisk
    fromText "rootDeviceName" = Right Rootdevicename
    fromText "sourceDestCheck" = Right Sourcedestcheck
    fromText "userData" = Right Userdata
    fromText e = failFromText $ "Unrecognised InstanceAttributeName: " <> e

instance Read InstanceAttributeName where
    readsPrec _ = readFromText

instance ToText InstanceAttributeName where
    toText Blockdevicemapping = "blockDeviceMapping"
    toText Disableapitermination = "disableApiTermination"
    toText Ebsoptimized = "ebsOptimized"
    toText Groupset = "groupSet"
    toText Instanceinitiatedshutdownbehavior = "instanceInitiatedShutdownBehavior"
    toText Instancetype = "instanceType"
    toText Kernel = "kernel"
    toText Productcodes = "productCodes"
    toText Ramdisk = "ramdisk"
    toText Rootdevicename = "rootDeviceName"
    toText Sourcedestcheck = "sourceDestCheck"
    toText Userdata = "userData"

instance Show InstanceAttributeName where
    show = showToText

instance ToQuery InstanceAttributeName where
    toQuery = primToQuery

instance FromXML InstanceAttributeName where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML InstanceAttributeName where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | The type of image (machine, kernel, or ramdisk).
data ImageTypeValues
    = Kernel
    | Machine
    | Ramdisk
      deriving (Eq, Ord, Generic)

instance FromText ImageTypeValues where
    fromText "kernel" = Right Kernel
    fromText "machine" = Right Machine
    fromText "ramdisk" = Right Ramdisk
    fromText e = failFromText $ "Unrecognised ImageTypeValues: " <> e

instance Read ImageTypeValues where
    readsPrec _ = readFromText

instance ToText ImageTypeValues where
    toText Kernel = "kernel"
    toText Machine = "machine"
    toText Ramdisk = "ramdisk"

instance Show ImageTypeValues where
    show = showToText

instance ToQuery ImageTypeValues where
    toQuery = primToQuery

instance FromXML ImageTypeValues where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML ImageTypeValues where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | Current state of the AMI. If the operation returns available, the image is
-- successfully registered and available for launching. If the operation
-- returns deregistered, the image is deregistered and no longer available for
-- launching.
data ImageState
    = Available
    | Deregistered
      deriving (Eq, Ord, Generic)

instance FromText ImageState where
    fromText "available" = Right Available
    fromText "deregistered" = Right Deregistered
    fromText e = failFromText $ "Unrecognised ImageState: " <> e

instance Read ImageState where
    readsPrec _ = readFromText

instance ToText ImageState where
    toText Available = "available"
    toText Deregistered = "deregistered"

instance Show ImageState where
    show = showToText

instance ToQuery ImageState where
    toQuery = primToQuery

instance FromXML ImageState where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML ImageState where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | The name of the attribute to describe. Available attribute names:
-- productCodes, kernel, ramdisk, launchPermisson, blockDeviceMapping.
data ImageAttributeName
    = Blockdevicemapping
    | Description
    | Kernel
    | Launchpermission
    | Productcodes
    | Ramdisk
      deriving (Eq, Ord, Generic)

instance FromText ImageAttributeName where
    fromText "blockDeviceMapping" = Right Blockdevicemapping
    fromText "description" = Right Description
    fromText "kernel" = Right Kernel
    fromText "launchPermission" = Right Launchpermission
    fromText "productCodes" = Right Productcodes
    fromText "ramdisk" = Right Ramdisk
    fromText e = failFromText $ "Unrecognised ImageAttributeName: " <> e

instance Read ImageAttributeName where
    readsPrec _ = readFromText

instance ToText ImageAttributeName where
    toText Blockdevicemapping = "blockDeviceMapping"
    toText Description = "description"
    toText Kernel = "kernel"
    toText Launchpermission = "launchPermission"
    toText Productcodes = "productCodes"
    toText Ramdisk = "ramdisk"

instance Show ImageAttributeName where
    show = showToText

instance ToQuery ImageAttributeName where
    toQuery = primToQuery

instance FromXML ImageAttributeName where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML ImageAttributeName where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for HypervisorType
data HypervisorType
    = Ovm
    | Xen
      deriving (Eq, Ord, Generic)

instance FromText HypervisorType where
    fromText "ovm" = Right Ovm
    fromText "xen" = Right Xen
    fromText e = failFromText $ "Unrecognised HypervisorType: " <> e

instance Read HypervisorType where
    readsPrec _ = readFromText

instance ToText HypervisorType where
    toText Ovm = "ovm"
    toText Xen = "xen"

instance Show HypervisorType where
    show = showToText

instance ToQuery HypervisorType where
    toQuery = primToQuery

instance FromXML HypervisorType where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML HypervisorType where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | The type of VPN connection this VPN gateway supports.
data GatewayType
    = Ipsec_1
      deriving (Eq, Ord, Generic)

instance FromText GatewayType where
    fromText "ipsec.1" = Right Ipsec_1
    fromText e = failFromText $ "Unrecognised GatewayType: " <> e

instance Read GatewayType where
    readsPrec _ = readFromText

instance ToText GatewayType where
    toText Ipsec_1 = "ipsec.1"

instance Show GatewayType where
    show = showToText

instance ToQuery GatewayType where
    toQuery = primToQuery

instance FromXML GatewayType where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML GatewayType where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for ExportTaskState
data ExportTaskState
    = Active
    | Cancelled
    | Cancelling
    | Completed
      deriving (Eq, Ord, Generic)

instance FromText ExportTaskState where
    fromText "active" = Right Active
    fromText "cancelled" = Right Cancelled
    fromText "cancelling" = Right Cancelling
    fromText "completed" = Right Completed
    fromText e = failFromText $ "Unrecognised ExportTaskState: " <> e

instance Read ExportTaskState where
    readsPrec _ = readFromText

instance ToText ExportTaskState where
    toText Active = "active"
    toText Cancelled = "cancelled"
    toText Cancelling = "cancelling"
    toText Completed = "completed"

instance Show ExportTaskState where
    show = showToText

instance ToQuery ExportTaskState where
    toQuery = primToQuery

instance FromXML ExportTaskState where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML ExportTaskState where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for ExportEnvironment
data ExportEnvironment
    = Citrix
    | Microsoft
    | Vmware
      deriving (Eq, Ord, Generic)

instance FromText ExportEnvironment where
    fromText "citrix" = Right Citrix
    fromText "microsoft" = Right Microsoft
    fromText "vmware" = Right Vmware
    fromText e = failFromText $ "Unrecognised ExportEnvironment: " <> e

instance Read ExportEnvironment where
    readsPrec _ = readFromText

instance ToText ExportEnvironment where
    toText Citrix = "citrix"
    toText Microsoft = "microsoft"
    toText Vmware = "vmware"

instance Show ExportEnvironment where
    show = showToText

instance ToQuery ExportEnvironment where
    toQuery = primToQuery

instance FromXML ExportEnvironment where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML ExportEnvironment where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | The associated code of the event. Valid values: instance-reboot,
-- system-reboot, instance-retirement.
data EventCode
    = InstanceReboot
    | InstanceRetirement
    | InstanceStop
    | SystemMaintenance
    | SystemReboot
      deriving (Eq, Ord, Generic)

instance FromText EventCode where
    fromText "instance-reboot" = Right InstanceReboot
    fromText "instance-retirement" = Right InstanceRetirement
    fromText "instance-stop" = Right InstanceStop
    fromText "system-maintenance" = Right SystemMaintenance
    fromText "system-reboot" = Right SystemReboot
    fromText e = failFromText $ "Unrecognised EventCode: " <> e

instance Read EventCode where
    readsPrec _ = readFromText

instance ToText EventCode where
    toText InstanceReboot = "instance-reboot"
    toText InstanceRetirement = "instance-retirement"
    toText InstanceStop = "instance-stop"
    toText SystemMaintenance = "system-maintenance"
    toText SystemReboot = "system-reboot"

instance Show EventCode where
    show = showToText

instance ToQuery EventCode where
    toQuery = primToQuery

instance FromXML EventCode where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML EventCode where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for DomainType
data DomainType
    = Standard
    | Vpc
      deriving (Eq, Ord, Generic)

instance FromText DomainType where
    fromText "standard" = Right Standard
    fromText "vpc" = Right Vpc
    fromText e = failFromText $ "Unrecognised DomainType: " <> e

instance Read DomainType where
    readsPrec _ = readFromText

instance ToText DomainType where
    toText Standard = "standard"
    toText Vpc = "vpc"

instance Show DomainType where
    show = showToText

instance ToQuery DomainType where
    toQuery = primToQuery

instance FromXML DomainType where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML DomainType where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for DiskImageFormat
data DiskImageFormat
    = Raw
    | Vhd
    | Vmdk
      deriving (Eq, Ord, Generic)

instance FromText DiskImageFormat where
    fromText "RAW" = Right Raw
    fromText "VHD" = Right Vhd
    fromText "VMDK" = Right Vmdk
    fromText e = failFromText $ "Unrecognised DiskImageFormat: " <> e

instance Read DiskImageFormat where
    readsPrec _ = readFromText

instance ToText DiskImageFormat where
    toText Raw = "RAW"
    toText Vhd = "VHD"
    toText Vmdk = "VMDK"

instance Show DiskImageFormat where
    show = showToText

instance ToQuery DiskImageFormat where
    toQuery = primToQuery

instance FromXML DiskImageFormat where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML DiskImageFormat where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | The root device type used by the AMI. The AMI can use an Amazon EBS or
-- instance store root device.
data DeviceType
    = Ebs
    | InstanceStore
      deriving (Eq, Ord, Generic)

instance FromText DeviceType where
    fromText "ebs" = Right Ebs
    fromText "instance-store" = Right InstanceStore
    fromText e = failFromText $ "Unrecognised DeviceType: " <> e

instance Read DeviceType where
    readsPrec _ = readFromText

instance ToText DeviceType where
    toText Ebs = "ebs"
    toText InstanceStore = "instance-store"

instance Show DeviceType where
    show = showToText

instance ToQuery DeviceType where
    toQuery = primToQuery

instance FromXML DeviceType where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML DeviceType where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | Specifies the state of the Spot Instance request.
data DatafeedSubscriptionState
    = Active
    | Inactive
      deriving (Eq, Ord, Generic)

instance FromText DatafeedSubscriptionState where
    fromText "Active" = Right Active
    fromText "Inactive" = Right Inactive
    fromText e = failFromText $ "Unrecognised DatafeedSubscriptionState: " <> e

instance Read DatafeedSubscriptionState where
    readsPrec _ = readFromText

instance ToText DatafeedSubscriptionState where
    toText Active = "Active"
    toText Inactive = "Inactive"

instance Show DatafeedSubscriptionState where
    show = showToText

instance ToQuery DatafeedSubscriptionState where
    toQuery = primToQuery

instance FromXML DatafeedSubscriptionState where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML DatafeedSubscriptionState where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for CurrencyCodeValues
data CurrencyCodeValues
    = Usd
      deriving (Eq, Ord, Generic)

instance FromText CurrencyCodeValues where
    fromText "USD" = Right Usd
    fromText e = failFromText $ "Unrecognised CurrencyCodeValues: " <> e

instance Read CurrencyCodeValues where
    readsPrec _ = readFromText

instance ToText CurrencyCodeValues where
    toText Usd = "USD"

instance Show CurrencyCodeValues where
    show = showToText

instance ToQuery CurrencyCodeValues where
    toQuery = primToQuery

instance FromXML CurrencyCodeValues where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML CurrencyCodeValues where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for ConversionTaskState
data ConversionTaskState
    = Active
    | Cancelled
    | Cancelling
    | Completed
      deriving (Eq, Ord, Generic)

instance FromText ConversionTaskState where
    fromText "active" = Right Active
    fromText "cancelled" = Right Cancelled
    fromText "cancelling" = Right Cancelling
    fromText "completed" = Right Completed
    fromText e = failFromText $ "Unrecognised ConversionTaskState: " <> e

instance Read ConversionTaskState where
    readsPrec _ = readFromText

instance ToText ConversionTaskState where
    toText Active = "active"
    toText Cancelled = "cancelled"
    toText Cancelling = "cancelling"
    toText Completed = "completed"

instance Show ConversionTaskState where
    show = showToText

instance ToQuery ConversionTaskState where
    toQuery = primToQuery

instance FromXML ConversionTaskState where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML ConversionTaskState where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for ContainerFormat
data ContainerFormat
    = Ova
      deriving (Eq, Ord, Generic)

instance FromText ContainerFormat where
    fromText "ova" = Right Ova
    fromText e = failFromText $ "Unrecognised ContainerFormat: " <> e

instance Read ContainerFormat where
    readsPrec _ = readFromText

instance ToText ContainerFormat where
    toText Ova = "ova"

instance Show ContainerFormat where
    show = showToText

instance ToQuery ContainerFormat where
    toQuery = primToQuery

instance FromXML ContainerFormat where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML ContainerFormat where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for CancelSpotInstanceRequestState
data CancelSpotInstanceRequestState
    = Active
    | Cancelled
    | Closed
    | Completed
    | Open
      deriving (Eq, Ord, Generic)

instance FromText CancelSpotInstanceRequestState where
    fromText "active" = Right Active
    fromText "cancelled" = Right Cancelled
    fromText "closed" = Right Closed
    fromText "completed" = Right Completed
    fromText "open" = Right Open
    fromText e = failFromText $ "Unrecognised CancelSpotInstanceRequestState: " <> e

instance Read CancelSpotInstanceRequestState where
    readsPrec _ = readFromText

instance ToText CancelSpotInstanceRequestState where
    toText Active = "active"
    toText Cancelled = "cancelled"
    toText Closed = "closed"
    toText Completed = "completed"
    toText Open = "open"

instance Show CancelSpotInstanceRequestState where
    show = showToText

instance ToQuery CancelSpotInstanceRequestState where
    toQuery = primToQuery

instance FromXML CancelSpotInstanceRequestState where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML CancelSpotInstanceRequestState where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | The state of this task.
data BundleTaskState
    = Bundling
    | Cancelling
    | Complete
    | Failed
    | Pending
    | Storing
    | WaitingForShutdown
      deriving (Eq, Ord, Generic)

instance FromText BundleTaskState where
    fromText "bundling" = Right Bundling
    fromText "cancelling" = Right Cancelling
    fromText "complete" = Right Complete
    fromText "failed" = Right Failed
    fromText "pending" = Right Pending
    fromText "storing" = Right Storing
    fromText "waiting-for-shutdown" = Right WaitingForShutdown
    fromText e = failFromText $ "Unrecognised BundleTaskState: " <> e

instance Read BundleTaskState where
    readsPrec _ = readFromText

instance ToText BundleTaskState where
    toText Bundling = "bundling"
    toText Cancelling = "cancelling"
    toText Complete = "complete"
    toText Failed = "failed"
    toText Pending = "pending"
    toText Storing = "storing"
    toText WaitingForShutdown = "waiting-for-shutdown"

instance Show BundleTaskState where
    show = showToText

instance ToQuery BundleTaskState where
    toQuery = primToQuery

instance FromXML BundleTaskState where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML BundleTaskState where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | State of the Availability Zone.
data AvailabilityZoneState
    = Available
      deriving (Eq, Ord, Generic)

instance FromText AvailabilityZoneState where
    fromText "available" = Right Available
    fromText e = failFromText $ "Unrecognised AvailabilityZoneState: " <> e

instance Read AvailabilityZoneState where
    readsPrec _ = readFromText

instance ToText AvailabilityZoneState where
    toText Available = "available"

instance Show AvailabilityZoneState where
    show = showToText

instance ToQuery AvailabilityZoneState where
    toQuery = primToQuery

instance FromXML AvailabilityZoneState where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML AvailabilityZoneState where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for AttachmentStatus
data AttachmentStatus
    = Attached
    | Attaching
    | Detached
    | Detaching
      deriving (Eq, Ord, Generic)

instance FromText AttachmentStatus where
    fromText "attached" = Right Attached
    fromText "attaching" = Right Attaching
    fromText "detached" = Right Detached
    fromText "detaching" = Right Detaching
    fromText e = failFromText $ "Unrecognised AttachmentStatus: " <> e

instance Read AttachmentStatus where
    readsPrec _ = readFromText

instance ToText AttachmentStatus where
    toText Attached = "attached"
    toText Attaching = "attaching"
    toText Detached = "detached"
    toText Detaching = "detaching"

instance Show AttachmentStatus where
    show = showToText

instance ToQuery AttachmentStatus where
    toQuery = primToQuery

instance FromXML AttachmentStatus where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML AttachmentStatus where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for ArchitectureValues
data ArchitectureValues
    = I386
    | X86_64
      deriving (Eq, Ord, Generic)

instance FromText ArchitectureValues where
    fromText "i386" = Right I386
    fromText "x86_64" = Right X86_64
    fromText e = failFromText $ "Unrecognised ArchitectureValues: " <> e

instance Read ArchitectureValues where
    readsPrec _ = readFromText

instance ToText ArchitectureValues where
    toText I386 = "i386"
    toText X86_64 = "x86_64"

instance Show ArchitectureValues where
    show = showToText

instance ToQuery ArchitectureValues where
    toQuery = primToQuery

instance FromXML ArchitectureValues where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML ArchitectureValues where
    toXMLOptions = xmlOptions
    toXML        = primToXML

-- | FIXME: Type documentation for AccountAttributeName
data AccountAttributeName
    = DefaultVpc
    | SupportedPlatforms
      deriving (Eq, Ord, Generic)

instance FromText AccountAttributeName where
    fromText "default-vpc" = Right DefaultVpc
    fromText "supported-platforms" = Right SupportedPlatforms
    fromText e = failFromText $ "Unrecognised AccountAttributeName: " <> e

instance Read AccountAttributeName where
    readsPrec _ = readFromText

instance ToText AccountAttributeName where
    toText DefaultVpc = "default-vpc"
    toText SupportedPlatforms = "supported-platforms"

instance Show AccountAttributeName where
    show = showToText

instance ToQuery AccountAttributeName where
    toQuery = primToQuery

instance FromXML AccountAttributeName where
    fromXMLOptions = xmlOptions
    fromXML        = primFromXML

instance ToXML AccountAttributeName where
    toXMLOptions = xmlOptions
    toXML        = primToXML
