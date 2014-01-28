{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.RDS.Types where

import Data.ByteString      (ByteString)
import Data.Hashable
import Data.Monoid
import Data.List.NonEmpty
import Data.Text            (Text)
import Data.HashMap.Strict  (HashMap)
import Data.Time
import GHC.Generics         (Generic)
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.RDS.Service

-- | This data type is used as a response element for queries on VPC security
-- group membership.
data VpcSecurityGroupMembership = VpcSecurityGroupMembership
    { vsgmStatus :: Maybe Text
      -- ^ The status of the VPC security group.
    , vsgmVpcSecurityGroupId :: Maybe Text
      -- ^ The name of the VPC security group.
    } deriving (Eq, Show, Generic)

instance ToQuery VpcSecurityGroupMembership

instance FromXML VpcSecurityGroupMembership where
    fromXMLOptions = xmlOptions

instance ToXML VpcSecurityGroupMembership where
    toXMLOptions = xmlOptions

-- | Metadata assigned to an Amazon RDS resource consisting of a key-value pair.
data Tag = Tag
    { tKey :: Maybe Text
      -- ^ A key is the required name of the tag. The string value can be from 1 to
      -- 128 Unicode characters in length and cannot be prefixed with "aws:" or
      -- "rds:". The string may only contain only the set of Unicode letters,
      -- digits, white-space, '_', '.', '/', '=', '+', '-' (Java regex:
      -- "^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-]*)$").
    , tValue :: Maybe Text
      -- ^ A value is the optional value of the tag. The string value can be from 1 to
      -- 256 Unicode characters in length and cannot be prefixed with "aws:" or
      -- "rds:". The string may only contain only the set of Unicode letters,
      -- digits, white-space, '_', '.', '/', '=', '+', '-' (Java regex:
      -- "^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-]*)$").
    } deriving (Eq, Show, Generic)

instance ToQuery Tag

instance FromXML Tag where
    fromXMLOptions = xmlOptions

instance ToXML Tag where
    toXMLOptions = xmlOptions

-- | This data type is used as a response element in the DescribeDBSubnetGroups
-- action.
data Subnet = Subnet
    { sSubnetAvailabilityZone :: Maybe AvailabilityZone
      -- ^ Contains Availability Zone information. This data type is used as an
      -- element in the following data type: OrderableDBInstanceOption.
    , sSubnetIdentifier :: Maybe Text
      -- ^ Specifies the identifier of the subnet.
    , sSubnetStatus :: Maybe Text
      -- ^ Specifies the status of the subnet.
    } deriving (Eq, Show, Generic)

instance ToQuery Subnet

instance FromXML Subnet where
    fromXMLOptions = xmlOptions

instance ToXML Subnet where
    toXMLOptions = xmlOptions

-- | This data type is used as a response element in the
-- DescribeReservedDBInstancesOfferings action.
data ReservedDBInstancesOffering = ReservedDBInstancesOffering
    { rdbioCurrencyCode :: Maybe Text
      -- ^ The currency code for the reserved DB instance offering.
    , rdbioDBInstanceClass :: Maybe Text
      -- ^ The DB instance class for the reserved DB instance.
    , rdbioDuration :: Maybe Int
      -- ^ The duration of the offering in seconds.
    , rdbioFixedPrice :: Maybe Double
      -- ^ The fixed price charged for this offering.
    , rdbioMultiAZ :: Maybe Bool
      -- ^ Indicates if the offering applies to Multi-AZ deployments.
    , rdbioOfferingType :: Maybe Text
      -- ^ The offering type.
    , rdbioProductDescription :: Maybe Text
      -- ^ The database engine used by the offering.
    , rdbioRecurringCharges :: [RecurringCharge]
      -- ^ The recurring price charged to run this reserved DB instance.
    , rdbioReservedDBInstancesOfferingId :: Maybe Text
      -- ^ The offering identifier.
    , rdbioUsagePrice :: Maybe Double
      -- ^ The hourly price charged for this offering.
    } deriving (Eq, Show, Generic)

instance ToQuery ReservedDBInstancesOffering

instance FromXML ReservedDBInstancesOffering where
    fromXMLOptions = xmlOptions

instance ToXML ReservedDBInstancesOffering where
    toXMLOptions = xmlOptions

-- | This data type is used as a response element in the
-- DescribeReservedDBInstances and PurchaseReservedDBInstancesOffering
-- actions.
data ReservedDBInstance = ReservedDBInstance
    { rdbiCurrencyCode :: Maybe Text
      -- ^ The currency code for the reserved DB instance.
    , rdbiDBInstanceClass :: Maybe Text
      -- ^ The DB instance class for the reserved DB instance.
    , rdbiDBInstanceCount :: Maybe Int
      -- ^ The number of reserved DB instances.
    , rdbiDuration :: Maybe Int
      -- ^ The duration of the reservation in seconds.
    , rdbiFixedPrice :: Maybe Double
      -- ^ The fixed price charged for this reserved DB instance.
    , rdbiMultiAZ :: Maybe Bool
      -- ^ Indicates if the reservation applies to Multi-AZ deployments.
    , rdbiOfferingType :: Maybe Text
      -- ^ The offering type of this reserved DB instance.
    , rdbiProductDescription :: Maybe Text
      -- ^ The description of the reserved DB instance.
    , rdbiRecurringCharges :: [RecurringCharge]
      -- ^ The recurring price charged to run this reserved DB instance.
    , rdbiReservedDBInstanceId :: Maybe Text
      -- ^ The unique identifier for the reservation.
    , rdbiReservedDBInstancesOfferingId :: Maybe Text
      -- ^ The offering identifier.
    , rdbiStartTime :: Maybe UTCTime
      -- ^ The time the reservation started.
    , rdbiState :: Maybe Text
      -- ^ The state of the reserved DB instance.
    , rdbiUsagePrice :: Maybe Double
      -- ^ The hourly price charged for this reserved DB instance.
    } deriving (Eq, Show, Generic)

instance ToQuery ReservedDBInstance

instance FromXML ReservedDBInstance where
    fromXMLOptions = xmlOptions

instance ToXML ReservedDBInstance where
    toXMLOptions = xmlOptions

-- | This data type is used as a response element in the
-- DescribeReservedDBInstances and DescribeReservedDBInstancesOfferings
-- actions.
data RecurringCharge = RecurringCharge
    { rcRecurringChargeAmount :: Maybe Double
      -- ^ The amount of the recurring charge.
    , rcRecurringChargeFrequency :: Maybe Text
      -- ^ The frequency of the recurring charge.
    } deriving (Eq, Show, Generic)

instance ToQuery RecurringCharge

instance FromXML RecurringCharge where
    fromXMLOptions = xmlOptions

instance ToXML RecurringCharge where
    toXMLOptions = xmlOptions

-- | Specifies that changes to the DB instance are pending. This element is only
-- included when changes are pending. Specific changes are identified by
-- subelements.
data PendingModifiedValues = PendingModifiedValues
    { pmvAllocatedStorage :: Maybe Int
      -- ^ Contains the new AllocatedStorage size for the DB instance that will be
      -- applied or is in progress.
    , pmvBackupRetentionPeriod :: Maybe Int
      -- ^ Specifies the pending number of days for which automated backups are
      -- retained.
    , pmvDBInstanceClass :: Maybe Text
      -- ^ Contains the new DBInstanceClass for the DB instance that will be applied
      -- or is in progress.
    , pmvDBInstanceIdentifier :: Maybe Text
      -- ^ Contains the new DBInstanceIdentifier for the DB instance that will be
      -- applied or is in progress.
    , pmvEngineVersion :: Maybe Text
      -- ^ Indicates the database engine version.
    , pmvIops :: Maybe Int
      -- ^ Specifies the new Provisioned IOPS value for the DB instance that will be
      -- applied or is being applied.
    , pmvMasterUserPassword :: Maybe Text
      -- ^ Contains the pending or in-progress change of the master credentials for
      -- the DB instance.
    , pmvMultiAZ :: Maybe Bool
      -- ^ Indicates that the Single-AZ DB instance is to change to a Multi-AZ
      -- deployment.
    , pmvPort :: Maybe Int
      -- ^ Specifies the pending port for the DB instance.
    } deriving (Eq, Show, Generic)

instance ToQuery PendingModifiedValues

instance FromXML PendingModifiedValues where
    fromXMLOptions = xmlOptions

instance ToXML PendingModifiedValues where
    toXMLOptions = xmlOptions

-- | This data type is used as a request parameter in the ModifyDBParameterGroup
-- and ResetDBParameterGroup actions. This data type is used as a response
-- element in the DescribeEngineDefaultParameters and DescribeDBParameters
-- actions.
data Parameter = Parameter
    { pAllowedValues :: Maybe Text
      -- ^ Specifies the valid range of values for the parameter.
    , pApplyMethod :: Maybe ApplyMethod
      -- ^ Indicates when to apply parameter updates.
    , pApplyType :: Maybe Text
      -- ^ Specifies the engine specific parameters type.
    , pDataType :: Maybe Text
      -- ^ Specifies the valid data type for the parameter.
    , pDescription :: Maybe Text
      -- ^ Provides a description of the parameter.
    , pIsModifiable :: Maybe Bool
      -- ^ Indicates whether (true) or not (false) the parameter can be modified. Some
      -- parameters have security or operational implications that prevent them from
      -- being changed.
    , pMinimumEngineVersion :: Maybe Text
      -- ^ The earliest engine version to which the parameter can apply.
    , pParameterName :: Maybe Text
      -- ^ Specifies the name of the parameter.
    , pParameterValue :: Maybe Text
      -- ^ Specifies the value of the parameter.
    , pSource :: Maybe Text
      -- ^ Indicates the source of the parameter value.
    } deriving (Eq, Show, Generic)

instance ToQuery Parameter

instance FromXML Parameter where
    fromXMLOptions = xmlOptions

instance ToXML Parameter where
    toXMLOptions = xmlOptions

-- | Contains a list of available options for a DB instance This data type is
-- used as a response element in the DescribeOrderableDBInstanceOptions
-- action.
data OrderableDBInstanceOption = OrderableDBInstanceOption
    { odbioAvailabilityZones :: [AvailabilityZone]
      -- ^ A list of availability zones for the orderable DB instance.
    , odbioDBInstanceClass :: Maybe Text
      -- ^ The DB instance Class for the orderable DB instance.
    , odbioEngine :: Maybe Text
      -- ^ The engine type of the orderable DB instance.
    , odbioEngineVersion :: Maybe Text
      -- ^ The engine version of the orderable DB instance.
    , odbioLicenseModel :: Maybe Text
      -- ^ The license model for the orderable DB instance.
    , odbioMultiAZCapable :: Maybe Bool
      -- ^ Indicates whether this orderable DB instance is multi-AZ capable.
    , odbioReadReplicaCapable :: Maybe Bool
      -- ^ Indicates whether this orderable DB instance can have a read replica.
    , odbioVpc :: Maybe Bool
      -- ^ Indicates whether this is a VPC orderable DB instance.
    } deriving (Eq, Show, Generic)

instance ToQuery OrderableDBInstanceOption

instance FromXML OrderableDBInstanceOption where
    fromXMLOptions = xmlOptions

instance ToXML OrderableDBInstanceOption where
    toXMLOptions = xmlOptions

-- | Option settings are the actual settings being applied or configured for
-- that option. It is used when you modify an option group or describe option
-- groups. For example, the NATIVE_NETWORK_ENCRYPTION option has a setting
-- called SQLNET.ENCRYPTION_SERVER that can have several different values.
data OptionSetting = OptionSetting
    { osAllowedValues :: Maybe Text
      -- ^ The allowed values of the option setting.
    , osApplyType :: Maybe Text
      -- ^ The DB engine specific parameter type.
    , osDataType :: Maybe Text
      -- ^ The data type of the option setting.
    , osDefaultValue :: Maybe Text
      -- ^ The default value of the option setting.
    , osDescription :: Maybe Text
      -- ^ The description of the option setting.
    , osIsCollection :: Maybe Bool
      -- ^ Indicates if the option setting is part of a collection.
    , osIsModifiable :: Maybe Bool
      -- ^ A Boolean value that, when true, indicates the option setting can be
      -- modified from the default.
    , osName :: Maybe Text
      -- ^ The name of the option that has settings that you can set.
    , osValue :: Maybe Text
      -- ^ The current value of the option setting.
    } deriving (Eq, Show, Generic)

instance ToQuery OptionSetting

instance FromXML OptionSetting where
    fromXMLOptions = xmlOptions

instance ToXML OptionSetting where
    toXMLOptions = xmlOptions

-- | option group option settings are used to display settings available for
-- each option with their default values and other information. These values
-- are used with the DescribeOptionGroupOptions action.
data OptionGroupOptionSetting = OptionGroupOptionSetting
    { ogosAllowedValues :: Maybe Text
      -- ^ Indicates the acceptable values for the option group option.
    , ogosApplyType :: Maybe Text
      -- ^ The DB engine specific parameter type for the option group option.
    , ogosDefaultValue :: Maybe Text
      -- ^ The default value for the option group option.
    , ogosIsModifiable :: Maybe Bool
      -- ^ Boolean value where true indicates that this option group option can be
      -- changed from the default value.
    , ogosSettingDescription :: Maybe Text
      -- ^ The description of the option group option.
    , ogosSettingName :: Maybe Text
      -- ^ The name of the option group option.
    } deriving (Eq, Show, Generic)

instance ToQuery OptionGroupOptionSetting

instance FromXML OptionGroupOptionSetting where
    fromXMLOptions = xmlOptions

instance ToXML OptionGroupOptionSetting where
    toXMLOptions = xmlOptions

-- | Available option.
data OptionGroupOption = OptionGroupOption
    { ogoDefaultPort :: Maybe Int
      -- ^ If the option requires a port, specifies the default port for the option.
    , ogoDescription :: Maybe Text
      -- ^ The description of the option.
    , ogoEngineName :: Maybe Text
      -- ^ Engine name that this option can be applied to.
    , ogoMajorEngineVersion :: Maybe Text
      -- ^ Indicates the major engine version that the option is available for.
    , ogoMinimumRequiredMinorEngineVersion :: Maybe Text
      -- ^ The minimum required engine version for the option to be applied.
    , ogoName :: Maybe Text
      -- ^ The name of the option.
    , ogoOptionGroupOptionSettings :: [OptionGroupOptionSetting]
      -- ^ Specifies the option settings that are available (and the default value)
      -- for each option in an option group.
    , ogoOptionsDependedOn :: [Text]
      -- ^ List of all options that are prerequisites for this option.
    , ogoPermanent :: Maybe Bool
      -- ^ A permanent option cannot be removed from the option group once the option
      -- group is used, and it cannot be removed from the db instance after
      -- assigning an option group with this permanent option.
    , ogoPersistent :: Maybe Bool
      -- ^ A persistent option cannot be removed from the option group once the option
      -- group is used, but this option can be removed from the db instance while
      -- modifying the related data and assigning another option group without this
      -- option.
    , ogoPortRequired :: Maybe Bool
      -- ^ Specifies whether the option requires a port.
    } deriving (Eq, Show, Generic)

instance ToQuery OptionGroupOption

instance FromXML OptionGroupOption where
    fromXMLOptions = xmlOptions

instance ToXML OptionGroupOption where
    toXMLOptions = xmlOptions

-- | Provides information on the option groups the DB instance is a member of.
data OptionGroupMembership = OptionGroupMembership
    { ogmOptionGroupName :: Maybe Text
      -- ^ The name of the option group that the instance belongs to.
    , ogmStatus :: Maybe Text
      -- ^ The status of the DB instance's option group membership (e.g. in-sync,
      -- pending, pending-maintenance, applying).
    } deriving (Eq, Show, Generic)

instance ToQuery OptionGroupMembership

instance FromXML OptionGroupMembership where
    fromXMLOptions = xmlOptions

instance ToXML OptionGroupMembership where
    toXMLOptions = xmlOptions

-- | FIXME: Type documentation for OptionGroup
data OptionGroup = OptionGroup
    { ogAllowsVpcAndNonVpcInstanceMemberships :: Maybe Bool
      -- ^ Indicates whether this option group can be applied to both VPC and non-VPC
      -- instances. The value 'true' indicates the option group can be applied to
      -- both VPC and non-VPC instances.
    , ogEngineName :: Maybe Text
      -- ^ Engine name that this option group can be applied to.
    , ogMajorEngineVersion :: Maybe Text
      -- ^ Indicates the major engine version associated with this option group.
    , ogOptionGroupDescription :: Maybe Text
      -- ^ Provides the description of the option group.
    , ogOptionGroupName :: Maybe Text
      -- ^ Specifies the name of the option group.
    , ogOptions :: [Option]
      -- ^ Indicates what options are available in the option group.
    , ogVpcId :: Maybe Text
      -- ^ If AllowsVpcAndNonVpcInstanceMemberships is 'false', this field is blank.
      -- If AllowsVpcAndNonVpcInstanceMemberships is 'true' and this field is blank,
      -- then this option group can be applied to both VPC and non-VPC instances. If
      -- this field contains a value, then this option group can only be applied to
      -- instances that are in the VPC indicated by this field.
    } deriving (Eq, Show, Generic)

instance ToQuery OptionGroup

instance FromXML OptionGroup where
    fromXMLOptions = xmlOptions

instance ToXML OptionGroup where
    toXMLOptions = xmlOptions

-- | A list of all available options.
data OptionConfiguration = OptionConfiguration
    { ocDBSecurityGroupMemberships :: [Text]
      -- ^ A list of DBSecurityGroupMemebrship name strings used for this option.
    , ocOptionName :: !Text
      -- ^ The configuration of options to include in a group.
    , ocOptionSettings :: [OptionSetting]
      -- ^ The option settings to include in an option group.
    , ocPort :: Maybe Int
      -- ^ The optional port for the option.
    , ocVpcSecurityGroupMemberships :: [Text]
      -- ^ A list of VpcSecurityGroupMemebrship name strings used for this option.
    } deriving (Eq, Show, Generic)

instance ToQuery OptionConfiguration

instance FromXML OptionConfiguration where
    fromXMLOptions = xmlOptions

instance ToXML OptionConfiguration where
    toXMLOptions = xmlOptions

-- | Option details.
data Option = Option
    { oDBSecurityGroupMemberships :: [DBSecurityGroupMembership]
      -- ^ If the option requires access to a port, then this DB security group allows
      -- access to the port.
    , oOptionDescription :: Maybe Text
      -- ^ The description of the option.
    , oOptionName :: Maybe Text
      -- ^ The name of the option.
    , oOptionSettings :: [OptionSetting]
      -- ^ The option settings for this option.
    , oPermanent :: Maybe Bool
      -- ^ Indicate if this option is permanent.
    , oPersistent :: Maybe Bool
      -- ^ Indicate if this option is persistent.
    , oPort :: Maybe Int
      -- ^ If required, the port configured for this option to use.
    , oVpcSecurityGroupMemberships :: [VpcSecurityGroupMembership]
      -- ^ If the option requires access to a port, then this VPC security group
      -- allows access to the port.
    } deriving (Eq, Show, Generic)

instance ToQuery Option

instance FromXML Option where
    fromXMLOptions = xmlOptions

instance ToXML Option where
    toXMLOptions = xmlOptions

-- | This data type is used as a response element in the
-- DescribeDBSecurityGroups action.
data IPRange = IPRange
    { iprCIDRIP :: Maybe Text
      -- ^ Specifies the IP range.
    , iprStatus :: Maybe Text
      -- ^ Specifies the status of the IP range. Status can be "authorizing",
      -- "authorized", "revoking", and "revoked".
    } deriving (Eq, Show, Generic)

instance ToQuery IPRange

instance FromXML IPRange where
    fromXMLOptions = xmlOptions

instance ToXML IPRange where
    toXMLOptions = xmlOptions

-- | Contains the results of a successful invocation of the
-- DescribeEventSubscriptions action.
data EventSubscription = EventSubscription
    { esCustSubscriptionId :: Maybe Text
      -- ^ The RDS event notification subscription Id.
    , esCustomerAwsId :: Maybe Text
      -- ^ The AWS customer account associated with the RDS event notification
      -- subscription.
    , esEnabled :: Maybe Bool
      -- ^ A Boolean value indicating if the subscription is enabled. True indicates
      -- the subscription is enabled.
    , esEventCategoriesList :: [Text]
      -- ^ A list of event categories for the RDS event notification subscription.
    , esSnsTopicArn :: Maybe Text
      -- ^ The topic ARN of the RDS event notification subscription.
    , esSourceIdsList :: [Text]
      -- ^ A list of source Ids for the RDS event notification subscription.
    , esSourceType :: Maybe Text
      -- ^ The source type for the RDS event notification subscription.
    , esStatus :: Maybe Text
      -- ^ The status of the RDS event notification subscription. Constraints: Can be
      -- one of the following: creating | modifying | deleting | active |
      -- no-permission | topic-not-exist The status "no-permission" indicates that
      -- RDS no longer has permission to post to the SNS topic. The status
      -- "topic-not-exist" indicates that the topic was deleted after the
      -- subscription was created.
    , esSubscriptionCreationTime :: Maybe Text
      -- ^ The time the RDS event notification subscription was created.
    } deriving (Eq, Show, Generic)

instance ToQuery EventSubscription

instance FromXML EventSubscription where
    fromXMLOptions = xmlOptions

instance ToXML EventSubscription where
    toXMLOptions = xmlOptions

-- | Contains the results of a successful invocation of the
-- DescribeEventCategories action.
data EventCategoriesMap = EventCategoriesMap
    { ecmEventCategories :: [Text]
      -- ^ The event categories for the specified source type.
    , ecmSourceType :: Maybe Text
      -- ^ The source type that the returned categories belong to.
    } deriving (Eq, Show, Generic)

instance ToQuery EventCategoriesMap

instance FromXML EventCategoriesMap where
    fromXMLOptions = xmlOptions

instance ToXML EventCategoriesMap where
    toXMLOptions = xmlOptions

-- | This data type is used as a response element in the DescribeEvents action.
data Event = Event
    { fDate :: Maybe UTCTime
      -- ^ Specifies the date and time of the event.
    , fEventCategories :: [Text]
      -- ^ Specifies the category for the event.
    , fMessage :: Maybe Text
      -- ^ Provides the text of this event.
    , fSourceIdentifier :: Maybe Text
      -- ^ Provides the identifier for the source of the event.
    , fSourceType :: Maybe SourceType
      -- ^ Specifies the source type for this event.
    } deriving (Eq, Show, Generic)

instance ToQuery Event

instance FromXML Event where
    fromXMLOptions = xmlOptions

instance ToXML Event where
    toXMLOptions = xmlOptions

-- | Contains the result of a successful invocation of the
-- DescribeEngineDefaultParameters action.
data EngineDefaults = EngineDefaults
    { edDBParameterGroupFamily :: Maybe Text
      -- ^ Specifies the name of the DB parameter group family which the engine
      -- default parameters apply to.
    , edMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous EngineDefaults request.
      -- If this parameter is specified, the response includes only records beyond
      -- the marker, up to the value specified by MaxRecords .
    , edParameters :: [Parameter]
      -- ^ Contains a list of engine default parameters.
    } deriving (Eq, Show, Generic)

instance ToQuery EngineDefaults

instance FromXML EngineDefaults where
    fromXMLOptions = xmlOptions

instance ToXML EngineDefaults where
    toXMLOptions = xmlOptions

-- | Specifies the connection endpoint.
data Endpoint = Endpoint
    { eAddress :: Maybe Text
      -- ^ Specifies the DNS address of the DB instance.
    , ePort :: Maybe Int
      -- ^ Specifies the port that the database engine is listening on.
    } deriving (Eq, Show, Generic)

instance ToQuery Endpoint

instance FromXML Endpoint where
    fromXMLOptions = xmlOptions

instance ToXML Endpoint where
    toXMLOptions = xmlOptions

-- | This data type is used as a response element in the following actions:
-- AuthorizeDBSecurityGroupIngress DescribeDBSecurityGroups
-- RevokeDBSecurityGroupIngress.
data EC2SecurityGroup = EC2SecurityGroup
    { ec2sgEC2SecurityGroupId :: Maybe Text
      -- ^ Specifies the id of the EC2 security group.
    , ec2sgEC2SecurityGroupName :: Maybe Text
      -- ^ Specifies the name of the EC2 security group.
    , ec2sgEC2SecurityGroupOwnerId :: Maybe Text
      -- ^ Specifies the AWS ID of the owner of the EC2 security group specified in
      -- the EC2SecurityGroupName field.
    , ec2sgStatus :: Maybe Text
      -- ^ Provides the status of the EC2 security group. Status can be "authorizing",
      -- "authorized", "revoking", and "revoked".
    } deriving (Eq, Show, Generic)

instance ToQuery EC2SecurityGroup

instance FromXML EC2SecurityGroup where
    fromXMLOptions = xmlOptions

instance ToXML EC2SecurityGroup where
    toXMLOptions = xmlOptions

-- | This data type is used as a response element to DescribeDBLogFiles.
data DescribeDBLogFilesDetails = DescribeDBLogFilesDetails
    { ddblfdLastWritten :: Maybe Integer
      -- ^ A POSIX timestamp when the last log entry was written.
    , ddblfdLogFileName :: Maybe Text
      -- ^ The name of the log file for the specified DB instance.
    , ddblfdSize :: Maybe Integer
      -- ^ The size, in bytes, of the log file for the specified DB instance.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeDBLogFilesDetails

instance FromXML DescribeDBLogFilesDetails where
    fromXMLOptions = xmlOptions

instance ToXML DescribeDBLogFilesDetails where
    toXMLOptions = xmlOptions

-- | Specifies information on the subnet group associated with the DB instance,
-- including the name, description, and subnets in the subnet group.
data DBSubnetGroup = DBSubnetGroup
    { dbshDBSubnetGroupDescription :: Maybe Text
      -- ^ Provides the description of the DB subnet group.
    , dbshDBSubnetGroupName :: Maybe Text
      -- ^ Specifies the name of the DB subnet group.
    , dbshSubnetGroupStatus :: Maybe Text
      -- ^ Provides the status of the DB subnet group.
    , dbshSubnets :: [Subnet]
      -- ^ Contains a list of Subnet elements.
    , dbshVpcId :: Maybe Text
      -- ^ Provides the VpcId of the DB subnet group.
    } deriving (Eq, Show, Generic)

instance ToQuery DBSubnetGroup

instance FromXML DBSubnetGroup where
    fromXMLOptions = xmlOptions

instance ToXML DBSubnetGroup where
    toXMLOptions = xmlOptions

-- | Contains the result of a successful invocation of the following actions:
-- CreateDBSnapshot DeleteDBSnapshot This data type is used as a response
-- element in the DescribeDBSnapshots action.
data DBSnapshot = DBSnapshot
    { dbsAllocatedStorage :: Maybe Int
      -- ^ Specifies the allocated storage size in gigabytes (GB).
    , dbsAvailabilityZone :: Maybe Text
      -- ^ Specifies the name of the Availability Zone the DB instance was located in
      -- at the time of the DB snapshot.
    , dbsDBInstanceIdentifier :: Maybe Text
      -- ^ Specifies the DB instance identifier of the DB instance this DB snapshot
      -- was created from.
    , dbsDBSnapshotIdentifier :: Maybe Text
      -- ^ Specifies the identifier for the DB snapshot.
    , dbsEngine :: Maybe Text
      -- ^ Specifies the name of the database engine.
    , dbsEngineVersion :: Maybe Text
      -- ^ Specifies the version of the database engine.
    , dbsInstanceCreateTime :: Maybe UTCTime
      -- ^ Specifies the time (UTC) when the snapshot was taken.
    , dbsIops :: Maybe Int
      -- ^ Specifies the Provisioned IOPS (I/O operations per second) value of the DB
      -- instance at the time of the snapshot.
    , dbsLicenseModel :: Maybe Text
      -- ^ License model information for the restored DB instance.
    , dbsMasterUsername :: Maybe Text
      -- ^ Provides the master username for the DB snapshot.
    , dbsOptionGroupName :: Maybe Text
      -- ^ Provides the option group name for the DB snapshot.
    , dbsPercentProgress :: Maybe Int
      -- ^ The percentage of the estimated data that has been transferred.
    , dbsPort :: Maybe Int
      -- ^ Specifies the port that the database engine was listening on at the time of
      -- the snapshot.
    , dbsSnapshotCreateTime :: Maybe UTCTime
      -- ^ Provides the time (UTC) when the snapshot was taken.
    , dbsSnapshotType :: Maybe Text
      -- ^ Provides the type of the DB snapshot.
    , dbsSourceRegion :: Maybe Text
      -- ^ The region that the DB snapshot was created in or copied from.
    , dbsStatus :: Maybe Text
      -- ^ Specifies the status of this DB snapshot.
    , dbsVpcId :: Maybe Text
      -- ^ Provides the Vpc Id associated with the DB snapshot.
    } deriving (Eq, Show, Generic)

instance ToQuery DBSnapshot

instance FromXML DBSnapshot where
    fromXMLOptions = xmlOptions

instance ToXML DBSnapshot where
    toXMLOptions = xmlOptions

-- | This data type is used as a response element in the following actions:
-- ModifyDBInstance RebootDBInstance RestoreDBInstanceFromDBSnapshot
-- RestoreDBInstanceToPointInTime.
data DBSecurityGroupMembership = DBSecurityGroupMembership
    { dbsgmDBSecurityGroupName :: Maybe Text
      -- ^ The name of the DB security group.
    , dbsgmStatus :: Maybe Text
      -- ^ The status of the DB security group.
    } deriving (Eq, Show, Generic)

instance ToQuery DBSecurityGroupMembership

instance FromXML DBSecurityGroupMembership where
    fromXMLOptions = xmlOptions

instance ToXML DBSecurityGroupMembership where
    toXMLOptions = xmlOptions

-- | Contains the result of a successful invocation of the following actions:
-- DescribeDBSecurityGroups AuthorizeDBSecurityGroupIngress
-- CreateDBSecurityGroup RevokeDBSecurityGroupIngress This data type is used
-- as a response element in the DescribeDBSecurityGroups action.
data DBSecurityGroup = DBSecurityGroup
    { dbsgDBSecurityGroupDescription :: Maybe Text
      -- ^ Provides the description of the DB security group.
    , dbsgDBSecurityGroupName :: Maybe Text
      -- ^ Specifies the name of the DB security group.
    , dbsgEC2SecurityGroups :: [EC2SecurityGroup]
      -- ^ Contains a list of EC2SecurityGroup elements.
    , dbsgIPRanges :: [IPRange]
      -- ^ Contains a list of IPRange elements.
    , dbsgOwnerId :: Maybe Text
      -- ^ Provides the AWS ID of the owner of a specific DB security group.
    , dbsgVpcId :: Maybe Text
      -- ^ Provides the VpcId of the DB security group.
    } deriving (Eq, Show, Generic)

instance ToQuery DBSecurityGroup

instance FromXML DBSecurityGroup where
    fromXMLOptions = xmlOptions

instance ToXML DBSecurityGroup where
    toXMLOptions = xmlOptions

-- | The status of the DB parameter group. This data type is used as a response
-- element in the following actions: CreateDBInstance
-- CreateDBInstanceReadReplica DeleteDBInstance ModifyDBInstance
-- RebootDBInstance RestoreDBInstanceFromDBSnapshot.
data DBParameterGroupStatus = DBParameterGroupStatus
    { dbpgsDBParameterGroupName :: Maybe Text
      -- ^ The name of the DP parameter group.
    , dbpgsParameterApplyStatus :: Maybe Text
      -- ^ The status of parameter updates.
    } deriving (Eq, Show, Generic)

instance ToQuery DBParameterGroupStatus

instance FromXML DBParameterGroupStatus where
    fromXMLOptions = xmlOptions

instance ToXML DBParameterGroupStatus where
    toXMLOptions = xmlOptions

-- | Contains the result of a successful invocation of the
-- CreateDBParameterGroup action. This data type is used as a request
-- parameter in the DeleteDBParameterGroup action, and as a response element
-- in the DescribeDBParameterGroups action.
data DBParameterGroup = DBParameterGroup
    { dbpgDBParameterGroupFamily :: Maybe Text
      -- ^ Provides the name of the DB parameter group family that this DB parameter
      -- group is compatible with.
    , dbpgDBParameterGroupName :: Maybe Text
      -- ^ Provides the name of the DB parameter group.
    , dbpgDescription :: Maybe Text
      -- ^ Provides the customer-specified description for this DB parameter group.
    } deriving (Eq, Show, Generic)

instance ToQuery DBParameterGroup

instance FromXML DBParameterGroup where
    fromXMLOptions = xmlOptions

instance ToXML DBParameterGroup where
    toXMLOptions = xmlOptions

-- | Provides a list of status information for a DB instance.
data DBInstanceStatusInfo = DBInstanceStatusInfo
    { dbisiMessage :: Maybe Text
      -- ^ Details of the error if there is an error for the instance. If the instance
      -- is not in an error state, this value is blank.
    , dbisiNormal :: Maybe Bool
      -- ^ Boolean value that is true if the instance is operating normally, or false
      -- if the instance is in an error state.
    , dbisiStatus :: Maybe Text
      -- ^ Status of the DB instance. For a StatusType of read replica, the values can
      -- be replicating, error, stopped, or terminated.
    , dbisiStatusType :: Maybe Text
      -- ^ This value is currently "read replication.".
    } deriving (Eq, Show, Generic)

instance ToQuery DBInstanceStatusInfo

instance FromXML DBInstanceStatusInfo where
    fromXMLOptions = xmlOptions

instance ToXML DBInstanceStatusInfo where
    toXMLOptions = xmlOptions

-- | Contains the result of a successful invocation of the following actions:
-- CreateDBInstance DeleteDBInstance ModifyDBInstance This data type is used
-- as a response element in the DescribeDBInstances action.
data DBInstance = DBInstance
    { dbiAllocatedStorage :: Maybe Int
      -- ^ Specifies the allocated storage size specified in gigabytes.
    , dbiAutoMinorVersionUpgrade :: Maybe Bool
      -- ^ Indicates that minor version patches are applied automatically.
    , dbiAvailabilityZone :: Maybe Text
      -- ^ Specifies the name of the Availability Zone the DB instance is located in.
    , dbiBackupRetentionPeriod :: Maybe Int
      -- ^ Specifies the number of days for which automatic DB snapshots are retained.
    , dbiCharacterSetName :: Maybe Text
      -- ^ If present, specifies the name of the character set that this instance is
      -- associated with.
    , dbiDBInstanceClass :: Maybe Text
      -- ^ Contains the name of the compute and memory capacity class of the DB
      -- instance.
    , dbiDBInstanceIdentifier :: Maybe Text
      -- ^ Contains a user-supplied database identifier. This is the unique key that
      -- identifies a DB instance.
    , dbiDBInstanceStatus :: Maybe Text
      -- ^ Specifies the current state of this database.
    , dbiDBName :: Maybe Text
      -- ^ The meaning of this parameter differs according to the database engine you
      -- use. For example, this value returns only MySQL information when returning
      -- values from CreateDBInstanceReadReplica since read replicas are only
      -- supported for MySQL. MySQL Contains the name of the initial database of
      -- this instance that was provided at create time, if one was specified when
      -- the DB instance was created. This same name is returned for the life of the
      -- DB instance. Type: String Oracle Contains the Oracle System ID (SID) of the
      -- created DB instance. Not shown when the returned parameters do not apply to
      -- an Oracle DB instance.
    , dbiDBParameterGroups :: [DBParameterGroupStatus]
      -- ^ Provides the list of DB parameter groups applied to this DB instance.
    , dbiDBSecurityGroups :: [DBSecurityGroupMembership]
      -- ^ Provides List of DB security group elements containing only
      -- DBSecurityGroup.Name and DBSecurityGroup.Status subelements.
    , dbiDBSubnetGroup :: Maybe DBSubnetGroup
      -- ^ Specifies information on the subnet group associated with the DB instance,
      -- including the name, description, and subnets in the subnet group.
    , dbiEndpoint :: Maybe Endpoint
      -- ^ Specifies the connection endpoint.
    , dbiEngine :: Maybe Text
      -- ^ Provides the name of the database engine to be used for this DB instance.
    , dbiEngineVersion :: Maybe Text
      -- ^ Indicates the database engine version.
    , dbiInstanceCreateTime :: Maybe UTCTime
      -- ^ Provides the date and time the DB instance was created.
    , dbiIops :: Maybe Int
      -- ^ Specifies the Provisioned IOPS (I/O operations per second) value.
    , dbiLatestRestorableTime :: Maybe UTCTime
      -- ^ Specifies the latest time to which a database can be restored with
      -- point-in-time restore.
    , dbiLicenseModel :: Maybe Text
      -- ^ License model information for this DB instance.
    , dbiMasterUsername :: Maybe Text
      -- ^ Contains the master username for the DB instance.
    , dbiMultiAZ :: Maybe Bool
      -- ^ Specifies if the DB instance is a Multi-AZ deployment.
    , dbiOptionGroupMemberships :: [OptionGroupMembership]
      -- ^ Provides the list of option group memberships for this DB instance.
    , dbiPendingModifiedValues :: Maybe PendingModifiedValues
      -- ^ Specifies that changes to the DB instance are pending. This element is only
      -- included when changes are pending. Specific changes are identified by
      -- subelements.
    , dbiPreferredBackupWindow :: Maybe Text
      -- ^ Specifies the daily time range during which automated backups are created
      -- if automated backups are enabled, as determined by the
      -- BackupRetentionPeriod.
    , dbiPreferredMaintenanceWindow :: Maybe Text
      -- ^ Specifies the weekly time range (in UTC) during which system maintenance
      -- can occur.
    , dbiPubliclyAccessible :: Maybe Bool
      -- ^ Specifies the accessibility options for the DB instance. A value of true
      -- specifies an Internet-facing instance with a publicly resolvable DNS name,
      -- which resolves to a public IP address. A value of false specifies an
      -- internal instance with a DNS name that resolves to a private IP address.
      -- Default: The default behavior varies depending on whether a VPC has been
      -- requested or not. The following list shows the default behavior in each
      -- case. Default VPC:true VPC:false If no DB subnet group has been specified
      -- as part of the request and the PubliclyAccessible value has not been set,
      -- the DB instance will be publicly accessible. If a specific DB subnet group
      -- has been specified as part of the request and the PubliclyAccessible value
      -- has not been set, the DB instance will be private.
    , dbiReadReplicaDBInstanceIdentifiers :: [Text]
      -- ^ Contains one or more identifiers of the read replicas associated with this
      -- DB instance.
    , dbiReadReplicaSourceDBInstanceIdentifier :: Maybe Text
      -- ^ Contains the identifier of the source DB instance if this DB instance is a
      -- read replica.
    , dbiSecondaryAvailabilityZone :: Maybe Text
      -- ^ If present, specifies the name of the secondary Availability Zone for a DB
      -- instance with multi-AZ support.
    , dbiStatusInfos :: [DBInstanceStatusInfo]
      -- ^ The status of a read replica. If the instance is not a read replica, this
      -- will be blank.
    , dbiVpcSecurityGroups :: [VpcSecurityGroupMembership]
      -- ^ Provides List of VPC security group elements that the DB instance belongs
      -- to.
    } deriving (Eq, Show, Generic)

instance ToQuery DBInstance

instance FromXML DBInstance where
    fromXMLOptions = xmlOptions

instance ToXML DBInstance where
    toXMLOptions = xmlOptions

-- | This data type is used as a response element in the action
-- DescribeDBEngineVersions.
data DBEngineVersion = DBEngineVersion
    { dbevDBEngineDescription :: Maybe Text
      -- ^ The description of the database engine.
    , dbevDBEngineVersionDescription :: Maybe Text
      -- ^ The description of the database engine version.
    , dbevDBParameterGroupFamily :: Maybe Text
      -- ^ The name of the DB parameter group family for the database engine.
    , dbevDefaultCharacterSet :: Maybe CharacterSet
      -- ^ The default character set for new instances of this engine version, if the
      -- CharacterSetName parameter of the CreateDBInstance API is not specified.
    , dbevEngine :: Maybe Text
      -- ^ The name of the database engine.
    , dbevEngineVersion :: Maybe Text
      -- ^ The version number of the database engine.
    , dbevSupportedCharacterSets :: [CharacterSet]
      -- ^ A list of the character sets supported by this engine for the
      -- CharacterSetName parameter of the CreateDBInstance API.
    } deriving (Eq, Show, Generic)

instance ToQuery DBEngineVersion

instance FromXML DBEngineVersion where
    fromXMLOptions = xmlOptions

instance ToXML DBEngineVersion where
    toXMLOptions = xmlOptions

-- | The default character set for new instances of this engine version, if the
-- CharacterSetName parameter of the CreateDBInstance API is not specified.
data CharacterSet = CharacterSet
    { csCharacterSetDescription :: Maybe Text
      -- ^ The description of the character set.
    , csCharacterSetName :: Maybe Text
      -- ^ The name of the character set.
    } deriving (Eq, Show, Generic)

instance ToQuery CharacterSet

instance FromXML CharacterSet where
    fromXMLOptions = xmlOptions

instance ToXML CharacterSet where
    toXMLOptions = xmlOptions

-- | Contains Availability Zone information. This data type is used as an
-- element in the following data type: OrderableDBInstanceOption.
data AvailabilityZone = AvailabilityZone
    { azName :: Maybe Text
      -- ^ The name of the availability zone.
    , azProvisionedIopsCapable :: Maybe Bool
      -- ^ True indicates the availability zone is capable of provisioned IOPs.
    } deriving (Eq, Show, Generic)

instance ToQuery AvailabilityZone

instance FromXML AvailabilityZone where
    fromXMLOptions = xmlOptions

instance ToXML AvailabilityZone where
    toXMLOptions = xmlOptions

-- | The event source to retrieve events for. If no value is specified, all
-- events are returned.
data SourceType
    = SourceTypeDbInstance
    | SourceTypeDbParameterGroup
    | SourceTypeDbSecurityGroup
    | SourceTypeDbSnapshot
      deriving (Eq, Ord, Generic)

instance Hashable SourceType

instance FromText SourceType where
    fromText "db-instance" = Right SourceTypeDbInstance
    fromText "db-parameter-group" = Right SourceTypeDbParameterGroup
    fromText "db-security-group" = Right SourceTypeDbSecurityGroup
    fromText "db-snapshot" = Right SourceTypeDbSnapshot
    fromText e = fromTextFail $ "Unrecognised SourceType: " <> e

instance Read SourceType where
    readsPrec _ = fromTextRead

instance ToText SourceType where
    toText SourceTypeDbInstance = "db-instance"
    toText SourceTypeDbParameterGroup = "db-parameter-group"
    toText SourceTypeDbSecurityGroup = "db-security-group"
    toText SourceTypeDbSnapshot = "db-snapshot"

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

-- | Indicates when to apply parameter updates.
data ApplyMethod
    = ApplyMethodImmediate
    | ApplyMethodPendingReboot
      deriving (Eq, Ord, Generic)

instance Hashable ApplyMethod

instance FromText ApplyMethod where
    fromText "immediate" = Right ApplyMethodImmediate
    fromText "pending-reboot" = Right ApplyMethodPendingReboot
    fromText e = fromTextFail $ "Unrecognised ApplyMethod: " <> e

instance Read ApplyMethod where
    readsPrec _ = fromTextRead

instance ToText ApplyMethod where
    toText ApplyMethodImmediate = "immediate"
    toText ApplyMethodPendingReboot = "pending-reboot"

instance Show ApplyMethod where
    show = toTextShow

instance ToQuery ApplyMethod where
    toQuery = toTextQuery

instance FromXML ApplyMethod where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML ApplyMethod where
    toXMLOptions = xmlOptions
    toXML        = toTextXML
