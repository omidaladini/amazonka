{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.RDS.V2013_09_09.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Relational Database Service (Amazon RDS) is a web service that makes
-- it easy to set up, operate, and scale a relational database in the cloud.
-- It provides cost-efficient and resizable capacity while managing
-- time-consuming database administration tasks, freeing you up to focus on
-- your applications and business.
module Network.AWS.RDS.V2013_09_09.Types where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2013-09-09@) of the
-- @Amazon Relational Database Service@ service.
data RDS deriving (Typeable)

instance AWSService RDS where
    type Sg RDS = V4
    data Er RDS

        = AuthorizationAlreadyExistsFault

        | AuthorizationNotFoundFault

        | AuthorizationQuotaExceededFault

        | DBInstanceAlreadyExistsFault

        | DBInstanceNotFoundFault

        | DBParameterGroupAlreadyExistsFault

        | DBParameterGroupNotFoundFault

        | DBParameterGroupQuotaExceededFault

        | DBSecurityGroupAlreadyExistsFault

        | DBSecurityGroupNotFoundFault

        | DBSecurityGroupNotSupportedFault

        | DBSecurityGroupQuotaExceededFault

        | DBSnapshotAlreadyExistsFault

        | DBSnapshotNotFoundFault

        | DBSubnetGroupAlreadyExistsFault

        | DBSubnetGroupDoesNotCoverEnoughAZs

        | DBSubnetGroupNotAllowedFault

        | DBSubnetGroupNotFoundFault

        | DBSubnetGroupQuotaExceededFault

        | DBSubnetQuotaExceededFault

        | DBUpgradeDependencyFailureFault

        | EventSubscriptionQuotaExceededFault

        | InstanceQuotaExceededFault

        | InsufficientDBInstanceCapacityFault

        | InvalidDBInstanceStateFault

        | InvalidDBParameterGroupStateFault

        | InvalidDBSecurityGroupStateFault

        | InvalidDBSnapshotStateFault

        | InvalidDBSubnetGroupFault

        | InvalidDBSubnetGroupStateFault

        | InvalidDBSubnetStateFault

        | InvalidEventSubscriptionStateFault

        | InvalidOptionGroupStateFault

        | InvalidRestoreFault

        | InvalidSubnet

        | InvalidVPCNetworkStateFault

        | OptionGroupAlreadyExistsFault

        | OptionGroupNotFoundFault

        | OptionGroupQuotaExceededFault

        | PointInTimeRestoreNotEnabledFault

        | ProvisionedIopsNotAvailableInAZFault

        | RDSClient HttpException
        | RDSSerializer String
        | RDSService String
        | ReservedDBInstanceAlreadyExistsFault

        | ReservedDBInstanceNotFoundFault

        | ReservedDBInstanceQuotaExceededFault

        | ReservedDBInstancesOfferingNotFoundFault

        | SNSInvalidTopicFault

        | SNSNoAuthorizationFault

        | SNSTopicArnNotFoundFault

        | SnapshotQuotaExceededFault

        | SourceNotFoundFault

        | StorageQuotaExceededFault

        | SubnetAlreadyInUse

        | SubscriptionAlreadyExistFault

        | SubscriptionCategoryNotFoundFault

        | SubscriptionNotFoundFault

    service = Service
        { _svcEndpoint = Regional
        , _svcPrefix   = "rds"
        , _svcVersion  = "2013-09-09"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er RDS)
deriving instance Generic (Er RDS)

instance AWSError (Er RDS) where
    awsError = const "RDSError"

instance ServiceError (Er RDS) where
    serviceError    = RDSService
    clientError     = RDSClient
    serializerError = RDSSerializer

instance Exception (Er RDS)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlNamespace = Just "http://rds.amazonaws.com/doc/2013-09-09/"
    }

-- | Indicates when to apply parameter updates.
data ApplyMethod
    = ApplyMethodImmediate -- ^ immediate
    | ApplyMethodPendingReboot -- ^ pending-reboot
      deriving (Eq, Show, Generic)

instance Hashable ApplyMethod

instance FromText ApplyMethod where
    parser = match "immediate" ApplyMethodImmediate
         <|> match "pending-reboot" ApplyMethodPendingReboot

instance ToText ApplyMethod where
    toText ApplyMethodImmediate = "immediate"
    toText ApplyMethodPendingReboot = "pending-reboot"

instance ToByteString ApplyMethod

instance FromXML ApplyMethod where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ApplyMethod"

instance ToQuery ApplyMethod where
    toQuery = genericToQuery def

-- | The event source to retrieve events for. If no value is specified, all
-- events are returned.
data SourceType
    = SourceTypeDbInstance -- ^ db-instance
    | SourceTypeDbParameterGroup -- ^ db-parameter-group
    | SourceTypeDbSecurityGroup -- ^ db-security-group
    | SourceTypeDbSnapshot -- ^ db-snapshot
      deriving (Eq, Show, Generic)

instance Hashable SourceType

instance FromText SourceType where
    parser = match "db-instance" SourceTypeDbInstance
         <|> match "db-parameter-group" SourceTypeDbParameterGroup
         <|> match "db-security-group" SourceTypeDbSecurityGroup
         <|> match "db-snapshot" SourceTypeDbSnapshot

instance ToText SourceType where
    toText SourceTypeDbInstance = "db-instance"
    toText SourceTypeDbParameterGroup = "db-parameter-group"
    toText SourceTypeDbSecurityGroup = "db-security-group"
    toText SourceTypeDbSnapshot = "db-snapshot"

instance ToByteString SourceType

instance FromXML SourceType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SourceType"

instance ToQuery SourceType where
    toQuery = genericToQuery def

-- | Contains Availability Zone information. This data type is used as an
-- element in the following data type: OrderableDBInstanceOption.
data AvailabilityZone = AvailabilityZone
    { _azProvisionedIopsCapable :: Maybe Bool
      -- ^ True indicates the availability zone is capable of provisioned
      -- IOPs.
    , _azName :: Maybe Text
      -- ^ The name of the availability zone.
    } deriving (Generic)

instance FromXML AvailabilityZone where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AvailabilityZone"

instance ToQuery AvailabilityZone where
    toQuery = genericToQuery def

-- | The default character set for new instances of this engine version, if the
-- CharacterSetName parameter of the CreateDBInstance API is not specified.
data CharacterSet = CharacterSet
    { _csCharacterSetName :: Maybe Text
      -- ^ The name of the character set.
    , _csCharacterSetDescription :: Maybe Text
      -- ^ The description of the character set.
    } deriving (Generic)

instance FromXML CharacterSet where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CharacterSet"

instance ToQuery CharacterSet where
    toQuery = genericToQuery def

-- | This data type is used as a response element in the action
-- DescribeDBEngineVersions.
data DBEngineVersion = DBEngineVersion
    { _dbevEngineVersion :: Maybe Text
      -- ^ The version number of the database engine.
    , _dbevDBEngineVersionDescription :: Maybe Text
      -- ^ The description of the database engine version.
    , _dbevDefaultCharacterSet :: Maybe CharacterSet
      -- ^ The default character set for new instances of this engine
      -- version, if the CharacterSetName parameter of the
      -- CreateDBInstance API is not specified.
    , _dbevEngine :: Maybe Text
      -- ^ The name of the database engine.
    , _dbevDBParameterGroupFamily :: Maybe Text
      -- ^ The name of the DB parameter group family for the database
      -- engine.
    , _dbevSupportedCharacterSets :: [CharacterSet]
      -- ^ A list of the character sets supported by this engine for the
      -- CharacterSetName parameter of the CreateDBInstance API.
    , _dbevDBEngineDescription :: Maybe Text
      -- ^ The description of the database engine.
    } deriving (Generic)

instance FromXML DBEngineVersion where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBEngineVersion"

-- | Contains the result of a successful invocation of the following actions:
-- CreateDBInstance DeleteDBInstance ModifyDBInstance This data type is used
-- as a response element in the DescribeDBInstances action.
data DBInstance = DBInstance
    { _dbiEngineVersion :: Maybe Text
      -- ^ Indicates the database engine version.
    , _dbiDBSecurityGroups :: [DBSecurityGroupMembership]
      -- ^ Provides List of DB security group elements containing only
      -- DBSecurityGroup.Name and DBSecurityGroup.Status subelements.
    , _dbiPubliclyAccessible :: Maybe Bool
      -- ^ Specifies the accessibility options for the DB instance. A value
      -- of true specifies an Internet-facing instance with a publicly
      -- resolvable DNS name, which resolves to a public IP address. A
      -- value of false specifies an internal instance with a DNS name
      -- that resolves to a private IP address. Default: The default
      -- behavior varies depending on whether a VPC has been requested or
      -- not. The following list shows the default behavior in each case.
      -- Default VPC:true VPC:false If no DB subnet group has been
      -- specified as part of the request and the PubliclyAccessible value
      -- has not been set, the DB instance will be publicly accessible. If
      -- a specific DB subnet group has been specified as part of the
      -- request and the PubliclyAccessible value has not been set, the DB
      -- instance will be private.
    , _dbiAutoMinorVersionUpgrade :: Maybe Bool
      -- ^ Indicates that minor version patches are applied automatically.
    , _dbiMasterUsername :: Maybe Text
      -- ^ Contains the master username for the DB instance.
    , _dbiReadReplicaDBInstanceIdentifiers :: [Text]
      -- ^ Contains one or more identifiers of the read replicas associated
      -- with this DB instance.
    , _dbiIops :: Maybe Integer
      -- ^ Specifies the Provisioned IOPS (I/O operations per second) value.
    , _dbiInstanceCreateTime :: Maybe ISO8601
      -- ^ Provides the date and time the DB instance was created.
    , _dbiReadReplicaSourceDBInstanceIdentifier :: Maybe Text
      -- ^ Contains the identifier of the source DB instance if this DB
      -- instance is a read replica.
    , _dbiEngine :: Maybe Text
      -- ^ Provides the name of the database engine to be used for this DB
      -- instance.
    , _dbiLatestRestorableTime :: Maybe ISO8601
      -- ^ Specifies the latest time to which a database can be restored
      -- with point-in-time restore.
    , _dbiDBInstanceClass :: Maybe Text
      -- ^ Contains the name of the compute and memory capacity class of the
      -- DB instance.
    , _dbiLicenseModel :: Maybe Text
      -- ^ License model information for this DB instance.
    , _dbiPreferredMaintenanceWindow :: Maybe Text
      -- ^ Specifies the weekly time range (in UTC) during which system
      -- maintenance can occur.
    , _dbiDBInstanceIdentifier :: Maybe Text
      -- ^ Contains a user-supplied database identifier. This is the unique
      -- key that identifies a DB instance.
    , _dbiCharacterSetName :: Maybe Text
      -- ^ If present, specifies the name of the character set that this
      -- instance is associated with.
    , _dbiPreferredBackupWindow :: Maybe Text
      -- ^ Specifies the daily time range during which automated backups are
      -- created if automated backups are enabled, as determined by the
      -- BackupRetentionPeriod.
    , _dbiAvailabilityZone :: Maybe Text
      -- ^ Specifies the name of the Availability Zone the DB instance is
      -- located in.
    , _dbiVpcSecurityGroups :: [VpcSecurityGroupMembership]
      -- ^ Provides List of VPC security group elements that the DB instance
      -- belongs to.
    , _dbiBackupRetentionPeriod :: Maybe Integer
      -- ^ Specifies the number of days for which automatic DB snapshots are
      -- retained.
    , _dbiDBSubnetGroup :: Maybe DBSubnetGroup
      -- ^ Specifies information on the subnet group associated with the DB
      -- instance, including the name, description, and subnets in the
      -- subnet group.
    , _dbiMultiAZ :: Maybe Bool
      -- ^ Specifies if the DB instance is a Multi-AZ deployment.
    , _dbiOptionGroupMemberships :: [OptionGroupMembership]
      -- ^ Provides the list of option group memberships for this DB
      -- instance.
    , _dbiSecondaryAvailabilityZone :: Maybe Text
      -- ^ If present, specifies the name of the secondary Availability Zone
      -- for a DB instance with multi-AZ support.
    , _dbiAllocatedStorage :: Maybe Integer
      -- ^ Specifies the allocated storage size specified in gigabytes.
    , _dbiDBParameterGroups :: [DBParameterGroupStatus]
      -- ^ Provides the list of DB parameter groups applied to this DB
      -- instance.
    , _dbiEndpoint :: Maybe Endpoint
      -- ^ Specifies the connection endpoint.
    , _dbiDBInstanceStatus :: Maybe Text
      -- ^ Specifies the current state of this database.
    , _dbiPendingModifiedValues :: Maybe PendingModifiedValues
      -- ^ Specifies that changes to the DB instance are pending. This
      -- element is only included when changes are pending. Specific
      -- changes are identified by subelements.
    , _dbiStatusInfos :: [DBInstanceStatusInfo]
      -- ^ The status of a read replica. If the instance is not a read
      -- replica, this will be blank.
    , _dbiDBName :: Maybe Text
      -- ^ The meaning of this parameter differs according to the database
      -- engine you use. For example, this value returns only MySQL
      -- information when returning values from
      -- CreateDBInstanceReadReplica since read replicas are only
      -- supported for MySQL. MySQL Contains the name of the initial
      -- database of this instance that was provided at create time, if
      -- one was specified when the DB instance was created. This same
      -- name is returned for the life of the DB instance. Type: String
      -- Oracle Contains the Oracle System ID (SID) of the created DB
      -- instance. Not shown when the returned parameters do not apply to
      -- an Oracle DB instance.
    } deriving (Generic)

instance FromXML DBInstance where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBInstance"

-- | Provides a list of status information for a DB instance.
data DBInstanceStatusInfo = DBInstanceStatusInfo
    { _dbisiStatus :: Maybe Text
      -- ^ Status of the DB instance. For a StatusType of read replica, the
      -- values can be replicating, error, stopped, or terminated.
    , _dbisiNormal :: Maybe Bool
      -- ^ Boolean value that is true if the instance is operating normally,
      -- or false if the instance is in an error state.
    , _dbisiStatusType :: Maybe Text
      -- ^ This value is currently "read replication.".
    , _dbisiMessage :: Maybe Text
      -- ^ Details of the error if there is an error for the instance. If
      -- the instance is not in an error state, this value is blank.
    } deriving (Generic)

instance FromXML DBInstanceStatusInfo where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBInstanceStatusInfo"

instance ToQuery DBInstanceStatusInfo where
    toQuery = genericToQuery def

-- | Contains the result of a successful invocation of the
-- CreateDBParameterGroup action. This data type is used as a request
-- parameter in the DeleteDBParameterGroup action, and as a response element
-- in the DescribeDBParameterGroups action.
data DBParameterGroup = DBParameterGroup
    { _dbpgDBParameterGroupFamily :: Maybe Text
      -- ^ Provides the name of the DB parameter group family that this DB
      -- parameter group is compatible with.
    , _dbpgDBParameterGroupName :: Maybe Text
      -- ^ Provides the name of the DB parameter group.
    , _dbpgDescription :: Maybe Text
      -- ^ Provides the customer-specified description for this DB parameter
      -- group.
    } deriving (Generic)

instance FromXML DBParameterGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBParameterGroup"

-- | The status of the DB parameter group. This data type is used as a response
-- element in the following actions: CreateDBInstance
-- CreateDBInstanceReadReplica DeleteDBInstance ModifyDBInstance
-- RebootDBInstance RestoreDBInstanceFromDBSnapshot.
data DBParameterGroupStatus = DBParameterGroupStatus
    { _dbpgsDBParameterGroupName :: Maybe Text
      -- ^ The name of the DP parameter group.
    , _dbpgsParameterApplyStatus :: Maybe Text
      -- ^ The status of parameter updates.
    } deriving (Generic)

instance FromXML DBParameterGroupStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBParameterGroup"

instance ToQuery DBParameterGroupStatus where
    toQuery = genericToQuery def

-- | Contains the result of a successful invocation of the following actions:
-- DescribeDBSecurityGroups AuthorizeDBSecurityGroupIngress
-- CreateDBSecurityGroup RevokeDBSecurityGroupIngress This data type is used
-- as a response element in the DescribeDBSecurityGroups action.
data DBSecurityGroup = DBSecurityGroup
    { _dbslVpcId :: Maybe Text
      -- ^ Provides the VpcId of the DB security group.
    , _dbslOwnerId :: Maybe Text
      -- ^ Provides the AWS ID of the owner of a specific DB security group.
    , _dbslIPRanges :: [IPRange]
      -- ^ Contains a list of IPRange elements.
    , _dbslDBSecurityGroupName :: Maybe Text
      -- ^ Specifies the name of the DB security group.
    , _dbslEC2SecurityGroups :: [EC2SecurityGroup]
      -- ^ Contains a list of EC2SecurityGroup elements.
    , _dbslDBSecurityGroupDescription :: Maybe Text
      -- ^ Provides the description of the DB security group.
    } deriving (Generic)

instance FromXML DBSecurityGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBSecurityGroup"

-- | This data type is used as a response element in the following actions:
-- ModifyDBInstance RebootDBInstance RestoreDBInstanceFromDBSnapshot
-- RestoreDBInstanceToPointInTime.
data DBSecurityGroupMembership = DBSecurityGroupMembership
    { _dbsgmStatus :: Maybe Text
      -- ^ The status of the DB security group.
    , _dbsgmDBSecurityGroupName :: Maybe Text
      -- ^ The name of the DB security group.
    } deriving (Generic)

instance FromXML DBSecurityGroupMembership where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBSecurityGroup"

instance ToQuery DBSecurityGroupMembership where
    toQuery = genericToQuery def

-- | Contains the result of a successful invocation of the following actions:
-- CreateDBSnapshot DeleteDBSnapshot This data type is used as a response
-- element in the DescribeDBSnapshots action.
data DBSnapshot = DBSnapshot
    { _dbsEngineVersion :: Maybe Text
      -- ^ Specifies the version of the database engine.
    , _dbsStatus :: Maybe Text
      -- ^ Specifies the status of this DB snapshot.
    , _dbsMasterUsername :: Maybe Text
      -- ^ Provides the master username for the DB snapshot.
    , _dbsSourceRegion :: Maybe Text
      -- ^ The region that the DB snapshot was created in or copied from.
    , _dbsIops :: Maybe Integer
      -- ^ Specifies the Provisioned IOPS (I/O operations per second) value
      -- of the DB instance at the time of the snapshot.
    , _dbsVpcId :: Maybe Text
      -- ^ Provides the Vpc Id associated with the DB snapshot.
    , _dbsInstanceCreateTime :: Maybe ISO8601
      -- ^ Specifies the time (UTC) when the snapshot was taken.
    , _dbsEngine :: Maybe Text
      -- ^ Specifies the name of the database engine.
    , _dbsDBSnapshotIdentifier :: Maybe Text
      -- ^ Specifies the identifier for the DB snapshot.
    , _dbsLicenseModel :: Maybe Text
      -- ^ License model information for the restored DB instance.
    , _dbsSnapshotType :: Maybe Text
      -- ^ Provides the type of the DB snapshot.
    , _dbsDBInstanceIdentifier :: Maybe Text
      -- ^ Specifies the DB instance identifier of the DB instance this DB
      -- snapshot was created from.
    , _dbsAvailabilityZone :: Maybe Text
      -- ^ Specifies the name of the Availability Zone the DB instance was
      -- located in at the time of the DB snapshot.
    , _dbsSnapshotCreateTime :: Maybe ISO8601
      -- ^ Provides the time (UTC) when the snapshot was taken.
    , _dbsAllocatedStorage :: Maybe Integer
      -- ^ Specifies the allocated storage size in gigabytes (GB).
    , _dbsOptionGroupName :: Maybe Text
      -- ^ Provides the option group name for the DB snapshot.
    , _dbsPercentProgress :: Maybe Integer
      -- ^ The percentage of the estimated data that has been transferred.
    , _dbsPort :: Maybe Integer
      -- ^ Specifies the port that the database engine was listening on at
      -- the time of the snapshot.
    } deriving (Generic)

instance FromXML DBSnapshot where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBSnapshot"

-- | Specifies information on the subnet group associated with the DB instance,
-- including the name, description, and subnets in the subnet group.
data DBSubnetGroup = DBSubnetGroup
    { _dbsgDBSubnetGroupName :: Maybe Text
      -- ^ Specifies the name of the DB subnet group.
    , _dbsgVpcId :: Maybe Text
      -- ^ Provides the VpcId of the DB subnet group.
    , _dbsgSubnets :: [Subnet]
      -- ^ Contains a list of Subnet elements.
    , _dbsgDBSubnetGroupDescription :: Maybe Text
      -- ^ Provides the description of the DB subnet group.
    , _dbsgSubnetGroupStatus :: Maybe Text
      -- ^ Provides the status of the DB subnet group.
    } deriving (Generic)

instance FromXML DBSubnetGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DBSubnetGroup"

instance ToQuery DBSubnetGroup where
    toQuery = genericToQuery def

-- | This data type is used as a response element to DescribeDBLogFiles.
data DescribeDBLogFilesDetails = DescribeDBLogFilesDetails
    { _ddblfdLastWritten :: Maybe Integer
      -- ^ A POSIX timestamp when the last log entry was written.
    , _ddblfdSize :: Maybe Integer
      -- ^ The size, in bytes, of the log file for the specified DB
      -- instance.
    , _ddblfdLogFileName :: Maybe Text
      -- ^ The name of the log file for the specified DB instance.
    } deriving (Generic)

instance FromXML DescribeDBLogFilesDetails where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeDBLogFilesDetails"

-- | This data type is used as a response element in the following actions:
-- AuthorizeDBSecurityGroupIngress DescribeDBSecurityGroups
-- RevokeDBSecurityGroupIngress.
data EC2SecurityGroup = EC2SecurityGroup
    { _ecsgStatus :: Maybe Text
      -- ^ Provides the status of the EC2 security group. Status can be
      -- "authorizing", "authorized", "revoking", and "revoked".
    , _ecsgEC2SecurityGroupOwnerId :: Maybe Text
      -- ^ Specifies the AWS ID of the owner of the EC2 security group
      -- specified in the EC2SecurityGroupName field.
    , _ecsgEC2SecurityGroupName :: Maybe Text
      -- ^ Specifies the name of the EC2 security group.
    , _ecsgEC2SecurityGroupId :: Maybe Text
      -- ^ Specifies the id of the EC2 security group.
    } deriving (Generic)

instance FromXML EC2SecurityGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EC2SecurityGroup"

instance ToQuery EC2SecurityGroup where
    toQuery = genericToQuery def

-- | Specifies the connection endpoint.
data Endpoint = Endpoint
    { _fAddress :: Maybe Text
      -- ^ Specifies the DNS address of the DB instance.
    , _fPort :: Maybe Integer
      -- ^ Specifies the port that the database engine is listening on.
    } deriving (Generic)

instance FromXML Endpoint where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Endpoint"

instance ToQuery Endpoint where
    toQuery = genericToQuery def

-- | Contains the result of a successful invocation of the
-- DescribeEngineDefaultParameters action.
data EngineDefaults = EngineDefaults
    { _edDBParameterGroupFamily :: Maybe Text
      -- ^ Specifies the name of the DB parameter group family which the
      -- engine default parameters apply to.
    , _edMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous
      -- EngineDefaults request. If this parameter is specified, the
      -- response includes only records beyond the marker, up to the value
      -- specified by MaxRecords .
    , _edParameters :: [Parameter]
      -- ^ Contains a list of engine default parameters.
    } deriving (Generic)

instance FromXML EngineDefaults where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EngineDefaults"

-- | This data type is used as a response element in the DescribeEvents action.
data Event = Event
    { _evSourceType :: Maybe SourceType
      -- ^ Specifies the source type for this event.
    , _evSourceIdentifier :: Maybe Text
      -- ^ Provides the identifier for the source of the event.
    , _evDate :: Maybe ISO8601
      -- ^ Specifies the date and time of the event.
    , _evEventCategories :: [Text]
      -- ^ Specifies the category for the event.
    , _evMessage :: Maybe Text
      -- ^ Provides the text of this event.
    } deriving (Generic)

instance FromXML Event where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Event"

-- | Contains the results of a successful invocation of the
-- DescribeEventCategories action.
data EventCategoriesMap = EventCategoriesMap
    { _ecwSourceType :: Maybe Text
      -- ^ The source type that the returned categories belong to.
    , _ecwEventCategories :: [Text]
      -- ^ The event categories for the specified source type.
    } deriving (Generic)

instance FromXML EventCategoriesMap where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EventCategoriesMap"

-- | Contains the results of a successful invocation of the
-- DescribeEventSubscriptions action.
data EventSubscription = EventSubscription
    { _esStatus :: Maybe Text
      -- ^ The status of the RDS event notification subscription.
      -- Constraints: Can be one of the following: creating | modifying |
      -- deleting | active | no-permission | topic-not-exist The status
      -- "no-permission" indicates that RDS no longer has permission to
      -- post to the SNS topic. The status "topic-not-exist" indicates
      -- that the topic was deleted after the subscription was created.
    , _esCustomerAwsId :: Maybe Text
      -- ^ The AWS customer account associated with the RDS event
      -- notification subscription.
    , _esCustSubscriptionId :: Maybe Text
      -- ^ The RDS event notification subscription Id.
    , _esSnsTopicArn :: Maybe Text
      -- ^ The topic ARN of the RDS event notification subscription.
    , _esEnabled :: Maybe Bool
      -- ^ A Boolean value indicating if the subscription is enabled. True
      -- indicates the subscription is enabled.
    , _esSourceType :: Maybe Text
      -- ^ The source type for the RDS event notification subscription.
    , _esSubscriptionCreationTime :: Maybe Text
      -- ^ The time the RDS event notification subscription was created.
    , _esEventCategoriesList :: [Text]
      -- ^ A list of event categories for the RDS event notification
      -- subscription.
    , _esSourceIdsList :: [Text]
      -- ^ A list of source Ids for the RDS event notification subscription.
    } deriving (Generic)

instance FromXML EventSubscription where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EventSubscription"

-- | This data type is used as a response element in the
-- DescribeDBSecurityGroups action.
data IPRange = IPRange
    { _iprStatus :: Maybe Text
      -- ^ Specifies the status of the IP range. Status can be
      -- "authorizing", "authorized", "revoking", and "revoked".
    , _iprCIDRIP :: Maybe Text
      -- ^ Specifies the IP range.
    } deriving (Generic)

instance FromXML IPRange where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "IPRange"

instance ToQuery IPRange where
    toQuery = genericToQuery def

-- | Option details.
data Option = Option
    { _onOptionName :: Maybe Text
      -- ^ The name of the option.
    , _onPermanent :: Maybe Bool
      -- ^ Indicate if this option is permanent.
    , _onPersistent :: Maybe Bool
      -- ^ Indicate if this option is persistent.
    , _onOptionDescription :: Maybe Text
      -- ^ The description of the option.
    , _onOptionSettings :: [OptionSetting]
      -- ^ The option settings for this option.
    , _onVpcSecurityGroupMemberships :: [VpcSecurityGroupMembership]
      -- ^ If the option requires access to a port, then this VPC security
      -- group allows access to the port.
    , _onDBSecurityGroupMemberships :: [DBSecurityGroupMembership]
      -- ^ If the option requires access to a port, then this DB security
      -- group allows access to the port.
    , _onPort :: Maybe Integer
      -- ^ If required, the port configured for this option to use.
    } deriving (Generic)

instance FromXML Option where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Option"

instance ToQuery Option where
    toQuery = genericToQuery def

-- | A list of all available options.
data OptionConfiguration = OptionConfiguration
    { _ocOptionName :: Text
      -- ^ The configuration of options to include in a group.
    , _ocOptionSettings :: [OptionSetting]
      -- ^ The option settings to include in an option group.
    , _ocVpcSecurityGroupMemberships :: [Text]
      -- ^ A list of VpcSecurityGroupMemebrship name strings used for this
      -- option.
    , _ocDBSecurityGroupMemberships :: [Text]
      -- ^ A list of DBSecurityGroupMemebrship name strings used for this
      -- option.
    , _ocPort :: Maybe Integer
      -- ^ The optional port for the option.
    } deriving (Generic)

instance ToQuery OptionConfiguration where
    toQuery = genericToQuery def

-- | 
data OptionGroup = OptionGroup
    { _ohOptionGroupDescription :: Maybe Text
      -- ^ Provides the description of the option group.
    , _ohVpcId :: Maybe Text
      -- ^ If AllowsVpcAndNonVpcInstanceMemberships is 'false', this field
      -- is blank. If AllowsVpcAndNonVpcInstanceMemberships is 'true' and
      -- this field is blank, then this option group can be applied to
      -- both VPC and non-VPC instances. If this field contains a value,
      -- then this option group can only be applied to instances that are
      -- in the VPC indicated by this field.
    , _ohAllowsVpcAndNonVpcInstanceMemberships :: Maybe Bool
      -- ^ Indicates whether this option group can be applied to both VPC
      -- and non-VPC instances. The value 'true' indicates the option
      -- group can be applied to both VPC and non-VPC instances.
    , _ohEngineName :: Maybe Text
      -- ^ Engine name that this option group can be applied to.
    , _ohMajorEngineVersion :: Maybe Text
      -- ^ Indicates the major engine version associated with this option
      -- group.
    , _ohOptions :: [Option]
      -- ^ Indicates what options are available in the option group.
    , _ohOptionGroupName :: Maybe Text
      -- ^ Specifies the name of the option group.
    } deriving (Generic)

instance FromXML OptionGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OptionGroup"

-- | Provides information on the option groups the DB instance is a member of.
data OptionGroupMembership = OptionGroupMembership
    { _ogmStatus :: Maybe Text
      -- ^ The status of the DB instance's option group membership (e.g.
      -- in-sync, pending, pending-maintenance, applying).
    , _ogmOptionGroupName :: Maybe Text
      -- ^ The name of the option group that the instance belongs to.
    } deriving (Generic)

instance FromXML OptionGroupMembership where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OptionGroupMembership"

instance ToQuery OptionGroupMembership where
    toQuery = genericToQuery def

-- | Available option.
data OptionGroupOption = OptionGroupOption
    { _ogqMinimumRequiredMinorEngineVersion :: Maybe Text
      -- ^ The minimum required engine version for the option to be applied.
    , _ogqPermanent :: Maybe Bool
      -- ^ A permanent option cannot be removed from the option group once
      -- the option group is used, and it cannot be removed from the db
      -- instance after assigning an option group with this permanent
      -- option.
    , _ogqPersistent :: Maybe Bool
      -- ^ A persistent option cannot be removed from the option group once
      -- the option group is used, but this option can be removed from the
      -- db instance while modifying the related data and assigning
      -- another option group without this option.
    , _ogqEngineName :: Maybe Text
      -- ^ Engine name that this option can be applied to.
    , _ogqMajorEngineVersion :: Maybe Text
      -- ^ Indicates the major engine version that the option is available
      -- for.
    , _ogqName :: Maybe Text
      -- ^ The name of the option.
    , _ogqDefaultPort :: Maybe Integer
      -- ^ If the option requires a port, specifies the default port for the
      -- option.
    , _ogqOptionGroupOptionSettings :: [OptionGroupOptionSetting]
      -- ^ Specifies the option settings that are available (and the default
      -- value) for each option in an option group.
    , _ogqPortRequired :: Maybe Bool
      -- ^ Specifies whether the option requires a port.
    , _ogqDescription :: Maybe Text
      -- ^ The description of the option.
    , _ogqOptionsDependedOn :: [Text]
      -- ^ List of all options that are prerequisites for this option.
    } deriving (Generic)

instance FromXML OptionGroupOption where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OptionGroupOption"

-- | option group option settings are used to display settings available for
-- each option with their default values and other information. These values
-- are used with the DescribeOptionGroupOptions action.
data OptionGroupOptionSetting = OptionGroupOptionSetting
    { _ogosApplyType :: Maybe Text
      -- ^ The DB engine specific parameter type for the option group
      -- option.
    , _ogosSettingName :: Maybe Text
      -- ^ The name of the option group option.
    , _ogosDefaultValue :: Maybe Text
      -- ^ The default value for the option group option.
    , _ogosIsModifiable :: Maybe Bool
      -- ^ Boolean value where true indicates that this option group option
      -- can be changed from the default value.
    , _ogosSettingDescription :: Maybe Text
      -- ^ The description of the option group option.
    , _ogosAllowedValues :: Maybe Text
      -- ^ Indicates the acceptable values for the option group option.
    } deriving (Generic)

instance FromXML OptionGroupOptionSetting where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OptionGroupOptionSetting"

instance ToQuery OptionGroupOptionSetting where
    toQuery = genericToQuery def

-- | Option settings are the actual settings being applied or configured for
-- that option. It is used when you modify an option group or describe option
-- groups. For example, the NATIVE_NETWORK_ENCRYPTION option has a setting
-- called SQLNET.ENCRYPTION_SERVER that can have several different values.
data OptionSetting = OptionSetting
    { _osIsCollection :: Maybe Bool
      -- ^ Indicates if the option setting is part of a collection.
    , _osApplyType :: Maybe Text
      -- ^ The DB engine specific parameter type.
    , _osValue :: Maybe Text
      -- ^ The current value of the option setting.
    , _osName :: Maybe Text
      -- ^ The name of the option that has settings that you can set.
    , _osDefaultValue :: Maybe Text
      -- ^ The default value of the option setting.
    , _osIsModifiable :: Maybe Bool
      -- ^ A Boolean value that, when true, indicates the option setting can
      -- be modified from the default.
    , _osDataType :: Maybe Text
      -- ^ The data type of the option setting.
    , _osAllowedValues :: Maybe Text
      -- ^ The allowed values of the option setting.
    , _osDescription :: Maybe Text
      -- ^ The description of the option setting.
    } deriving (Generic)

instance FromXML OptionSetting where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OptionSetting"

instance ToQuery OptionSetting where
    toQuery = genericToQuery def

-- | Contains a list of available options for a DB instance This data type is
-- used as a response element in the DescribeOrderableDBInstanceOptions
-- action.
data OrderableDBInstanceOption = OrderableDBInstanceOption
    { _odbioEngineVersion :: Maybe Text
      -- ^ The engine version of the orderable DB instance.
    , _odbioMultiAZCapable :: Maybe Bool
      -- ^ Indicates whether this orderable DB instance is multi-AZ capable.
    , _odbioEngine :: Maybe Text
      -- ^ The engine type of the orderable DB instance.
    , _odbioDBInstanceClass :: Maybe Text
      -- ^ The DB instance Class for the orderable DB instance.
    , _odbioLicenseModel :: Maybe Text
      -- ^ The license model for the orderable DB instance.
    , _odbioAvailabilityZones :: [AvailabilityZone]
      -- ^ A list of availability zones for the orderable DB instance.
    , _odbioReadReplicaCapable :: Maybe Bool
      -- ^ Indicates whether this orderable DB instance can have a read
      -- replica.
    , _odbioVpc :: Maybe Bool
      -- ^ Indicates whether this is a VPC orderable DB instance.
    } deriving (Generic)

instance FromXML OrderableDBInstanceOption where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "OrderableDBInstanceOption"

-- | This data type is used as a request parameter in the ModifyDBParameterGroup
-- and ResetDBParameterGroup actions. This data type is used as a response
-- element in the DescribeEngineDefaultParameters and DescribeDBParameters
-- actions.
data Parameter = Parameter
    { _prApplyType :: Maybe Text
      -- ^ Specifies the engine specific parameters type.
    , _prParameterValue :: Maybe Text
      -- ^ Specifies the value of the parameter.
    , _prApplyMethod :: Maybe ApplyMethod
      -- ^ Indicates when to apply parameter updates.
    , _prMinimumEngineVersion :: Maybe Text
      -- ^ The earliest engine version to which the parameter can apply.
    , _prSource :: Maybe Text
      -- ^ Indicates the source of the parameter value.
    , _prIsModifiable :: Maybe Bool
      -- ^ Indicates whether (true) or not (false) the parameter can be
      -- modified. Some parameters have security or operational
      -- implications that prevent them from being changed.
    , _prDataType :: Maybe Text
      -- ^ Specifies the valid data type for the parameter.
    , _prAllowedValues :: Maybe Text
      -- ^ Specifies the valid range of values for the parameter.
    , _prParameterName :: Maybe Text
      -- ^ Specifies the name of the parameter.
    , _prDescription :: Maybe Text
      -- ^ Provides a description of the parameter.
    } deriving (Generic)

instance FromXML Parameter where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Parameter"

instance ToQuery Parameter where
    toQuery = genericToQuery def

-- | Specifies that changes to the DB instance are pending. This element is only
-- included when changes are pending. Specific changes are identified by
-- subelements.
data PendingModifiedValues = PendingModifiedValues
    { _pmvEngineVersion :: Maybe Text
      -- ^ Indicates the database engine version.
    , _pmvMasterUserPassword :: Maybe Text
      -- ^ Contains the pending or in-progress change of the master
      -- credentials for the DB instance.
    , _pmvIops :: Maybe Integer
      -- ^ Specifies the new Provisioned IOPS value for the DB instance that
      -- will be applied or is being applied.
    , _pmvDBInstanceClass :: Maybe Text
      -- ^ Contains the new DBInstanceClass for the DB instance that will be
      -- applied or is in progress.
    , _pmvDBInstanceIdentifier :: Maybe Text
      -- ^ Contains the new DBInstanceIdentifier for the DB instance that
      -- will be applied or is in progress.
    , _pmvBackupRetentionPeriod :: Maybe Integer
      -- ^ Specifies the pending number of days for which automated backups
      -- are retained.
    , _pmvMultiAZ :: Maybe Bool
      -- ^ Indicates that the Single-AZ DB instance is to change to a
      -- Multi-AZ deployment.
    , _pmvAllocatedStorage :: Maybe Integer
      -- ^ Contains the new AllocatedStorage size for the DB instance that
      -- will be applied or is in progress.
    , _pmvPort :: Maybe Integer
      -- ^ Specifies the pending port for the DB instance.
    } deriving (Generic)

instance FromXML PendingModifiedValues where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PendingModifiedValues"

instance ToQuery PendingModifiedValues where
    toQuery = genericToQuery def

-- | This data type is used as a response element in the
-- DescribeReservedDBInstances and DescribeReservedDBInstancesOfferings
-- actions.
data RecurringCharge = RecurringCharge
    { _rcRecurringChargeFrequency :: Maybe Text
      -- ^ The frequency of the recurring charge.
    , _rcRecurringChargeAmount :: Maybe Double
      -- ^ The amount of the recurring charge.
    } deriving (Generic)

instance FromXML RecurringCharge where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RecurringCharge"

instance ToQuery RecurringCharge where
    toQuery = genericToQuery def

-- | This data type is used as a response element in the
-- DescribeReservedDBInstances and PurchaseReservedDBInstancesOffering
-- actions.
data ReservedDBInstance = ReservedDBInstance
    { _rdbiDBInstanceCount :: Maybe Integer
      -- ^ The number of reserved DB instances.
    , _rdbiState :: Maybe Text
      -- ^ The state of the reserved DB instance.
    , _rdbiCurrencyCode :: Maybe Text
      -- ^ The currency code for the reserved DB instance.
    , _rdbiStartTime :: Maybe ISO8601
      -- ^ The time the reservation started.
    , _rdbiProductDescription :: Maybe Text
      -- ^ The description of the reserved DB instance.
    , _rdbiReservedDBInstanceId :: Maybe Text
      -- ^ The unique identifier for the reservation.
    , _rdbiDBInstanceClass :: Maybe Text
      -- ^ The DB instance class for the reserved DB instance.
    , _rdbiMultiAZ :: Maybe Bool
      -- ^ Indicates if the reservation applies to Multi-AZ deployments.
    , _rdbiReservedDBInstancesOfferingId :: Maybe Text
      -- ^ The offering identifier.
    , _rdbiRecurringCharges :: [RecurringCharge]
      -- ^ The recurring price charged to run this reserved DB instance.
    , _rdbiOfferingType :: Maybe Text
      -- ^ The offering type of this reserved DB instance.
    , _rdbiUsagePrice :: Maybe Double
      -- ^ The hourly price charged for this reserved DB instance.
    , _rdbiFixedPrice :: Maybe Double
      -- ^ The fixed price charged for this reserved DB instance.
    , _rdbiDuration :: Maybe Integer
      -- ^ The duration of the reservation in seconds.
    } deriving (Generic)

instance FromXML ReservedDBInstance where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReservedDBInstance"

-- | This data type is used as a response element in the
-- DescribeReservedDBInstancesOfferings action.
data ReservedDBInstancesOffering = ReservedDBInstancesOffering
    { _rdbioCurrencyCode :: Maybe Text
      -- ^ The currency code for the reserved DB instance offering.
    , _rdbioProductDescription :: Maybe Text
      -- ^ The database engine used by the offering.
    , _rdbioDBInstanceClass :: Maybe Text
      -- ^ The DB instance class for the reserved DB instance.
    , _rdbioMultiAZ :: Maybe Bool
      -- ^ Indicates if the offering applies to Multi-AZ deployments.
    , _rdbioReservedDBInstancesOfferingId :: Maybe Text
      -- ^ The offering identifier.
    , _rdbioRecurringCharges :: [RecurringCharge]
      -- ^ The recurring price charged to run this reserved DB instance.
    , _rdbioOfferingType :: Maybe Text
      -- ^ The offering type.
    , _rdbioUsagePrice :: Maybe Double
      -- ^ The hourly price charged for this offering.
    , _rdbioFixedPrice :: Maybe Double
      -- ^ The fixed price charged for this offering.
    , _rdbioDuration :: Maybe Integer
      -- ^ The duration of the offering in seconds.
    } deriving (Generic)

instance FromXML ReservedDBInstancesOffering where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReservedDBInstancesOffering"

-- | This data type is used as a response element in the DescribeDBSubnetGroups
-- action.
data Subnet = Subnet
    { _stSubnetStatus :: Maybe Text
      -- ^ Specifies the status of the subnet.
    , _stSubnetIdentifier :: Maybe Text
      -- ^ Specifies the identifier of the subnet.
    , _stSubnetAvailabilityZone :: Maybe AvailabilityZone
      -- ^ Contains Availability Zone information. This data type is used as
      -- an element in the following data type: OrderableDBInstanceOption.
    } deriving (Generic)

instance FromXML Subnet where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Subnet"

instance ToQuery Subnet where
    toQuery = genericToQuery def

-- | Metadata assigned to an Amazon RDS resource consisting of a key-value pair.
data Tag = Tag
    { _tgValue :: Maybe Text
      -- ^ A value is the optional value of the tag. The string value can be
      -- from 1 to 256 Unicode characters in length and cannot be prefixed
      -- with "aws:" or "rds:". The string may only contain only the set
      -- of Unicode letters, digits, white-space, '_', '.', '/', '=', '+',
      -- '-' (Java regex: "^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-]*)$").
    , _tgKey :: Maybe Text
      -- ^ A key is the required name of the tag. The string value can be
      -- from 1 to 128 Unicode characters in length and cannot be prefixed
      -- with "aws:" or "rds:". The string may only contain only the set
      -- of Unicode letters, digits, white-space, '_', '.', '/', '=', '+',
      -- '-' (Java regex: "^([\\p{L}\\p{Z}\\p{N}_.:/=+\\-]*)$").
    } deriving (Generic)

instance FromXML Tag where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Tag"

instance ToQuery Tag where
    toQuery = genericToQuery def

-- | This data type is used as a response element for queries on VPC security
-- group membership.
data VpcSecurityGroupMembership = VpcSecurityGroupMembership
    { _vsgmStatus :: Maybe Text
      -- ^ The status of the VPC security group.
    , _vsgmVpcSecurityGroupId :: Maybe Text
      -- ^ The name of the VPC security group.
    } deriving (Generic)

instance FromXML VpcSecurityGroupMembership where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "VpcSecurityGroupMembership"

instance ToQuery VpcSecurityGroupMembership where
    toQuery = genericToQuery def