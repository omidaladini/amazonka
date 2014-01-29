{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.RestoreDBInstanceToPointInTime
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Restores a DB instance to an arbitrary point-in-time. Users can restore to
-- any point in time before the latestRestorableTime for up to
-- backupRetentionPeriod days. The target database is created from the source
-- database with the same configuration as the original database except that
-- the DB instance is created with the default DB security group.
-- https://rds.amazon.com/ ?Action=RestoreDBInstanceToPointInTime
-- &TargetDBInstanceIdentifier=restored-db
-- &SourceDBInstanceIdentifier=simcoprod01 &UseLatestRestorableTime=true
-- &Version=2013-05-15 &Timestamp=2011-05-23T07%3A06%3A02.313Z
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256 &AWSAccessKeyId=
-- &Signature= mysql 1 false general-public-license creating 5.1.50
-- restored-db in-sync default.mysql5.1 active default 00:00-00:30 true
-- sat:07:30-sat:08:00 10 db.m1.large master
-- 1ef546bc-850b-11e0-90aa-eb648410240d.
module Network.AWS.RDS.RestoreDBInstanceToPointInTime where

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

import Network.AWS.RDS.Service
import Network.AWS.RDS.Types

-- | Convenience method utilising default fields where applicable.
restoreDBInstanceToPointInTime :: Text
                               -> Text
                               -> AWS (Either RDSError RestoreDBInstanceToPointInTimeResponse)
restoreDBInstanceToPointInTime p1 p2 = undefined $ RestoreDBInstanceToPointInTime
    { rdbitpitmSourceDBInstanceIdentifier = p1
    , rdbitpitmTargetDBInstanceIdentifier = p2
    , rdbitpitmAutoMinorVersionUpgrade = Nothing
    , rdbitpitmAvailabilityZone = Nothing
    , rdbitpitmDBInstanceClass = Nothing
    , rdbitpitmDBName = Nothing
    , rdbitpitmDBSubnetGroupName = Nothing
    , rdbitpitmEngine = Nothing
    , rdbitpitmIops = Nothing
    , rdbitpitmLicenseModel = Nothing
    , rdbitpitmMultiAZ = Nothing
    , rdbitpitmOptionGroupName = Nothing
    , rdbitpitmPort = Nothing
    , rdbitpitmPubliclyAccessible = Nothing
    , rdbitpitmRestoreTime = Nothing
    , rdbitpitmTags = []
    , rdbitpitmUseLatestRestorableTime = Nothing
    }

data RestoreDBInstanceToPointInTime = RestoreDBInstanceToPointInTime
    { rdbitpitmAutoMinorVersionUpgrade :: Maybe Bool
      -- ^ Indicates that minor version upgrades will be applied automatically to the
      -- DB instance during the maintenance window.
    , rdbitpitmAvailabilityZone :: Maybe Text
      -- ^ The EC2 Availability Zone that the database instance will be created in.
      -- Default: A random, system-chosen Availability Zone. Constraint: You cannot
      -- specify the AvailabilityZone parameter if the MultiAZ parameter is set to
      -- true. Example: us-east-1a.
    , rdbitpitmDBInstanceClass :: Maybe Text
      -- ^ The compute and memory capacity of the Amazon RDS DB instance. Valid
      -- Values: db.t1.micro | db.m1.small | db.m1.medium | db.m1.large |
      -- db.m1.xlarge | db.m2.2xlarge | db.m2.4xlarge Default: The same
      -- DBInstanceClass as the original DB instance.
    , rdbitpitmDBName :: Maybe Text
      -- ^ The database name for the restored DB instance. This parameter is not used
      -- for the MySQL engine.
    , rdbitpitmDBSubnetGroupName :: Maybe Text
      -- ^ The DB subnet group name to use for the new instance.
    , rdbitpitmEngine :: Maybe Text
      -- ^ The database engine to use for the new instance. Default: The same as
      -- source Constraint: Must be compatible with the engine of the source
      -- Example: oracle-ee.
    , rdbitpitmIops :: Maybe Int
      -- ^ The amount of Provisioned IOPS (input/output operations per second) to be
      -- initially allocated for the DB instance. Constraints: Must be an integer
      -- greater than 1000.
    , rdbitpitmLicenseModel :: Maybe Text
      -- ^ License model information for the restored DB instance. Default: Same as
      -- source. Valid values: license-included | bring-your-own-license |
      -- general-public-license.
    , rdbitpitmMultiAZ :: Maybe Bool
      -- ^ Specifies if the DB instance is a Multi-AZ deployment. Constraint: You
      -- cannot specify the AvailabilityZone parameter if the MultiAZ parameter is
      -- set to true.
    , rdbitpitmOptionGroupName :: Maybe Text
      -- ^ The name of the option group to be used for the restored DB instance.
      -- cannot be removed from an option group while DB instances are associated
      -- with the option group. --> Permanent options, such as the TDE option for
      -- Oracle Advanced Security TDE, cannot be removed from an option group, and
      -- that option group cannot be removed from a DB instance once it is
      -- associated with a DB instance.
    , rdbitpitmPort :: Maybe Int
      -- ^ The port number on which the database accepts connections. Constraints:
      -- Value must be 1150-65535 Default: The same port as the original DB
      -- instance.
    , rdbitpitmPubliclyAccessible :: Maybe Bool
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
    , rdbitpitmRestoreTime :: Maybe UTCTime
      -- ^ The date and time to restore from. Valid Values: Value must be a UTC time
      -- Constraints: Must be before the latest restorable time for the DB instance
      -- Cannot be specified if UseLatestRestorableTime parameter is true Example:
      -- 2009-09-07T23:45:00Z.
    , rdbitpitmSourceDBInstanceIdentifier :: !Text
      -- ^ The identifier of the source DB instance from which to restore.
      -- Constraints: Must be the identifier of an existing database instance Must
      -- contain from 1 to 63 alphanumeric characters or hyphens First character
      -- must be a letter Cannot end with a hyphen or contain two consecutive
      -- hyphens.
    , rdbitpitmTags :: [Tag]
      -- ^ A list of tags.
    , rdbitpitmTargetDBInstanceIdentifier :: !Text
      -- ^ The name of the new database instance to be created. Constraints: Must
      -- contain from 1 to 63 alphanumeric characters or hyphens First character
      -- must be a letter Cannot end with a hyphen or contain two consecutive
      -- hyphens.
    , rdbitpitmUseLatestRestorableTime :: Maybe Bool
      -- ^ Specifies whether (true) or not (false) the DB instance is restored from
      -- the latest backup time. Default: false Constraints: Cannot be specified if
      -- RestoreTime parameter is provided.
    } deriving (Eq, Show, Generic)

instance ToQuery RestoreDBInstanceToPointInTime

instance AWSRequest RestoreDBInstanceToPointInTime where
    type Er RestoreDBInstanceToPointInTime = RDSError
    type Rs RestoreDBInstanceToPointInTime = RestoreDBInstanceToPointInTimeResponse
    request = getQuery service "RestoreDBInstanceToPointInTime"

data RestoreDBInstanceToPointInTimeResponse = RestoreDBInstanceToPointInTimeResponse
    { rdbitpitmrsDBInstance :: Maybe DBInstance
      -- ^ Contains the result of a successful invocation of the following actions:
      -- CreateDBInstance DeleteDBInstance ModifyDBInstance This data type is used
      -- as a response element in the DescribeDBInstances action.
    } deriving (Eq, Show, Generic)

instance FromXML RestoreDBInstanceToPointInTimeResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "RestoreDBInstanceToPointInTimeResponse"
        :| ["RestoreDBInstanceToPointInTimeResult"]
