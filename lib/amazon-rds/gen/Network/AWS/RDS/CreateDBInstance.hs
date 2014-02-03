{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.CreateDBInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new DB instance. https://rds.amazonaws.com/
-- ?Action=CreateDBInstance &DBInstanceIdentifier=SimCoProd01 &Engine=mysql
-- &MasterUserPassword=Password01 &AllocatedStorage=10 &MasterUsername=master
-- &Version=2013-05-15 &DBInstanceClass=db.m1.large
-- &DBSubnetGroupName=dbSubnetgroup01 &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2011-05-23T05%3A54%3A53.578Z
-- &AWSAccessKeyId= &Signature= mysql **** 1 false general-public-license
-- 990524496922 Complete description subnet_grp1 Active subnet-7c5b4115
-- us-east-1c Active subnet-7b5b4112 us-east-1b Active subnet-3ea6bd57
-- us-east-1d creating 5.1.50 simcoprod01 in-sync default.mysql5.1 active
-- default 00:00-00:30 true sat:07:30-sat:08:00 10 db.m1.large master
-- 2e5d4270-8501-11e0-bd9b-a7b1ece36d51.
module Network.AWS.RDS.CreateDBInstance where

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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
createDBInstance :: Int
                 -> Text
                 -> Text
                 -> Text
                 -> Text
                 -> Text
                 -> CreateDBInstance
createDBInstance p1 p2 p3 p4 p5 p6 = CreateDBInstance
    { cdbimAllocatedStorage = p1
    , cdbimDBInstanceClass = p2
    , cdbimDBInstanceIdentifier = p3
    , cdbimEngine = p4
    , cdbimMasterUserPassword = p5
    , cdbimMasterUsername = p6
    , cdbimAutoMinorVersionUpgrade = Nothing
    , cdbimAvailabilityZone = Nothing
    , cdbimBackupRetentionPeriod = Nothing
    , cdbimCharacterSetName = Nothing
    , cdbimDBName = Nothing
    , cdbimDBParameterGroupName = Nothing
    , cdbimDBSecurityGroups = []
    , cdbimDBSubnetGroupName = Nothing
    , cdbimEngineVersion = Nothing
    , cdbimIops = Nothing
    , cdbimLicenseModel = Nothing
    , cdbimMultiAZ = Nothing
    , cdbimOptionGroupName = Nothing
    , cdbimPort = Nothing
    , cdbimPreferredBackupWindow = Nothing
    , cdbimPreferredMaintenanceWindow = Nothing
    , cdbimPubliclyAccessible = Nothing
    , cdbimTags = []
    , cdbimVpcSecurityGroupIds = []
    }

data CreateDBInstance = CreateDBInstance
    { cdbimAllocatedStorage :: !Int
      -- ^ The amount of storage (in gigabytes) to be initially allocated for the
      -- database instance. MySQL Constraints: Must be an integer from 5 to 1024.
      -- Type: Integer Oracle Constraints: Must be an integer from 10 to 1024. SQL
      -- Server Constraints: Must be an integer from 200 to 1024 (Standard Edition
      -- and Enterprise Edition) or from 30 to 1024 (Express Edition and Web
      -- Edition).
    , cdbimAutoMinorVersionUpgrade :: Maybe Bool
      -- ^ Indicates that minor engine upgrades will be applied automatically to the
      -- DB instance during the maintenance window. Default: true.
    , cdbimAvailabilityZone :: Maybe Text
      -- ^ The EC2 Availability Zone that the database instance will be created in.
      -- Default: A random, system-chosen Availability Zone in the endpoint's
      -- region. Example: us-east-1d Constraint: The AvailabilityZone parameter
      -- cannot be specified if the MultiAZ parameter is set to true. The specified
      -- Availability Zone must be in the same region as the current endpoint.
    , cdbimBackupRetentionPeriod :: Maybe Int
      -- ^ The number of days for which automated backups are retained. Setting this
      -- parameter to a positive number enables backups. Setting this parameter to 0
      -- disables automated backups. Default: 1 Constraints: Must be a value from 0
      -- to 35 Cannot be set to 0 if the DB instance is a source to read replicas.
    , cdbimCharacterSetName :: Maybe Text
      -- ^ For supported engines, indicates that the DB instance should be associated
      -- with the specified CharacterSet.
    , cdbimDBInstanceClass :: !Text
      -- ^ The compute and memory capacity of the DB instance. Valid Values:
      -- db.t1.micro | db.m1.small | db.m1.medium | db.m1.large | db.m1.xlarge |
      -- db.m2.xlarge |db.m2.2xlarge | db.m2.4xlarge.
    , cdbimDBInstanceIdentifier :: !Text
      -- ^ The DB instance identifier. This parameter is stored as a lowercase string.
      -- Constraints: Must contain from 1 to 63 alphanumeric characters or hyphens
      -- (1 to 15 for SQL Server). First character must be a letter. Cannot end with
      -- a hyphen or contain two consecutive hyphens. Example: mydbinstance.
    , cdbimDBName :: Maybe Text
      -- ^ The meaning of this parameter differs according to the database engine you
      -- use. MySQL The name of the database to create when the DB instance is
      -- created. If this parameter is not specified, no database is created in the
      -- DB instance. Constraints: Must contain 1 to 64 alphanumeric characters
      -- Cannot be a word reserved by the specified database engine Type: String
      -- Oracle The Oracle System ID (SID) of the created DB instance. Default: ORCL
      -- Constraints: Cannot be longer than 8 characters SQL Server Not applicable.
      -- Must be null.
    , cdbimDBParameterGroupName :: Maybe Text
      -- ^ The name of the DB parameter group to associate with this DB instance. If
      -- this argument is omitted, the default DBParameterGroup for the specified
      -- engine will be used. Constraints: Must be 1 to 255 alphanumeric characters
      -- First character must be a letter Cannot end with a hyphen or contain two
      -- consecutive hyphens.
    , cdbimDBSecurityGroups :: [Text]
      -- ^ A list of DB security groups to associate with this DB instance. Default:
      -- The default DB security group for the database engine.
    , cdbimDBSubnetGroupName :: Maybe Text
      -- ^ A DB subnet group to associate with this DB instance. If there is no DB
      -- subnet group, then it is a non-VPC DB instance.
    , cdbimEngine :: !Text
      -- ^ The name of the database engine to be used for this instance. Valid Values:
      -- MySQL | oracle-se1 | oracle-se | oracle-ee | sqlserver-ee | sqlserver-se |
      -- sqlserver-ex | sqlserver-web.
    , cdbimEngineVersion :: Maybe Text
      -- ^ The version number of the database engine to use. MySQL Example: 5.1.42
      -- Type: String Oracle Example: 11.2.0.2.v2 Type: String SQL Server Example:
      -- 10.50.2789.0.v1.
    , cdbimIops :: Maybe Int
      -- ^ The amount of Provisioned IOPS (input/output operations per second) to be
      -- initially allocated for the DB instance. Constraints: Must be an integer
      -- greater than 1000.
    , cdbimLicenseModel :: Maybe Text
      -- ^ License model information for this DB instance. Valid values:
      -- license-included | bring-your-own-license | general-public-license.
    , cdbimMasterUserPassword :: !Text
      -- ^ The password for the master database user. Can be any printable ASCII
      -- character except "/", """, or "@". Type: String MySQL Constraints: Must
      -- contain from 8 to 41 characters. Oracle Constraints: Must contain from 8 to
      -- 30 characters. SQL Server Constraints: Must contain from 8 to 128
      -- characters.
    , cdbimMasterUsername :: !Text
      -- ^ The name of master user for the client DB instance. MySQL Constraints: Must
      -- be 1 to 16 alphanumeric characters. First character must be a letter.
      -- Cannot be a reserved word for the chosen database engine. Type: String
      -- Oracle Constraints: Must be 1 to 30 alphanumeric characters. First
      -- character must be a letter. Cannot be a reserved word for the chosen
      -- database engine. SQL Server Constraints: Must be 1 to 128 alphanumeric
      -- characters. First character must be a letter. Cannot be a reserved word for
      -- the chosen database engine.
    , cdbimMultiAZ :: Maybe Bool
      -- ^ Specifies if the DB instance is a Multi-AZ deployment. You cannot set the
      -- AvailabilityZone parameter if the MultiAZ parameter is set to true.
    , cdbimOptionGroupName :: Maybe Text
      -- ^ Indicates that the DB instance should be associated with the specified
      -- option group. cannot be removed from an option group while DB instances are
      -- associated with the option group. --> Permanent options, such as the TDE
      -- option for Oracle Advanced Security TDE, cannot be removed from an option
      -- group, and that option group cannot be removed from a DB instance once it
      -- is associated with a DB instance.
    , cdbimPort :: Maybe Int
      -- ^ The port number on which the database accepts connections. MySQL Default:
      -- 3306 Valid Values: 1150-65535 Type: Integer Oracle Default: 1521 Valid
      -- Values: 1150-65535 SQL Server Default: 1433 Valid Values: 1150-65535 except
      -- for 1434 and 3389.
    , cdbimPreferredBackupWindow :: Maybe Text
      -- ^ The daily time range during which automated backups are created if
      -- automated backups are enabled, using the BackupRetentionPeriod parameter.
      -- Default: A 30-minute window selected at random from an 8-hour block of time
      -- per region. See the Amazon RDS User Guide for the time blocks for each
      -- region from which the default backup windows are assigned. Constraints:
      -- Must be in the format hh24:mi-hh24:mi. Times should be Universal Time
      -- Coordinated (UTC). Must not conflict with the preferred maintenance window.
      -- Must be at least 30 minutes.
    , cdbimPreferredMaintenanceWindow :: Maybe Text
      -- ^ The weekly time range (in UTC) during which system maintenance can occur.
      -- Format: ddd:hh24:mi-ddd:hh24:mi Default: A 30-minute window selected at
      -- random from an 8-hour block of time per region, occurring on a random day
      -- of the week. To see the time blocks available, see Adjusting the Preferred
      -- Maintenance Window in the Amazon RDS User Guide. Valid Days: Mon, Tue, Wed,
      -- Thu, Fri, Sat, Sun Constraints: Minimum 30-minute window.
    , cdbimPubliclyAccessible :: Maybe Bool
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
    , cdbimTags :: [Tag]
      -- ^ A list of tags.
    , cdbimVpcSecurityGroupIds :: [Text]
      -- ^ A list of EC2 VPC security groups to associate with this DB instance.
      -- Default: The default EC2 VPC security group for the DB subnet group's VPC.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateDBInstance

instance AWSRequest CreateDBInstance where
    type Er CreateDBInstance = RDSError
    type Rs CreateDBInstance = CreateDBInstanceResponse
    request = getQuery service "CreateDBInstance"

data CreateDBInstanceResponse = CreateDBInstanceResponse
    { cdbimrsDBInstance :: Maybe DBInstance
      -- ^ Contains the result of a successful invocation of the following actions:
      -- CreateDBInstance DeleteDBInstance ModifyDBInstance This data type is used
      -- as a response element in the DescribeDBInstances action.
    } deriving (Eq, Show, Generic)

instance FromXML CreateDBInstanceResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "CreateDBInstanceResponse"
        :| ["CreateDBInstanceResult"]
