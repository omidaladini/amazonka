{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.RestoreFromClusterSnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new cluster from a snapshot. Amazon Redshift creates the
-- resulting cluster with the same configuration as the original cluster from
-- which the snapshot was created, except that the new cluster is created with
-- the default cluster security and parameter group. After Amazon Redshift
-- creates the cluster you can use the ModifyCluster API to associate a
-- different security group and different parameter group with the restored
-- cluster. If a snapshot is taken of a cluster in VPC, you can restore it
-- only in VPC. In this case, you must provide a cluster subnet group where
-- you want the cluster restored. If snapshot is taken of a cluster outside
-- VPC, then you can restore it only outside VPC. For more information about
-- working with snapshots, go to Amazon Redshift Snapshots in the Amazon
-- Redshift Management Guide. https://redshift.us-east-1.amazonaws.com/
-- ?Action=RestoreFromClusterSnapshot
-- &ClusterIdentifier=examplecluster-restored
-- &SnapshotIdentifier=cm:examplecluster-2013-01-22-19-27-58
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T023350Z
-- &x-amz-signedheaders=content-type;host;x-amz-date 1.0 creating 2 1 true
-- false dev sun:06:30-sun:07:00 in-sync default.redshift-1.0 active default
-- dw.hs1.xlarge examplecluster-restored true adminuser
-- 52a9aee8-6505-11e2-bec0-17624ad140dd.
module Network.AWS.Redshift.RestoreFromClusterSnapshot where

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

import Network.AWS.Redshift.Service
import Network.AWS.Redshift.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
restoreFromClusterSnapshot :: Text
                           -> Text
                           -> RestoreFromClusterSnapshot
restoreFromClusterSnapshot p1 p2 = undefined $ RestoreFromClusterSnapshot
    { rfcsmClusterIdentifier = p1
    , rfcsmSnapshotIdentifier = p2
    , rfcsmAllowVersionUpgrade = Nothing
    , rfcsmAvailabilityZone = Nothing
    , rfcsmClusterSubnetGroupName = Nothing
    , rfcsmElasticIp = Nothing
    , rfcsmHsmClientCertificateIdentifier = Nothing
    , rfcsmHsmConfigurationIdentifier = Nothing
    , rfcsmOwnerAccount = Nothing
    , rfcsmPort = Nothing
    , rfcsmPubliclyAccessible = Nothing
    , rfcsmSnapshotClusterIdentifier = Nothing
    }

data RestoreFromClusterSnapshot = RestoreFromClusterSnapshot
    { rfcsmAllowVersionUpgrade :: Maybe Bool
      -- ^ If true, upgrades can be applied during the maintenance window to the
      -- Amazon Redshift engine that is running on the cluster. Default: true.
    , rfcsmAvailabilityZone :: Maybe Text
      -- ^ The Amazon EC2 Availability Zone in which to restore the cluster. Default:
      -- A random, system-chosen Availability Zone. Example: us-east-1a.
    , rfcsmClusterIdentifier :: !Text
      -- ^ The identifier of the cluster that will be created from restoring the
      -- snapshot. Constraints: Must contain from 1 to 63 alphanumeric characters or
      -- hyphens. Alphabetic characters must be lowercase. First character must be a
      -- letter. Cannot end with a hyphen or contain two consecutive hyphens. Must
      -- be unique for all clusters within an AWS account.
    , rfcsmClusterSubnetGroupName :: Maybe Text
      -- ^ The name of the subnet group where you want to cluster restored. A snapshot
      -- of cluster in VPC can be restored only in VPC. Therefore, you must provide
      -- subnet group name where you want the cluster restored.
    , rfcsmElasticIp :: Maybe Text
      -- ^ The elastic IP (EIP) address for the cluster.
    , rfcsmHsmClientCertificateIdentifier :: Maybe Text
      -- ^ Specifies the name of the HSM client certificate the Amazon Redshift
      -- cluster uses to retrieve the data encryption keys stored in an HSM.
    , rfcsmHsmConfigurationIdentifier :: Maybe Text
      -- ^ Specifies the name of the HSM configuration that contains the information
      -- the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
    , rfcsmOwnerAccount :: Maybe Text
      -- ^ The AWS customer account used to create or copy the snapshot. Required if
      -- you are restoring a snapshot you do not own, optional if you own the
      -- snapshot.
    , rfcsmPort :: Maybe Int
      -- ^ The port number on which the cluster accepts connections. Default: The same
      -- port as the original cluster. Constraints: Must be between 1115 and 65535.
    , rfcsmPubliclyAccessible :: Maybe Bool
      -- ^ If true, the cluster can be accessed from a public network.
    , rfcsmSnapshotClusterIdentifier :: Maybe Text
      -- ^ The name of the cluster the source snapshot was created from. This
      -- parameter is required if your IAM user has a policy containing a snapshot
      -- resource element that specifies anything other than * for the cluster name.
    , rfcsmSnapshotIdentifier :: !Text
      -- ^ The name of the snapshot from which to create the new cluster. This
      -- parameter isn't case sensitive. Example: my-snapshot-id.
    } deriving (Eq, Show, Generic)

instance ToQuery RestoreFromClusterSnapshot

instance AWSRequest RestoreFromClusterSnapshot where
    type Er RestoreFromClusterSnapshot = RedshiftError
    type Rs RestoreFromClusterSnapshot = RestoreFromClusterSnapshotResponse
    request = getQuery service "RestoreFromClusterSnapshot"

data RestoreFromClusterSnapshotResponse = RestoreFromClusterSnapshotResponse
    { rfcsmrsCluster :: Maybe Cluster
      -- ^ Describes a cluster.
    } deriving (Eq, Show, Generic)

instance FromXML RestoreFromClusterSnapshotResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "RestoreFromClusterSnapshotResponse"
        :| ["RestoreFromClusterSnapshotResult"]
