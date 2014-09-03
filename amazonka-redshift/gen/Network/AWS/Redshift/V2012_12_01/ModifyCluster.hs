{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.ModifyCluster
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies the settings for a cluster. For example, you can add another
-- security or parameter group, update the preferred maintenance window, or
-- change the master user password. Resetting a cluster password or modifying
-- the security groups associated with a cluster do not need a reboot.
-- However, modifying a parameter group requires a reboot for parameters to
-- take effect. For more information about managing clusters, go to Amazon
-- Redshift Clusters in the Amazon Redshift Management Guide You can also
-- change node type and the number of nodes to scale up or down the cluster.
-- When resizing a cluster, you must specify both the number of nodes and the
-- node type even if one of the parameters does not change. If you specify the
-- same number of nodes and node type that are already configured for the
-- cluster, an error is returned. https://redshift.us-east-1.amazonaws.com/
-- ?Action=ModifyCluster &AllowVersionUpgrade=true
-- &AutomatedSnapshotRetentionPeriod=2 &ClusterIdentifier=examplecluster
-- &ClusterParameterGroupName=parametergroup1
-- &PreferredMaintenanceWindow=wed:07:30-wed:08:00 &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T022911Z
-- &x-amz-signedheaders=content-type;host;x-amz-date 1.0 5439
-- examplecluster.coqoarplqhsn.us-east-1.redshift.amazonaws.com available 2 2
-- true false dev wed:07:30-wed:08:00 applying parametergroup1
-- 2013-01-22T19:23:59.368Z active default us-east-1c dw1.xlarge
-- examplecluster true adminuser acbc43d5-6504-11e2-bea9-49e0ce183f07.
module Network.AWS.Redshift.V2012_12_01.ModifyCluster
    (
    -- * Request
      ModifyCluster
    -- ** Request constructor
    , modifyCluster
    -- ** Request lenses
    , mcmClusterIdentifier
    , mcmAllowVersionUpgrade
    , mcmClusterSecurityGroups
    , mcmNumberOfNodes
    , mcmAutomatedSnapshotRetentionPeriod
    , mcmClusterType
    , mcmNodeType
    , mcmMasterUserPassword
    , mcmClusterParameterGroupName
    , mcmPreferredMaintenanceWindow
    , mcmClusterVersion
    , mcmHsmClientCertificateIdentifier
    , mcmHsmConfigurationIdentifier
    , mcmNewClusterIdentifier
    , mcmVpcSecurityGroupIds

    -- * Response
    , ModifyClusterResponse
    -- ** Response lenses
    , ccsCluster
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ModifyCluster' request.
modifyCluster :: Text -- ^ 'mcmClusterIdentifier'
              -> ModifyCluster
modifyCluster p1 = ModifyCluster
    { _mcmClusterIdentifier = p1
    , _mcmAllowVersionUpgrade = Nothing
    , _mcmClusterSecurityGroups = mempty
    , _mcmNumberOfNodes = Nothing
    , _mcmAutomatedSnapshotRetentionPeriod = Nothing
    , _mcmClusterType = Nothing
    , _mcmNodeType = Nothing
    , _mcmMasterUserPassword = Nothing
    , _mcmClusterParameterGroupName = Nothing
    , _mcmPreferredMaintenanceWindow = Nothing
    , _mcmClusterVersion = Nothing
    , _mcmHsmClientCertificateIdentifier = Nothing
    , _mcmHsmConfigurationIdentifier = Nothing
    , _mcmNewClusterIdentifier = Nothing
    , _mcmVpcSecurityGroupIds = mempty
    }

data ModifyCluster = ModifyCluster
    { _mcmClusterIdentifier :: Text
      -- ^ The unique identifier of the cluster to be modified. Example:
      -- examplecluster.
    , _mcmAllowVersionUpgrade :: Maybe Bool
      -- ^ If true, upgrades will be applied automatically to the cluster
      -- during the maintenance window. Default: false.
    , _mcmClusterSecurityGroups :: [Text]
      -- ^ A list of cluster security groups to be authorized on this
      -- cluster. This change is asynchronously applied as soon as
      -- possible. Security groups currently associated with the cluster,
      -- and not in the list of groups to apply, will be revoked from the
      -- cluster. Constraints: Must be 1 to 255 alphanumeric characters or
      -- hyphens First character must be a letter Cannot end with a hyphen
      -- or contain two consecutive hyphens.
    , _mcmNumberOfNodes :: Maybe Integer
      -- ^ The new number of nodes of the cluster. If you specify a new
      -- number of nodes, you must also specify the node type parameter
      -- also. When you submit your request to resize a cluster, Amazon
      -- Redshift sets access permissions for the cluster to read-only.
      -- After Amazon Redshift provisions a new cluster according to your
      -- resize requirements, there will be a temporary outage while the
      -- old cluster is deleted and your connection is switched to the new
      -- cluster. When the new connection is complete, the original access
      -- permissions for the cluster are restored. You can use
      -- DescribeResize to track the progress of the resize request. Valid
      -- Values: Integer greater than 0.
    , _mcmAutomatedSnapshotRetentionPeriod :: Maybe Integer
      -- ^ The number of days that automated snapshots are retained. If the
      -- value is 0, automated snapshots are disabled. Even if automated
      -- snapshots are disabled, you can still create manual snapshots
      -- when you want with CreateClusterSnapshot. If you decrease the
      -- automated snapshot retention period from its current value,
      -- existing automated snapshots that fall outside of the new
      -- retention period will be immediately deleted. Default: Uses
      -- existing setting. Constraints: Must be a value from 0 to 35.
    , _mcmClusterType :: Maybe Text
      -- ^ The new cluster type. When you submit your cluster resize
      -- request, your existing cluster goes into a read-only mode. After
      -- Amazon Redshift provisions a new cluster based on your resize
      -- requirements, there will be outage for a period while the old
      -- cluster is deleted and your connection is switched to the new
      -- cluster. You can use DescribeResize to track the progress of the
      -- resize request. Valid Values: multi-node | single-node.
    , _mcmNodeType :: Maybe Text
      -- ^ The new node type of the cluster. If you specify a new node type,
      -- you must also specify the number of nodes parameter also. When
      -- you submit your request to resize a cluster, Amazon Redshift sets
      -- access permissions for the cluster to read-only. After Amazon
      -- Redshift provisions a new cluster according to your resize
      -- requirements, there will be a temporary outage while the old
      -- cluster is deleted and your connection is switched to the new
      -- cluster. When the new connection is complete, the original access
      -- permissions for the cluster are restored. You can use the
      -- DescribeResize to track the progress of the resize request. Valid
      -- Values: dw1.xlarge | dw1.8xlarge | dw2.large | dw2.8xlarge.
    , _mcmMasterUserPassword :: Maybe Text
      -- ^ The new password for the cluster master user. This change is
      -- asynchronously applied as soon as possible. Between the time of
      -- the request and the completion of the request, the
      -- MasterUserPassword element exists in the PendingModifiedValues
      -- element of the operation response. Operations never return the
      -- password, so this operation provides a way to regain access to
      -- the master user account for a cluster if the password is lost.
      -- Default: Uses existing setting. Constraints: Must be between 8
      -- and 64 characters in length. Must contain at least one uppercase
      -- letter. Must contain at least one lowercase letter. Must contain
      -- one number. Can be any printable ASCII character (ASCII code 33
      -- to 126) except ' (single quote), " (double quote), \, /, @, or
      -- space.
    , _mcmClusterParameterGroupName :: Maybe Text
      -- ^ The name of the cluster parameter group to apply to this cluster.
      -- This change is applied only after the cluster is rebooted. To
      -- reboot a cluster use RebootCluster. Default: Uses existing
      -- setting. Constraints: The cluster parameter group must be in the
      -- same parameter group family that matches the cluster version.
    , _mcmPreferredMaintenanceWindow :: Maybe Text
      -- ^ The weekly time range (in UTC) during which system maintenance
      -- can occur, if necessary. If system maintenance is necessary
      -- during the window, it may result in an outage. This maintenance
      -- window change is made immediately. If the new maintenance window
      -- indicates the current time, there must be at least 120 minutes
      -- between the current time and end of the window in order to ensure
      -- that pending changes are applied. Default: Uses existing setting.
      -- Format: ddd:hh24:mi-ddd:hh24:mi, for example wed:07:30-wed:08:00.
      -- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun Constraints:
      -- Must be at least 30 minutes.
    , _mcmClusterVersion :: Maybe Text
      -- ^ The new version number of the Amazon Redshift engine to upgrade
      -- to. For major version upgrades, if a non-default cluster
      -- parameter group is currently in use, a new cluster parameter
      -- group in the cluster parameter group family for the new version
      -- must be specified. The new cluster parameter group can be the
      -- default for that cluster parameter group family. For more
      -- information about managing parameter groups, go to Amazon
      -- Redshift Parameter Groups in the Amazon Redshift Management
      -- Guide. Example: 1.0.
    , _mcmHsmClientCertificateIdentifier :: Maybe Text
      -- ^ Specifies the name of the HSM client certificate the Amazon
      -- Redshift cluster uses to retrieve the data encryption keys stored
      -- in an HSM.
    , _mcmHsmConfigurationIdentifier :: Maybe Text
      -- ^ Specifies the name of the HSM configuration that contains the
      -- information the Amazon Redshift cluster can use to retrieve and
      -- store keys in an HSM.
    , _mcmNewClusterIdentifier :: Maybe Text
      -- ^ The new identifier for the cluster. Constraints: Must contain
      -- from 1 to 63 alphanumeric characters or hyphens. Alphabetic
      -- characters must be lowercase. First character must be a letter.
      -- Cannot end with a hyphen or contain two consecutive hyphens. Must
      -- be unique for all clusters within an AWS account. Example:
      -- examplecluster.
    , _mcmVpcSecurityGroupIds :: [Text]
      -- ^ A list of virtual private cloud (VPC) security groups to be
      -- associated with the cluster.
    } deriving (Show, Generic)

-- | The unique identifier of the cluster to be modified. Example:
-- examplecluster.
mcmClusterIdentifier
    :: Functor f
    => (Text
    -> f (Text))
    -> ModifyCluster
    -> f ModifyCluster
mcmClusterIdentifier f x =
    (\y -> x { _mcmClusterIdentifier = y })
       <$> f (_mcmClusterIdentifier x)
{-# INLINE mcmClusterIdentifier #-}

-- | If true, upgrades will be applied automatically to the cluster during the
-- maintenance window. Default: false.
mcmAllowVersionUpgrade
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> ModifyCluster
    -> f ModifyCluster
mcmAllowVersionUpgrade f x =
    (\y -> x { _mcmAllowVersionUpgrade = y })
       <$> f (_mcmAllowVersionUpgrade x)
{-# INLINE mcmAllowVersionUpgrade #-}

-- | A list of cluster security groups to be authorized on this cluster. This
-- change is asynchronously applied as soon as possible. Security groups
-- currently associated with the cluster, and not in the list of groups to
-- apply, will be revoked from the cluster. Constraints: Must be 1 to 255
-- alphanumeric characters or hyphens First character must be a letter Cannot
-- end with a hyphen or contain two consecutive hyphens.
mcmClusterSecurityGroups
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> ModifyCluster
    -> f ModifyCluster
mcmClusterSecurityGroups f x =
    (\y -> x { _mcmClusterSecurityGroups = y })
       <$> f (_mcmClusterSecurityGroups x)
{-# INLINE mcmClusterSecurityGroups #-}

-- | The new number of nodes of the cluster. If you specify a new number of
-- nodes, you must also specify the node type parameter also. When you submit
-- your request to resize a cluster, Amazon Redshift sets access permissions
-- for the cluster to read-only. After Amazon Redshift provisions a new
-- cluster according to your resize requirements, there will be a temporary
-- outage while the old cluster is deleted and your connection is switched to
-- the new cluster. When the new connection is complete, the original access
-- permissions for the cluster are restored. You can use DescribeResize to
-- track the progress of the resize request. Valid Values: Integer greater
-- than 0.
mcmNumberOfNodes
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ModifyCluster
    -> f ModifyCluster
mcmNumberOfNodes f x =
    (\y -> x { _mcmNumberOfNodes = y })
       <$> f (_mcmNumberOfNodes x)
{-# INLINE mcmNumberOfNodes #-}

-- | The number of days that automated snapshots are retained. If the value is
-- 0, automated snapshots are disabled. Even if automated snapshots are
-- disabled, you can still create manual snapshots when you want with
-- CreateClusterSnapshot. If you decrease the automated snapshot retention
-- period from its current value, existing automated snapshots that fall
-- outside of the new retention period will be immediately deleted. Default:
-- Uses existing setting. Constraints: Must be a value from 0 to 35.
mcmAutomatedSnapshotRetentionPeriod
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ModifyCluster
    -> f ModifyCluster
mcmAutomatedSnapshotRetentionPeriod f x =
    (\y -> x { _mcmAutomatedSnapshotRetentionPeriod = y })
       <$> f (_mcmAutomatedSnapshotRetentionPeriod x)
{-# INLINE mcmAutomatedSnapshotRetentionPeriod #-}

-- | The new cluster type. When you submit your cluster resize request, your
-- existing cluster goes into a read-only mode. After Amazon Redshift
-- provisions a new cluster based on your resize requirements, there will be
-- outage for a period while the old cluster is deleted and your connection is
-- switched to the new cluster. You can use DescribeResize to track the
-- progress of the resize request. Valid Values: multi-node | single-node.
mcmClusterType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ModifyCluster
    -> f ModifyCluster
mcmClusterType f x =
    (\y -> x { _mcmClusterType = y })
       <$> f (_mcmClusterType x)
{-# INLINE mcmClusterType #-}

-- | The new node type of the cluster. If you specify a new node type, you must
-- also specify the number of nodes parameter also. When you submit your
-- request to resize a cluster, Amazon Redshift sets access permissions for
-- the cluster to read-only. After Amazon Redshift provisions a new cluster
-- according to your resize requirements, there will be a temporary outage
-- while the old cluster is deleted and your connection is switched to the new
-- cluster. When the new connection is complete, the original access
-- permissions for the cluster are restored. You can use the DescribeResize to
-- track the progress of the resize request. Valid Values: dw1.xlarge |
-- dw1.8xlarge | dw2.large | dw2.8xlarge.
mcmNodeType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ModifyCluster
    -> f ModifyCluster
mcmNodeType f x =
    (\y -> x { _mcmNodeType = y })
       <$> f (_mcmNodeType x)
{-# INLINE mcmNodeType #-}

-- | The new password for the cluster master user. This change is asynchronously
-- applied as soon as possible. Between the time of the request and the
-- completion of the request, the MasterUserPassword element exists in the
-- PendingModifiedValues element of the operation response. Operations never
-- return the password, so this operation provides a way to regain access to
-- the master user account for a cluster if the password is lost. Default:
-- Uses existing setting. Constraints: Must be between 8 and 64 characters in
-- length. Must contain at least one uppercase letter. Must contain at least
-- one lowercase letter. Must contain one number. Can be any printable ASCII
-- character (ASCII code 33 to 126) except ' (single quote), " (double quote),
-- \, /, @, or space.
mcmMasterUserPassword
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ModifyCluster
    -> f ModifyCluster
mcmMasterUserPassword f x =
    (\y -> x { _mcmMasterUserPassword = y })
       <$> f (_mcmMasterUserPassword x)
{-# INLINE mcmMasterUserPassword #-}

-- | The name of the cluster parameter group to apply to this cluster. This
-- change is applied only after the cluster is rebooted. To reboot a cluster
-- use RebootCluster. Default: Uses existing setting. Constraints: The cluster
-- parameter group must be in the same parameter group family that matches the
-- cluster version.
mcmClusterParameterGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ModifyCluster
    -> f ModifyCluster
mcmClusterParameterGroupName f x =
    (\y -> x { _mcmClusterParameterGroupName = y })
       <$> f (_mcmClusterParameterGroupName x)
{-# INLINE mcmClusterParameterGroupName #-}

-- | The weekly time range (in UTC) during which system maintenance can occur,
-- if necessary. If system maintenance is necessary during the window, it may
-- result in an outage. This maintenance window change is made immediately. If
-- the new maintenance window indicates the current time, there must be at
-- least 120 minutes between the current time and end of the window in order
-- to ensure that pending changes are applied. Default: Uses existing setting.
-- Format: ddd:hh24:mi-ddd:hh24:mi, for example wed:07:30-wed:08:00. Valid
-- Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun Constraints: Must be at least
-- 30 minutes.
mcmPreferredMaintenanceWindow
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ModifyCluster
    -> f ModifyCluster
mcmPreferredMaintenanceWindow f x =
    (\y -> x { _mcmPreferredMaintenanceWindow = y })
       <$> f (_mcmPreferredMaintenanceWindow x)
{-# INLINE mcmPreferredMaintenanceWindow #-}

-- | The new version number of the Amazon Redshift engine to upgrade to. For
-- major version upgrades, if a non-default cluster parameter group is
-- currently in use, a new cluster parameter group in the cluster parameter
-- group family for the new version must be specified. The new cluster
-- parameter group can be the default for that cluster parameter group family.
-- For more information about managing parameter groups, go to Amazon Redshift
-- Parameter Groups in the Amazon Redshift Management Guide. Example: 1.0.
mcmClusterVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ModifyCluster
    -> f ModifyCluster
mcmClusterVersion f x =
    (\y -> x { _mcmClusterVersion = y })
       <$> f (_mcmClusterVersion x)
{-# INLINE mcmClusterVersion #-}

-- | Specifies the name of the HSM client certificate the Amazon Redshift
-- cluster uses to retrieve the data encryption keys stored in an HSM.
mcmHsmClientCertificateIdentifier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ModifyCluster
    -> f ModifyCluster
mcmHsmClientCertificateIdentifier f x =
    (\y -> x { _mcmHsmClientCertificateIdentifier = y })
       <$> f (_mcmHsmClientCertificateIdentifier x)
{-# INLINE mcmHsmClientCertificateIdentifier #-}

-- | Specifies the name of the HSM configuration that contains the information
-- the Amazon Redshift cluster can use to retrieve and store keys in an HSM.
mcmHsmConfigurationIdentifier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ModifyCluster
    -> f ModifyCluster
mcmHsmConfigurationIdentifier f x =
    (\y -> x { _mcmHsmConfigurationIdentifier = y })
       <$> f (_mcmHsmConfigurationIdentifier x)
{-# INLINE mcmHsmConfigurationIdentifier #-}

-- | The new identifier for the cluster. Constraints: Must contain from 1 to 63
-- alphanumeric characters or hyphens. Alphabetic characters must be
-- lowercase. First character must be a letter. Cannot end with a hyphen or
-- contain two consecutive hyphens. Must be unique for all clusters within an
-- AWS account. Example: examplecluster.
mcmNewClusterIdentifier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ModifyCluster
    -> f ModifyCluster
mcmNewClusterIdentifier f x =
    (\y -> x { _mcmNewClusterIdentifier = y })
       <$> f (_mcmNewClusterIdentifier x)
{-# INLINE mcmNewClusterIdentifier #-}

-- | A list of virtual private cloud (VPC) security groups to be associated with
-- the cluster.
mcmVpcSecurityGroupIds
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> ModifyCluster
    -> f ModifyCluster
mcmVpcSecurityGroupIds f x =
    (\y -> x { _mcmVpcSecurityGroupIds = y })
       <$> f (_mcmVpcSecurityGroupIds x)
{-# INLINE mcmVpcSecurityGroupIds #-}

instance ToQuery ModifyCluster where
    toQuery = genericQuery def

data ModifyClusterResponse = ModifyClusterResponse
    { _ccsCluster :: Maybe Cluster
      -- ^ Describes a cluster.
    } deriving (Show, Generic)

-- | Describes a cluster.
ccsCluster
    :: Functor f
    => (Maybe Cluster
    -> f (Maybe Cluster))
    -> ModifyClusterResponse
    -> f ModifyClusterResponse
ccsCluster f x =
    (\y -> x { _ccsCluster = y })
       <$> f (_ccsCluster x)
{-# INLINE ccsCluster #-}

instance FromXML ModifyClusterResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ModifyCluster where
    type Sv ModifyCluster = Redshift
    type Rs ModifyCluster = ModifyClusterResponse

    request = post "ModifyCluster"
    response _ = xmlResponse
