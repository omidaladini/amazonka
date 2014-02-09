{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.RunInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The RunInstances operation launches a specified number of instances. If
-- Amazon EC2 cannot launch the minimum number AMIs you request, no instances
-- launch. If there is insufficient capacity to launch the maximum number of
-- AMIs you request, Amazon EC2 launches as many as possible to satisfy the
-- requested maximum values. Every instance is launched in a security group.
-- If you do not specify a security group at launch, the instances start in
-- your default security group. For more information on creating security
-- groups, see CreateSecurityGroup. An optional instance type can be
-- specified. For information about instance types, see Instance Types. You
-- can provide an optional key pair ID for each image in the launch request
-- (for more information, see CreateKeyPair). All instances that are created
-- from images that use this key pair will have access to the associated
-- public key at boot. You can use this key to provide secure access to an
-- instance of an image on a per-instance basis. Amazon EC2 public images use
-- this feature to provide secure access without passwords. Launching public
-- images without a key pair ID will leave them inaccessible. The public key
-- material is made available to the instance at boot time by placing it in
-- the openssh_id.pub file on a logical device that is exposed to the instance
-- as /dev/sda2 (the ephemeral store). The format of this file is suitable for
-- use as an entry within ~/.ssh/authorized_keys (the OpenSSH format). This
-- can be done at boot (e.g., as part of rc.local) allowing for secure access
-- without passwords. Optional user data can be provided in the launch
-- request. All instances that collectively comprise the launch request have
-- access to this data For more information, see Instance Metadata. If any of
-- the AMIs have a product code attached for which the user has not
-- subscribed, the RunInstances call will fail. We strongly recommend using
-- the 2.6.18 Xen stock kernel with the c1.medium and c1.xlarge instances.
-- Although the default Amazon EC2 kernels will work, the new kernels provide
-- greater stability and performance for these instance types. For more
-- information about kernels, see Kernels, RAM Disks, and Block Device
-- Mappings.
module Network.AWS.EC2.RunInstances where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
runInstances :: Text
             -- ^ Unique ID of a machine image, returned by a call to DescribeImages.
             -> Int
             -- ^ Maximum number of instances to launch. If the value is more than Amazon EC2
             -- can launch, the largest possible number above minCount will be launched
             -- instead. Between 1 and the maximum number allowed for your account
             -- (default: 20).
             -> Int
             -- ^ Minimum number of instances to launch. If the value is more than Amazon EC2
             -- can launch, no instances are launched at all.
             -> RunInstances
runInstances p1 p2 p3 = RunInstances
    { riImageId = p1
    , riMaxCount = p2
    , riMinCount = p3
    , riAdditionalInfo = Nothing
    , riBlockDeviceMappings = []
    , riClientToken = Nothing
    , riDisableApiTermination = Nothing
    , riDryRun = Nothing
    , riEbsOptimized = Nothing
    , riIamInstanceProfile = Nothing
    , riInstanceInitiatedShutdownBehavior = Nothing
    , riInstanceType = Nothing
    , riKernelId = Nothing
    , riKeyName = Nothing
    , riLicense = Nothing
    , riMonitoring = Nothing
    , riNetworkInterfaces = []
    , riPlacement = Nothing
    , riPrivateIpAddress = Nothing
    , riRamdiskId = Nothing
    , riSecurityGroupIds = []
    , riSecurityGroups = []
    , riSubnetId = Nothing
    , riUserData = Nothing
    }

data RunInstances = RunInstances
    { riAdditionalInfo :: Maybe Text
      -- ^ Do not use. Reserved for internal use.
    , riBlockDeviceMappings :: [BlockDeviceMapping]
      -- ^ Specifies how block devices are exposed to the instance. Each mapping is
      -- made up of a virtualName and a deviceName.
    , riClientToken :: Maybe Text
      -- ^ Unique, case-sensitive identifier you provide to ensure idempotency of the
      -- request. For more information, go to How to Ensure Idempotency in the
      -- Amazon Elastic Compute Cloud User Guide.
    , riDisableApiTermination :: Maybe Bool
      -- ^ Specifies whether the instance can be terminated using the APIs. You must
      -- modify this attribute before you can terminate any "locked" instances from
      -- the APIs.
    , riDryRun :: Maybe Bool
    , riEbsOptimized :: Maybe Bool
    , riIamInstanceProfile :: Maybe IamInstanceProfileSpecification
    , riImageId :: !Text
      -- ^ Unique ID of a machine image, returned by a call to DescribeImages.
    , riInstanceInitiatedShutdownBehavior :: Maybe ShutdownBehavior
      -- ^ Specifies whether the instance's Amazon EBS volumes are stopped or
      -- terminated when the instance is shut down.
    , riInstanceType :: Maybe InstanceType
      -- ^ Specifies the instance type for the launched instances.
    , riKernelId :: Maybe Text
      -- ^ The ID of the kernel with which to launch the instance.
    , riKeyName :: Maybe Text
      -- ^ The name of the key pair.
    , riLicense :: Maybe InstanceLicenseSpecification
      -- ^ Specifies active licenses in use and attached to an Amazon EC2 instance.
    , riMaxCount :: !Int
      -- ^ Maximum number of instances to launch. If the value is more than Amazon EC2
      -- can launch, the largest possible number above minCount will be launched
      -- instead. Between 1 and the maximum number allowed for your account
      -- (default: 20).
    , riMinCount :: !Int
      -- ^ Minimum number of instances to launch. If the value is more than Amazon EC2
      -- can launch, no instances are launched at all.
    , riMonitoring :: Maybe RunInstancesMonitoringEnabled
      -- ^ Enables monitoring for the instance.
    , riNetworkInterfaces :: [InstanceNetworkInterfaceSpecification]
      -- ^ List of network interfaces associated with the instance.
    , riPlacement :: Maybe Placement
      -- ^ Specifies the placement constraints (Availability Zones) for launching the
      -- instances.
    , riPrivateIpAddress :: Maybe Text
      -- ^ If you're using Amazon Virtual Private Cloud, you can optionally use this
      -- parameter to assign the instance a specific available IP address from the
      -- subnet.
    , riRamdiskId :: Maybe Text
      -- ^ The ID of the RAM disk with which to launch the instance. Some kernels
      -- require additional drivers at launch. Check the kernel requirements for
      -- information on whether you need to specify a RAM disk. To find kernel
      -- requirements, go to the Resource Center and search for the kernel ID.
    , riSecurityGroupIds :: [Text]
    , riSecurityGroups :: [Text]
      -- ^ The names of the security groups into which the instances will be launched.
    , riSubnetId :: Maybe Text
      -- ^ Specifies the subnet ID within which to launch the instance(s) for Amazon
      -- Virtual Private Cloud.
    , riUserData :: Maybe Blob
      -- ^ Specifies additional information to make available to the instance(s). This
      -- parameter must be passed as a Base64-encoded string.
    } deriving (Eq, Show, Generic)

instance ToQuery RunInstances

instance AWSRequest RunInstances where
    type Er RunInstances = EC2Error
    type Rs RunInstances = RunInstancesResponse
    request = getQuery service "RunInstances"

data RunInstancesResponse = RunInstancesResponse
    { rirGroups :: [GroupIdentifier]
      -- ^ The list of security groups requested for the instances in this
      -- reservation.
    , rirInstances :: [Instance]
      -- ^ The list of Amazon EC2 instances included in this reservation.
    , rirOwnerId :: Maybe Text
      -- ^ The AWS Access Key ID of the user who owns the reservation.
    , rirRequesterId :: Maybe Text
      -- ^ The unique ID of the user who requested the instances in this reservation.
    , rirReservationId :: Maybe Text
      -- ^ The unique ID of this reservation.
    } deriving (Eq, Show, Generic)

instance FromXML RunInstancesResponse where
    fromXMLOptions = xmlOptions
