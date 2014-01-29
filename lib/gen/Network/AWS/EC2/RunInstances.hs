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

import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
runInstances :: Text
             -> Int
             -> Int
             -> RunInstances
runInstances p1 p2 p3 = undefined $ RunInstances
    { rirImageId = p1
    , rirMaxCount = p2
    , rirMinCount = p3
    , rirAdditionalInfo = Nothing
    , rirBlockDeviceMappings = []
    , rirClientToken = Nothing
    , rirDisableApiTermination = Nothing
    , rirDryRun = Nothing
    , rirEbsOptimized = Nothing
    , rirIamInstanceProfile = Nothing
    , rirInstanceInitiatedShutdownBehavior = Nothing
    , rirInstanceType = Nothing
    , rirKernelId = Nothing
    , rirKeyName = Nothing
    , rirLicense = Nothing
    , rirMonitoring = Nothing
    , rirNetworkInterfaces = []
    , rirPlacement = Nothing
    , rirPrivateIpAddress = Nothing
    , rirRamdiskId = Nothing
    , rirSecurityGroupIds = []
    , rirSecurityGroups = []
    , rirSubnetId = Nothing
    , rirUserData = Nothing
    }

data RunInstances = RunInstances
    { rirAdditionalInfo :: Maybe Text
      -- ^ Do not use. Reserved for internal use.
    , rirBlockDeviceMappings :: [BlockDeviceMapping]
      -- ^ Specifies how block devices are exposed to the instance. Each mapping is
      -- made up of a virtualName and a deviceName.
    , rirClientToken :: Maybe Text
      -- ^ Unique, case-sensitive identifier you provide to ensure idempotency of the
      -- request. For more information, go to How to Ensure Idempotency in the
      -- Amazon Elastic Compute Cloud User Guide.
    , rirDisableApiTermination :: Maybe Bool
      -- ^ Specifies whether the instance can be terminated using the APIs. You must
      -- modify this attribute before you can terminate any "locked" instances from
      -- the APIs.
    , rirDryRun :: Maybe Bool
    , rirEbsOptimized :: Maybe Bool
    , rirIamInstanceProfile :: Maybe IamInstanceProfileSpecification
    , rirImageId :: !Text
      -- ^ Unique ID of a machine image, returned by a call to DescribeImages.
    , rirInstanceInitiatedShutdownBehavior :: Maybe ShutdownBehavior
      -- ^ Specifies whether the instance's Amazon EBS volumes are stopped or
      -- terminated when the instance is shut down.
    , rirInstanceType :: Maybe InstanceType
      -- ^ Specifies the instance type for the launched instances.
    , rirKernelId :: Maybe Text
      -- ^ The ID of the kernel with which to launch the instance.
    , rirKeyName :: Maybe Text
      -- ^ The name of the key pair.
    , rirLicense :: Maybe InstanceLicenseSpecification
      -- ^ Specifies active licenses in use and attached to an Amazon EC2 instance.
    , rirMaxCount :: !Int
      -- ^ Maximum number of instances to launch. If the value is more than Amazon EC2
      -- can launch, the largest possible number above minCount will be launched
      -- instead. Between 1 and the maximum number allowed for your account
      -- (default: 20).
    , rirMinCount :: !Int
      -- ^ Minimum number of instances to launch. If the value is more than Amazon EC2
      -- can launch, no instances are launched at all.
    , rirMonitoring :: Maybe RunInstancesMonitoringEnabled
      -- ^ Enables monitoring for the instance.
    , rirNetworkInterfaces :: [InstanceNetworkInterfaceSpecification]
      -- ^ List of network interfaces associated with the instance.
    , rirPlacement :: Maybe Placement
      -- ^ Specifies the placement constraints (Availability Zones) for launching the
      -- instances.
    , rirPrivateIpAddress :: Maybe Text
      -- ^ If you're using Amazon Virtual Private Cloud, you can optionally use this
      -- parameter to assign the instance a specific available IP address from the
      -- subnet.
    , rirRamdiskId :: Maybe Text
      -- ^ The ID of the RAM disk with which to launch the instance. Some kernels
      -- require additional drivers at launch. Check the kernel requirements for
      -- information on whether you need to specify a RAM disk. To find kernel
      -- requirements, go to the Resource Center and search for the kernel ID.
    , rirSecurityGroupIds :: [Text]
    , rirSecurityGroups :: [Text]
      -- ^ The names of the security groups into which the instances will be launched.
    , rirSubnetId :: Maybe Text
      -- ^ Specifies the subnet ID within which to launch the instance(s) for Amazon
      -- Virtual Private Cloud.
    , rirUserData :: Maybe Blob
      -- ^ Specifies additional information to make available to the instance(s). This
      -- parameter must be passed as a Base64-encoded string.
    } deriving (Eq, Show, Generic)

instance ToQuery RunInstances

instance AWSRequest RunInstances where
    type Er RunInstances = EC2Error
    type Rs RunInstances = RunInstancesResponse
    request = getQuery service "RunInstances"

data RunInstancesResponse = RunInstancesResponse
    { rirrsGroups :: [GroupIdentifier]
      -- ^ The list of security groups requested for the instances in this
      -- reservation.
    , rirrsInstances :: [Instance]
      -- ^ The list of Amazon EC2 instances included in this reservation.
    , rirrsOwnerId :: Maybe Text
      -- ^ The AWS Access Key ID of the user who owns the reservation.
    , rirrsRequesterId :: Maybe Text
      -- ^ The unique ID of the user who requested the instances in this reservation.
    , rirrsReservationId :: Maybe Text
      -- ^ The unique ID of this reservation.
    } deriving (Eq, Show, Generic)

instance FromXML RunInstancesResponse where
    fromXMLOptions = xmlOptions
