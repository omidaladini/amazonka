{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.CreateLaunchConfiguration
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new launch configuration. The launch configuration name must be
-- unique within the scope of the client's AWS account. The maximum limit of
-- launch configurations, which by default is 100, must not yet have been met;
-- otherwise, the call will fail. When created, the new launch configuration
-- is available for immediate use.
-- https://autoscaling.amazonaws.com/?LaunchConfigurationName=my-test-lc
-- &AssociatePublicIpAddress=true &ImageId=ami-0078da69 &InstanceType=m1.small
-- &Action=CreateLaunchConfiguration &AUTHPARAMS
-- 7c6e177f-f082-11e1-ac58-3714bEXAMPLE.
module Network.AWS.AutoScaling.CreateLaunchConfiguration where

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

import Network.AWS.AutoScaling.Service
import Network.AWS.AutoScaling.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
createLaunchConfiguration :: Text
                          -> CreateLaunchConfiguration
createLaunchConfiguration p1 = CreateLaunchConfiguration
    { clctLaunchConfigurationName = p1
    , clctAssociatePublicIpAddress = Nothing
    , clctBlockDeviceMappings = []
    , clctEbsOptimized = Nothing
    , clctIamInstanceProfile = Nothing
    , clctImageId = Nothing
    , clctInstanceId = Nothing
    , clctInstanceMonitoring = Nothing
    , clctInstanceType = Nothing
    , clctKernelId = Nothing
    , clctKeyName = Nothing
    , clctRamdiskId = Nothing
    , clctSecurityGroups = []
    , clctSpotPrice = Nothing
    , clctUserData = Nothing
    }

data CreateLaunchConfiguration = CreateLaunchConfiguration
    { clctAssociatePublicIpAddress :: Maybe Bool
      -- ^ Used for Auto Scaling groups that launch instances into an Amazon Virtual
      -- Private Cloud (Amazon VPC). Specifies whether to assign a public IP address
      -- to each instance launched in a Amazon VPC. If you specify a value for this
      -- parameter, be sure to specify at least one VPC subnet using the
      -- VPCZoneIdentifier parameter when you create your Auto Scaling group.
      -- Default: If the instance is launched in default VPC, the default is true.
      -- If the instance is launched in a nondefault VPC (EC2-VPC), the default is
      -- false. For more information about the platforms supported by Auto Scaling,
      -- see Basic Auto Scaling Configuration.
    , clctBlockDeviceMappings :: [BlockDeviceMapping]
      -- ^ A list of mappings that specify how block devices are exposed to the
      -- instance. Each mapping is made up of a VirtualName, a DeviceName, and an
      -- ebs data structure that contains information about the associated Elastic
      -- Block Storage volume. For more information about Amazon EC2
      -- BlockDeviceMappings, go to Block Device Mapping in the Amazon EC2 product
      -- documentation.
    , clctEbsOptimized :: Maybe Bool
      -- ^ Whether the instance is optimized for EBS I/O. The optimization provides
      -- dedicated throughput to Amazon EBS and an optimized configuration stack to
      -- provide optimal EBS I/O performance. This optimization is not available
      -- with all instance types. Additional usage charges apply when using an EBS
      -- Optimized instance. By default the instance is not optimized for EBS I/O.
      -- For information about EBS-optimized instances, go to EBS-Optimized
      -- Instances in the Amazon Elastic Compute Cloud User Guide.
    , clctIamInstanceProfile :: Maybe Text
      -- ^ The name or the Amazon Resource Name (ARN) of the instance profile
      -- associated with the IAM role for the instance. Amazon EC2 instances
      -- launched with an IAM role will automatically have AWS security credentials
      -- available. You can use IAM roles with Auto Scaling to automatically enable
      -- applications running on your Amazon EC2 instances to securely access other
      -- AWS resources. For information on launching EC2 instances with an IAM role,
      -- go to Launching Auto Scaling Instances With an IAM Role in the Auto Scaling
      -- Developer Guide.
    , clctImageId :: Maybe Text
      -- ^ Unique ID of the Amazon Machine Image (AMI) you want to use to launch your
      -- EC2 instances. For information about finding Amazon EC2 AMIs, see Finding a
      -- Suitable AMI in the Amazon Elastic Compute Cloud User Guide.
    , clctInstanceId :: Maybe Text
      -- ^ The ID of the Amazon EC2 instance you want to use to create the launch
      -- configuration. Use this attribute if you want the launch configuration to
      -- derive its attributes from an EC2 instance. When you use an instance to
      -- create a launch configuration, all you need to specify is the InstanceId.
      -- The new launch configuration, by default, derives all the attributes from
      -- the specified instance with the exception of BlockDeviceMapping. If you
      -- want to create a launch configuration with BlockDeviceMapping or override
      -- any other instance attributes, specify them as part of the same request.
      -- For more information on using an InstanceID to create a launch
      -- configuration, see Create a Launch Configuration Using an Amazon EC2
      -- Instance in the Auto Scaling Developer Guide.
    , clctInstanceMonitoring :: Maybe InstanceMonitoring
      -- ^ Enables detailed monitoring if it is disabled. Detailed monitoring is
      -- enabled by default. When detailed monitoring is enabled, Amazon Cloudwatch
      -- will generate metrics every minute and your account will be charged a fee.
      -- When you disable detailed monitoring, by specifying False, Cloudwatch will
      -- generate metrics every 5 minutes. For more information, see Monitor Your
      -- Auto Scaling Instances. For information about Amazon CloudWatch, see the
      -- Amazon CloudWatch Developer Guide.
    , clctInstanceType :: Maybe Text
      -- ^ The instance type of the Amazon EC2 instance. For information about
      -- available Amazon EC2 instance types, see Available Instance Types in the
      -- Amazon Elastic Cloud Compute User Guide.
    , clctKernelId :: Maybe Text
      -- ^ The ID of the kernel associated with the Amazon EC2 AMI.
    , clctKeyName :: Maybe Text
      -- ^ The name of the Amazon EC2 key pair. For more information, see Getting a
      -- Key Pair in the Amazon Elastic Compute Cloud User Guide.
    , clctLaunchConfigurationName :: !Text
      -- ^ The name of the launch configuration to create.
    , clctRamdiskId :: Maybe Text
      -- ^ The ID of the RAM disk associated with the Amazon EC2 AMI.
    , clctSecurityGroups :: [Text]
      -- ^ The security groups with which to associate Amazon EC2 or Amazon VPC
      -- instances. If your instances are launched in EC2, you can either specify
      -- Amazon EC2 security group names or the security group IDs. For more
      -- information about Amazon EC2 security groups, see Using Security Groups in
      -- the Amazon Elastic Compute Cloud User Guide. If your instances are launched
      -- within VPC, specify Amazon VPC security group IDs. For more information
      -- about Amazon VPC security groups, see Security Groups in the Amazon Virtual
      -- Private Cloud User Guide.
    , clctSpotPrice :: Maybe Text
      -- ^ The maximum hourly price to be paid for any Spot Instance launched to
      -- fulfill the request. Spot Instances are launched when the price you specify
      -- exceeds the current Spot market price. For more information on launching
      -- Spot Instances, see Using Auto Scaling to Launch Spot Instances in the Auto
      -- Scaling Developer Guide.
    , clctUserData :: Maybe Blob
      -- ^ The user data to make available to the launched Amazon EC2 instances. For
      -- more information about Amazon EC2 user data, see User Data Retrieval in the
      -- Amazon Elastic Compute Cloud User Guide. At this time, Auto Scaling launch
      -- configurations don't support compressed (e.g. zipped) user data files.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateLaunchConfiguration

instance AWSRequest CreateLaunchConfiguration where
    type Er CreateLaunchConfiguration = AutoScalingError
    type Rs CreateLaunchConfiguration = CreateLaunchConfigurationResponse
    request = getQuery service "CreateLaunchConfiguration"

data CreateLaunchConfigurationResponse = CreateLaunchConfigurationResponse
    deriving (Eq, Show, Generic)

instance FromXML CreateLaunchConfigurationResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot CreateLaunchConfigurationResponse
