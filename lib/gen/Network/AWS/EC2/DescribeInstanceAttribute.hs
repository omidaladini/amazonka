{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeInstanceAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about an attribute of an instance. Only one attribute
-- can be specified per call.
module Network.AWS.EC2.DescribeInstanceAttribute where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
describeInstanceAttribute :: InstanceAttributeName
                          -- ^ The name of the attribute to describe. Available attribute names:
                          -- instanceType, kernel, ramdisk, userData, disableApiTermination,
                          -- instanceInitiatedShutdownBehavior, rootDeviceName, blockDeviceMapping.
                          -> Text
                          -- ^ The ID of the instance whose instance attribute is being described.
                          -> DescribeInstanceAttribute
describeInstanceAttribute p1 p2 = DescribeInstanceAttribute
    { diaAttribute = p1
    , diaInstanceId = p2
    , diaDryRun = Nothing
    }

data DescribeInstanceAttribute = DescribeInstanceAttribute
    { diaAttribute :: !InstanceAttributeName
      -- ^ The name of the attribute to describe. Available attribute names:
      -- instanceType, kernel, ramdisk, userData, disableApiTermination,
      -- instanceInitiatedShutdownBehavior, rootDeviceName, blockDeviceMapping.
    , diaDryRun :: Maybe Bool
    , diaInstanceId :: !Text
      -- ^ The ID of the instance whose instance attribute is being described.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeInstanceAttribute

instance AWSRequest DescribeInstanceAttribute where
    type Er DescribeInstanceAttribute = EC2Error
    type Rs DescribeInstanceAttribute = DescribeInstanceAttributeResponse
    request = getQuery service "DescribeInstanceAttribute"

data DescribeInstanceAttributeResponse = DescribeInstanceAttributeResponse
    { diarBlockDeviceMappings :: [InstanceBlockDeviceMapping]
      -- ^ How block devices are exposed to this instance. Each mapping is made up of
      -- a virtualName and a deviceName.
    , diarDisableApiTermination :: Maybe AttributeBooleanValue
      -- ^ Whether this instance can be terminated. You must modify this attribute
      -- before you can terminate any "locked" instances.
    , diarEbsOptimized :: Maybe AttributeBooleanValue
      -- ^ Boolean value.
    , diarInstanceId :: Maybe Text
      -- ^ The ID of the associated instance.
    , diarInstanceInitiatedShutdownBehavior :: Maybe AttributeValue
      -- ^ Whether this instance's Amazon EBS volumes are deleted when the instance is
      -- shut down.
    , diarInstanceType :: Maybe AttributeValue
      -- ^ The instance type (e.g., m1.small, c1.medium, m2.2xlarge, and so on).
    , diarKernelId :: Maybe AttributeValue
      -- ^ The kernel ID of the associated instance.
    , diarProductCodes :: [ProductCode]
    , diarRamdiskId :: Maybe AttributeValue
      -- ^ The ramdisk ID of the associated instance.
    , diarRootDeviceName :: Maybe AttributeValue
      -- ^ The root device name (e.g., /dev/sda1).
    , diarSriovNetSupport :: Maybe AttributeValue
      -- ^ String value.
    , diarUserData :: Maybe AttributeValue
      -- ^ MIME, Base64-encoded user data.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeInstanceAttributeResponse where
    fromXMLOptions = xmlOptions
