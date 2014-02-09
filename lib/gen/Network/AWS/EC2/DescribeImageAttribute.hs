{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeImageAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeImageAttribute operation returns information about an attribute
-- of an AMI. Only one attribute can be specified per call.
module Network.AWS.EC2.DescribeImageAttribute where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
describeImageAttribute :: ImageAttributeName
                       -- ^ The name of the attribute to describe. Available attribute names:
                       -- productCodes, kernel, ramdisk, launchPermisson, blockDeviceMapping.
                       -> Text
                       -- ^ The ID of the AMI whose attribute is to be described.
                       -> DescribeImageAttribute
describeImageAttribute p1 p2 = DescribeImageAttribute
    { diasAttribute = p1
    , diasImageId = p2
    , diasDryRun = Nothing
    }

data DescribeImageAttribute = DescribeImageAttribute
    { diasAttribute :: !ImageAttributeName
      -- ^ The name of the attribute to describe. Available attribute names:
      -- productCodes, kernel, ramdisk, launchPermisson, blockDeviceMapping.
    , diasDryRun :: Maybe Bool
    , diasImageId :: !Text
      -- ^ The ID of the AMI whose attribute is to be described.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeImageAttribute

instance AWSRequest DescribeImageAttribute where
    type Er DescribeImageAttribute = EC2Error
    type Rs DescribeImageAttribute = DescribeImageAttributeResponse
    request = getQuery service "DescribeImageAttribute"

data DescribeImageAttributeResponse = DescribeImageAttributeResponse
    { diasrBlockDeviceMappings :: [BlockDeviceMapping]
      -- ^ Block device mappings for the associated AMI.
    , diasrDescription :: Maybe AttributeValue
      -- ^ User-created description of the associated AMI.
    , diasrImageId :: Maybe Text
      -- ^ The ID of the associated AMI.
    , diasrKernelId :: Maybe AttributeValue
      -- ^ Kernel ID of the associated AMI.
    , diasrLaunchPermissions :: [LaunchPermission]
      -- ^ Launch permissions for the associated AMI.
    , diasrProductCodes :: [ProductCode]
      -- ^ Product codes for the associated AMI.
    , diasrRamdiskId :: Maybe AttributeValue
      -- ^ Ramdisk ID of the associated AMI.
    , diasrSriovNetSupport :: Maybe AttributeValue
      -- ^ String value.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeImageAttributeResponse where
    fromXMLOptions = xmlOptions
