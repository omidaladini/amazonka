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
    { diadAttribute = p1
    , diadImageId = p2
    , diadDryRun = Nothing
    }

data DescribeImageAttribute = DescribeImageAttribute
    { diadAttribute :: !ImageAttributeName
      -- ^ The name of the attribute to describe. Available attribute names:
      -- productCodes, kernel, ramdisk, launchPermisson, blockDeviceMapping.
    , diadDryRun :: Maybe Bool
    , diadImageId :: !Text
      -- ^ The ID of the AMI whose attribute is to be described.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeImageAttribute

instance AWSRequest DescribeImageAttribute where
    type Er DescribeImageAttribute = EC2Error
    type Rs DescribeImageAttribute = DescribeImageAttributeResponse
    request  = postQuery service "DescribeImageAttribute"
    response = responseXML

data DescribeImageAttributeResponse = DescribeImageAttributeResponse
    { diadrBlockDeviceMapping :: [BlockDeviceMapping]
      -- ^ Block device mappings for the associated AMI.
    , diadrDescription :: Maybe AttributeValue
      -- ^ User-created description of the associated AMI.
    , diadrImageId :: Maybe Text
      -- ^ The ID of the associated AMI.
    , diadrKernel :: Maybe AttributeValue
      -- ^ Kernel ID of the associated AMI.
    , diadrLaunchPermission :: [LaunchPermission]
      -- ^ Launch permissions for the associated AMI.
    , diadrProductCodes :: [ProductCode]
      -- ^ Product codes for the associated AMI.
    , diadrRamdisk :: Maybe AttributeValue
      -- ^ Ramdisk ID of the associated AMI.
    , diadrSriovNetSupport :: Maybe AttributeValue
      -- ^ String value.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeImageAttributeResponse where
    fromXMLOptions = xmlOptions
