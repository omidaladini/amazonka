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

-- | Convenience method utilising default fields where applicable.
describeImageAttribute :: ImageAttributeName
                       -> Text
                       -> AWS (Either EC2Error DescribeImageAttributeResponse)
describeImageAttribute p1 p2 = undefined $ DescribeImageAttribute
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
    { diasrsBlockDeviceMappings :: [BlockDeviceMapping]
      -- ^ Block device mappings for the associated AMI.
    , diasrsDescription :: Maybe AttributeValue
      -- ^ User-created description of the associated AMI.
    , diasrsImageId :: Maybe Text
      -- ^ The ID of the associated AMI.
    , diasrsKernelId :: Maybe AttributeValue
      -- ^ Kernel ID of the associated AMI.
    , diasrsLaunchPermissions :: [LaunchPermission]
      -- ^ Launch permissions for the associated AMI.
    , diasrsProductCodes :: [ProductCode]
      -- ^ Product codes for the associated AMI.
    , diasrsRamdiskId :: Maybe AttributeValue
      -- ^ Ramdisk ID of the associated AMI.
    , diasrsSriovNetSupport :: Maybe AttributeValue
      -- ^ String value.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeImageAttributeResponse where
    fromXMLOptions = xmlOptions
