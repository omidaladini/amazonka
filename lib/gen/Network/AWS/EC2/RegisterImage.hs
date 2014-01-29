{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.RegisterImage
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The RegisterImage operation registers an AMI with Amazon EC2. Images must
-- be registered before they can be launched. For more information, see
-- RunInstances. Each AMI is associated with an unique ID which is provided by
-- the Amazon EC2 service through the RegisterImage operation. During
-- registration, Amazon EC2 retrieves the specified image manifest from Amazon
-- S3 and verifies that the image is owned by the user registering the image.
-- The image manifest is retrieved once and stored within the Amazon EC2. Any
-- modifications to an image in Amazon S3 invalidates this registration. If
-- you make changes to an image, deregister the previous image and register
-- the new image. For more information, see DeregisterImage.
module Network.AWS.EC2.RegisterImage where

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
registerImage :: AWS (Either EC2Error RegisterImageResponse)
registerImage = undefined $ RegisterImage
    { risArchitecture = Nothing
    , risBlockDeviceMappings = []
    , risDescription = Nothing
    , risDryRun = Nothing
    , risImageLocation = Nothing
    , risKernelId = Nothing
    , risName = Nothing
    , risRamdiskId = Nothing
    , risRootDeviceName = Nothing
    , risSriovNetSupport = Nothing
    , risVirtualizationType = Nothing
    }

data RegisterImage = RegisterImage
    { risArchitecture :: Maybe ArchitectureValues
      -- ^ The architecture of the image. Valid Values: i386, x86_64.
    , risBlockDeviceMappings :: [BlockDeviceMapping]
      -- ^ The block device mappings for the new AMI, which specify how different
      -- block devices (ex: EBS volumes and ephemeral drives) will be exposed on
      -- instances launched from the new image.
    , risDescription :: Maybe Text
      -- ^ The description describing the new AMI.
    , risDryRun :: Maybe Bool
    , risImageLocation :: Maybe Text
      -- ^ The full path to your AMI manifest in Amazon S3 storage.
    , risKernelId :: Maybe Text
      -- ^ The optional ID of a specific kernel to register with the new AMI.
    , risName :: Maybe Text
      -- ^ The name to give the new Amazon Machine Image. Constraints: 3-128
      -- alphanumeric characters, parenthesis (()), commas (,), slashes (/), dashes
      -- (-), or underscores(_).
    , risRamdiskId :: Maybe Text
      -- ^ The optional ID of a specific ramdisk to register with the new AMI. Some
      -- kernels require additional drivers at launch. Check the kernel requirements
      -- for information on whether you need to specify a RAM disk.
    , risRootDeviceName :: Maybe Text
      -- ^ The root device name (e.g., /dev/sda1).
    , risSriovNetSupport :: Maybe Text
    , risVirtualizationType :: Maybe Text
      -- ^ The type of virtualization.
    } deriving (Eq, Show, Generic)

instance ToQuery RegisterImage

instance AWSRequest RegisterImage where
    type Er RegisterImage = EC2Error
    type Rs RegisterImage = RegisterImageResponse
    request = getQuery service "RegisterImage"

data RegisterImageResponse = RegisterImageResponse
    { risrsImageId :: Maybe Text
      -- ^ The ID of the new Amazon Machine Image (AMI).
    } deriving (Eq, Show, Generic)

instance FromXML RegisterImageResponse where
    fromXMLOptions = xmlOptions
