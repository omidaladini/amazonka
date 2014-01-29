{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateImage
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an Amazon EBS-backed AMI from a "running" or "stopped" instance.
-- AMIs that use an Amazon EBS root device boot faster than AMIs that use
-- instance stores. They can be up to 1 TiB in size, use storage that persists
-- on instance failure, and can be stopped and started.
module Network.AWS.EC2.CreateImage where

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
createImage :: Text
            -> Text
            -> AWS (Either EC2Error CreateImageResponse)
createImage p1 p2 = undefined $ CreateImage
    { cisInstanceId = p1
    , cisName = p2
    , cisBlockDeviceMappings = []
    , cisDescription = Nothing
    , cisDryRun = Nothing
    , cisNoReboot = Nothing
    }

data CreateImage = CreateImage
    { cisBlockDeviceMappings :: [BlockDeviceMapping]
    , cisDescription :: Maybe Text
      -- ^ The description for the new AMI being created.
    , cisDryRun :: Maybe Bool
    , cisInstanceId :: !Text
      -- ^ The ID of the instance from which to create the new image.
    , cisName :: !Text
      -- ^ The name for the new AMI being created.
    , cisNoReboot :: Maybe Bool
      -- ^ By default this property is set to false, which means Amazon EC2 attempts
      -- to cleanly shut down the instance before image creation and reboots the
      -- instance afterwards. When set to true, Amazon EC2 will not shut down the
      -- instance before creating the image. When this option is used, file system
      -- integrity on the created image cannot be guaranteed.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateImage

instance AWSRequest CreateImage where
    type Er CreateImage = EC2Error
    type Rs CreateImage = CreateImageResponse
    request = getQuery service "CreateImage"

data CreateImageResponse = CreateImageResponse
    { cisrsImageId :: Maybe Text
      -- ^ The ID of the new AMI.
    } deriving (Eq, Show, Generic)

instance FromXML CreateImageResponse where
    fromXMLOptions = xmlOptions
