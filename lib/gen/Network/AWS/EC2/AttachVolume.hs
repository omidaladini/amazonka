{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.AttachVolume
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Attach a previously created volume to a running instance.
module Network.AWS.EC2.AttachVolume where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
attachVolume :: Text
             -- ^ Specifies how the device is exposed to the instance (e.g., /dev/sdh).
             -> Text
             -- ^ The ID of the instance to which the volume attaches. The volume and
             -- instance must be within the same Availability Zone and the instance must be
             -- running.
             -> Text
             -- ^ The ID of the Amazon EBS volume. The volume and instance must be within the
             -- same Availability Zone and the instance must be running.
             -> AttachVolume
attachVolume p1 p2 p3 = AttachVolume
    { avDevice = p1
    , avInstanceId = p2
    , avVolumeId = p3
    , avDryRun = Nothing
    }

data AttachVolume = AttachVolume
    { avDevice :: !Text
      -- ^ Specifies how the device is exposed to the instance (e.g., /dev/sdh).
    , avDryRun :: Maybe Bool
    , avInstanceId :: !Text
      -- ^ The ID of the instance to which the volume attaches. The volume and
      -- instance must be within the same Availability Zone and the instance must be
      -- running.
    , avVolumeId :: !Text
      -- ^ The ID of the Amazon EBS volume. The volume and instance must be within the
      -- same Availability Zone and the instance must be running.
    } deriving (Eq, Show, Generic)

instance ToQuery AttachVolume

instance AWSRequest AttachVolume where
    type Er AttachVolume = EC2Error
    type Rs AttachVolume = AttachVolumeResponse
    request = getQuery service "AttachVolume"

data AttachVolumeResponse = AttachVolumeResponse
    { avrAttachTime :: Maybe UTCTime
      -- ^ Timestamp when this attachment initiated.
    , avrDeleteOnTermination :: Maybe Bool
      -- ^ ` Whether this volume will be deleted or not when the associated instance
      -- is terminated.
    , avrDevice :: Maybe Text
      -- ^ How the device is exposed to the instance (e.g., /dev/sdh).
    , avrInstanceId :: Maybe Text
    , avrState :: Maybe VolumeAttachmentState
    , avrVolumeId :: Maybe Text
    } deriving (Eq, Show, Generic)

instance FromXML AttachVolumeResponse where
    fromXMLOptions = xmlOptions
