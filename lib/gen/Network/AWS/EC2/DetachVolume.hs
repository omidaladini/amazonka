{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DetachVolume
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Detach a previously attached volume from a running instance.
module Network.AWS.EC2.DetachVolume where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
detachVolume :: Text
             -- ^ The ID of the volume to detach.
             -> DetachVolume
detachVolume p1 = DetachVolume
    { dvrVolumeId = p1
    , dvrDevice = Nothing
    , dvrDryRun = Nothing
    , dvrForce = Nothing
    , dvrInstanceId = Nothing
    }

data DetachVolume = DetachVolume
    { dvrDevice :: Maybe Text
      -- ^ The device name to which the volume is attached on the specified instance.
    , dvrDryRun :: Maybe Bool
    , dvrForce :: Maybe Bool
      -- ^ Forces detachment if the previous detachment attempt did not occur cleanly
      -- (logging into an instance, unmounting the volume, and detaching normally).
      -- This option can lead to data loss or a corrupted file system. Use this
      -- option only as a last resort to detach a volume from a failed instance. The
      -- instance will not have an opportunity to flush file system caches nor file
      -- system meta data. If you use this option, you must perform file system
      -- check and repair procedures.
    , dvrInstanceId :: Maybe Text
      -- ^ The ID of the instance from which to detach the the specified volume.
    , dvrVolumeId :: !Text
      -- ^ The ID of the volume to detach.
    } deriving (Eq, Show, Generic)

instance ToQuery DetachVolume

instance AWSRequest DetachVolume where
    type Er DetachVolume = EC2Error
    type Rs DetachVolume = DetachVolumeResponse
    request = getQuery service "DetachVolume"

data DetachVolumeResponse = DetachVolumeResponse
    { dvrrsAttachTime :: Maybe UTCTime
      -- ^ Timestamp when this attachment initiated.
    , dvrrsDeleteOnTermination :: Maybe Bool
      -- ^ ` Whether this volume will be deleted or not when the associated instance
      -- is terminated.
    , dvrrsDevice :: Maybe Text
      -- ^ How the device is exposed to the instance (e.g., /dev/sdh).
    , dvrrsInstanceId :: Maybe Text
    , dvrrsState :: Maybe VolumeAttachmentState
    , dvrrsVolumeId :: Maybe Text
    } deriving (Eq, Show, Generic)

instance FromXML DetachVolumeResponse where
    fromXMLOptions = xmlOptions
