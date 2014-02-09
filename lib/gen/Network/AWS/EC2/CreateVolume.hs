{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateVolume
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Initializes an empty volume of a given size.
module Network.AWS.EC2.CreateVolume where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
createVolume :: Text
             -- ^ The Availability Zone in which to create the new volume.
             -> CreateVolume
createVolume p1 = CreateVolume
    { cvdAvailabilityZone = p1
    , cvdDryRun = Nothing
    , cvdIops = Nothing
    , cvdSize = Nothing
    , cvdSnapshotId = Nothing
    , cvdVolumeType = Nothing
    }

data CreateVolume = CreateVolume
    { cvdAvailabilityZone :: !Text
      -- ^ The Availability Zone in which to create the new volume.
    , cvdDryRun :: Maybe Bool
    , cvdIops :: Maybe Int
    , cvdSize :: Maybe Int
      -- ^ The size of the volume, in gigabytes. Required if you are not creating a
      -- volume from a snapshot.
    , cvdSnapshotId :: Maybe Text
      -- ^ The ID of the snapshot from which to create the new volume.
    , cvdVolumeType :: Maybe VolumeType
    } deriving (Eq, Show, Generic)

instance ToQuery CreateVolume

instance AWSRequest CreateVolume where
    type Er CreateVolume = EC2Error
    type Rs CreateVolume = CreateVolumeResponse
    request  = postQuery service "CreateVolume"
    response = responseXML

data CreateVolumeResponse = CreateVolumeResponse
    { cvdrAttachments :: [VolumeAttachment]
      -- ^ Information on what this volume is attached to.
    , cvdrAvailabilityZone :: Maybe Text
      -- ^ Availability zone in which this volume was created.
    , cvdrCreateTime :: Maybe UTCTime
      -- ^ Timestamp when volume creation was initiated.
    , cvdrIops :: Maybe Int
    , cvdrSize :: Maybe Int
      -- ^ The size of this volume, in gigabytes.
    , cvdrSnapshotId :: Maybe Text
      -- ^ Optional snapshot from which this volume was created.
    , cvdrState :: Maybe VolumeState
      -- ^ State of this volume (e.g., creating, available).
    , cvdrTags :: [Tag]
      -- ^ A list of tags for the Volume.
    , cvdrVolumeId :: Maybe Text
      -- ^ The unique ID of this volume.
    , cvdrVolumeType :: Maybe VolumeType
    } deriving (Eq, Show, Generic)

instance FromXML CreateVolumeResponse where
    fromXMLOptions = xmlOptions
