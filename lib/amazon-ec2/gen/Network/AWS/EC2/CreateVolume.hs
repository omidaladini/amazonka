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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
createVolume :: Text
             -> CreateVolume
createVolume p1 = CreateVolume
    { cvsAvailabilityZone = p1
    , cvsDryRun = Nothing
    , cvsIops = Nothing
    , cvsSize = Nothing
    , cvsSnapshotId = Nothing
    , cvsVolumeType = Nothing
    }

data CreateVolume = CreateVolume
    { cvsAvailabilityZone :: !Text
      -- ^ The Availability Zone in which to create the new volume.
    , cvsDryRun :: Maybe Bool
    , cvsIops :: Maybe Int
    , cvsSize :: Maybe Int
      -- ^ The size of the volume, in gigabytes. Required if you are not creating a
      -- volume from a snapshot.
    , cvsSnapshotId :: Maybe Text
      -- ^ The ID of the snapshot from which to create the new volume.
    , cvsVolumeType :: Maybe VolumeType
    } deriving (Eq, Show, Generic)

instance ToQuery CreateVolume

instance AWSRequest CreateVolume where
    type Er CreateVolume = EC2Error
    type Rs CreateVolume = CreateVolumeResponse
    request = getQuery service "CreateVolume"

data CreateVolumeResponse = CreateVolumeResponse
    { cvsrsAttachments :: [VolumeAttachment]
      -- ^ Information on what this volume is attached to.
    , cvsrsAvailabilityZone :: Maybe Text
      -- ^ Availability zone in which this volume was created.
    , cvsrsCreateTime :: Maybe UTCTime
      -- ^ Timestamp when volume creation was initiated.
    , cvsrsIops :: Maybe Int
    , cvsrsSize :: Maybe Int
      -- ^ The size of this volume, in gigabytes.
    , cvsrsSnapshotId :: Maybe Text
      -- ^ Optional snapshot from which this volume was created.
    , cvsrsState :: Maybe VolumeState
      -- ^ State of this volume (e.g., creating, available).
    , cvsrsTags :: [Tag]
      -- ^ A list of tags for the Volume.
    , cvsrsVolumeId :: Maybe Text
      -- ^ The unique ID of this volume.
    , cvsrsVolumeType :: Maybe VolumeType
    } deriving (Eq, Show, Generic)

instance FromXML CreateVolumeResponse where
    fromXMLOptions = xmlOptions
