{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateSnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Create a snapshot of the volume identified by volume ID. A volume does not
-- have to be detached at the time the snapshot is taken. Snapshot creation
-- requires that the system is in a consistent state. For instance, this means
-- that if taking a snapshot of a database, the tables must be read-only
-- locked to ensure that the snapshot will not contain a corrupted version of
-- the database. Therefore, be careful when using this API to ensure that the
-- system remains in the consistent state until the create snapshot status has
-- returned.
module Network.AWS.EC2.CreateSnapshot where

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data CreateSnapshot = CreateSnapshot
    { cstDescription :: Maybe Text
      -- ^ The description for the new snapshot.
    , cstDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , cstVolumeId :: !Text
      -- ^ The ID of the volume from which to create the snapshot.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateSnapshot

instance AWSRequest CreateSnapshot where
    type Er CreateSnapshot = EC2Error
    type Rs CreateSnapshot = CreateSnapshotResponse
    request = v2Query service GET "CreateSnapshot"

data CreateSnapshotResponse = CreateSnapshotResponse
    { cstrsDescription :: Maybe Text
      -- ^ Description of the snapshot.
    , cstrsOwnerAlias :: Maybe Text
      -- ^ The AWS account alias (e.g., "amazon", "redhat", "self", etc.) or AWS
      -- account ID that owns the AMI.
    , cstrsOwnerId :: Maybe Text
      -- ^ AWS Access Key ID of the user who owns the snapshot.
    , cstrsProgress :: Maybe Text
      -- ^ The progress of the snapshot, in percentage.
    , cstrsSnapshotId :: Maybe Text
      -- ^ The unique ID of this snapshot.
    , cstrsStartTime :: Maybe UTCTime
      -- ^ Time stamp when the snapshot was initiated.
    , cstrsState :: Maybe SnapshotState
      -- ^ Snapshot state (e.g., pending, completed, or error).
    , cstrsTags :: [Tag]
      -- ^ A list of tags for the Snapshot.
    , cstrsVolumeId :: Maybe Text
      -- ^ The ID of the volume from which this snapshot was created.
    , cstrsVolumeSize :: Maybe Int
      -- ^ The size of the volume, in gigabytes.
    } deriving (Eq, Show, Generic)

instance FromXML CreateSnapshotResponse where
    fromXMLOptions = xmlOptions
