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

data AttachVolume = AttachVolume
    { avrDevice :: !Text
      -- ^ Specifies how the device is exposed to the instance (e.g., /dev/sdh).
    , avrDryRun :: Maybe Bool
    , avrInstanceId :: !Text
      -- ^ The ID of the instance to which the volume attaches. The volume and
      -- instance must be within the same Availability Zone and the instance must be
      -- running.
    , avrVolumeId :: !Text
      -- ^ The ID of the Amazon EBS volume. The volume and instance must be within the
      -- same Availability Zone and the instance must be running.
    } deriving (Eq, Show, Generic)

instance ToQuery AttachVolume

instance AWSRequest AttachVolume where
    type Er AttachVolume = EC2Error
    type Rs AttachVolume = AttachVolumeResponse
    request = getQuery service "AttachVolume"

data AttachVolumeResponse = AttachVolumeResponse
    { avrrsAttachTime :: Maybe UTCTime
      -- ^ Timestamp when this attachment initiated.
    , avrrsDeleteOnTermination :: Maybe Bool
      -- ^ ` Whether this volume will be deleted or not when the associated instance
      -- is terminated.
    , avrrsDevice :: Maybe Text
      -- ^ How the device is exposed to the instance (e.g., /dev/sdh).
    , avrrsInstanceId :: Maybe Text
    , avrrsState :: Maybe VolumeAttachmentState
    , avrrsVolumeId :: Maybe Text
    } deriving (Eq, Show, Generic)

instance FromXML AttachVolumeResponse where
    fromXMLOptions = xmlOptions
