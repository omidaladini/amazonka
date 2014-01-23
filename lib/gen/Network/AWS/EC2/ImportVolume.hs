{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ImportVolume
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- FIXME: Operation documentation for ImportVolume
module Network.AWS.EC2.ImportVolume where

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data ImportVolume = ImportVolume
    { ivrAvailabilityZone :: Maybe Text
      -- ^ FIXME: Missing documentation
    , ivrDescription :: Maybe Text
      -- ^ FIXME: Missing documentation
    , ivrDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , ivrImage :: Maybe DiskImageDetail
      -- ^ FIXME: Missing documentation
    , ivrVolume :: Maybe VolumeDetail
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery ImportVolume

instance AWSRequest ImportVolume where
    type Er ImportVolume = EC2Error
    type Rs ImportVolume = ImportVolumeResponse
    request = v2Query service GET "ImportVolume"

data ImportVolumeResponse = ImportVolumeResponse
    { ivrrsConversionTask :: Maybe ConversionTask
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance FromXML ImportVolumeResponse where
    fromXMLOptions = xmlOptions
