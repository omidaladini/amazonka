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

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data ImportVolume = ImportVolume
    { ivAvailabilityZone :: Maybe Text
    , ivDescription :: Maybe Text
    , ivDryRun :: Maybe Bool
    , ivImage :: Maybe DiskImageDetail
    , ivVolume :: Maybe VolumeDetail
    } deriving (Eq, Show, Generic)

instance ToQuery ImportVolume

instance AWSRequest ImportVolume where
    type Er ImportVolume = EC2Error
    type Rs ImportVolume = ImportVolumeResponse
    request = getQuery service "ImportVolume"

data ImportVolumeResponse = ImportVolumeResponse
    { ivrConversionTask :: Maybe ConversionTask
    } deriving (Eq, Show, Generic)

instance FromXML ImportVolumeResponse where
    fromXMLOptions = xmlOptions
