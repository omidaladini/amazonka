{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeVolumes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the status of the indicated volume or, in lieu of any specified,
-- all volumes belonging to the caller. Volumes that have been deleted are not
-- described.
module Network.AWS.EC2.DescribeVolumes where

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeVolumes = DescribeVolumes
    { dvtDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , dvtFilters :: [Filter]
      -- ^ A list of filters used to match properties for Volumes. For a complete
      -- reference to the available filter keys for this operation, see the Amazon
      -- EC2 API reference.
    , dvtVolumeIds :: [Text]
      -- ^ The optional list of EBS volumes to describe.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeVolumes

instance AWSRequest DescribeVolumes where
    type Er DescribeVolumes = EC2Error
    type Rs DescribeVolumes = DescribeVolumesResponse
    request = v2Query service GET "DescribeVolumes"

data DescribeVolumesResponse = DescribeVolumesResponse
    { dvtrsVolumes :: [Volume]
      -- ^ The list of described EBS volumes.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeVolumesResponse where
    fromXMLOptions = xmlOptions
