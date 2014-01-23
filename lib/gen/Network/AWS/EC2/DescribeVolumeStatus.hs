{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeVolumeStatus
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the status of a volume.
module Network.AWS.EC2.DescribeVolumeStatus where

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeVolumeStatus = DescribeVolumeStatus
    { dvsrDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , dvsrFilters :: [Filter]
      -- ^ FIXME: Missing documentation
    , dvsrMaxResults :: Maybe Int
      -- ^ FIXME: Missing documentation
    , dvsrNextToken :: Maybe Text
      -- ^ FIXME: Missing documentation
    , dvsrVolumeIds :: [Text]
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeVolumeStatus

instance AWSRequest DescribeVolumeStatus where
    type Er DescribeVolumeStatus = EC2Error
    type Rs DescribeVolumeStatus = DescribeVolumeStatusResponse
    request = v2Query service GET "DescribeVolumeStatus"

instance AWSPager DescribeVolumeStatus where
    next rq rs
        | Just x <- dvsrrsNextToken rs = Just $ rq { dvsrNextToken = Just x }
        | otherwise = Nothing

data DescribeVolumeStatusResponse = DescribeVolumeStatusResponse
    { dvsrrsNextToken :: Maybe Text
      -- ^ FIXME: Missing documentation
    , dvsrrsVolumeStatuses :: [VolumeStatusItem]
      -- ^ FIXME: Missing documentation
    } deriving (Eq, Show, Generic)

instance FromXML DescribeVolumeStatusResponse where
    fromXMLOptions = xmlOptions
