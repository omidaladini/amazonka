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

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data DescribeVolumeStatus = DescribeVolumeStatus
    { dvsDryRun :: Maybe Bool
    , dvsFilters :: [Filter]
    , dvsMaxResults :: Maybe Int
    , dvsNextToken :: Maybe Text
    , dvsVolumeIds :: [Text]
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeVolumeStatus

instance AWSRequest DescribeVolumeStatus where
    type Er DescribeVolumeStatus = EC2Error
    type Rs DescribeVolumeStatus = DescribeVolumeStatusResponse
    request  = postQuery service "DescribeVolumeStatus"
    response = responseXML

instance AWSPager DescribeVolumeStatus where
    next rq rs
        | Just x <- dvsrNextToken rs = Just $ rq { dvsNextToken = Just x }
        | otherwise = Nothing

data DescribeVolumeStatusResponse = DescribeVolumeStatusResponse
    { dvsrNextToken :: Maybe Text
    , dvsrVolumeStatuses :: [VolumeStatusItem]
    } deriving (Eq, Show, Generic)

instance FromXML DescribeVolumeStatusResponse where
    fromXMLOptions = xmlOptions
