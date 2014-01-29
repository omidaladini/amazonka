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
describeVolumeStatus :: DescribeVolumeStatus
describeVolumeStatus = DescribeVolumeStatus
    { dvsrDryRun = Nothing
    , dvsrFilters = []
    , dvsrMaxResults = Nothing
    , dvsrNextToken = Nothing
    , dvsrVolumeIds = []
    }

data DescribeVolumeStatus = DescribeVolumeStatus
    { dvsrDryRun :: Maybe Bool
    , dvsrFilters :: [Filter]
    , dvsrMaxResults :: Maybe Int
    , dvsrNextToken :: Maybe Text
    , dvsrVolumeIds :: [Text]
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeVolumeStatus

instance AWSRequest DescribeVolumeStatus where
    type Er DescribeVolumeStatus = EC2Error
    type Rs DescribeVolumeStatus = DescribeVolumeStatusResponse
    request = getQuery service "DescribeVolumeStatus"

instance AWSPager DescribeVolumeStatus where
    next rq rs
        | Just x <- dvsrrsNextToken rs = Just $ rq { dvsrNextToken = Just x }
        | otherwise = Nothing

data DescribeVolumeStatusResponse = DescribeVolumeStatusResponse
    { dvsrrsNextToken :: Maybe Text
    , dvsrrsVolumeStatuses :: [VolumeStatusItem]
    } deriving (Eq, Show, Generic)

instance FromXML DescribeVolumeStatusResponse where
    fromXMLOptions = xmlOptions
