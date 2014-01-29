{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeSpotPriceHistory
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the Spot Price history. Spot Instances are instances that Amazon
-- EC2 starts on your behalf when the maximum price that you specify exceeds
-- the current Spot Price. Amazon EC2 periodically sets the Spot Price based
-- on available Spot Instance capacity and current spot instance requests. For
-- conceptual information about Spot Instances, refer to the Amazon Elastic
-- Compute Cloud Developer Guide or Amazon Elastic Compute Cloud User Guide .
module Network.AWS.EC2.DescribeSpotPriceHistory where

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

-- | Convenience method utilising default fields where applicable.
describeSpotPriceHistory :: AWS (Either EC2Error DescribeSpotPriceHistoryResponse)
describeSpotPriceHistory = undefined $ DescribeSpotPriceHistory
    { dsphrAvailabilityZone = Nothing
    , dsphrDryRun = Nothing
    , dsphrEndTime = Nothing
    , dsphrFilters = []
    , dsphrInstanceTypes = []
    , dsphrMaxResults = Nothing
    , dsphrNextToken = Nothing
    , dsphrProductDescriptions = []
    , dsphrStartTime = Nothing
    }

data DescribeSpotPriceHistory = DescribeSpotPriceHistory
    { dsphrAvailabilityZone :: Maybe Text
      -- ^ Filters the results by availability zone (ex: 'us-east-1a').
    , dsphrDryRun :: Maybe Bool
    , dsphrEndTime :: Maybe UTCTime
      -- ^ The end date and time of the Spot Instance price history data.
    , dsphrFilters :: [Filter]
      -- ^ A list of filters used to match properties for SpotPriceHistory. For a
      -- complete reference to the available filter keys for this operation, see the
      -- Amazon EC2 API reference.
    , dsphrInstanceTypes :: [InstanceType]
      -- ^ Specifies the instance type to return.
    , dsphrMaxResults :: Maybe Int
      -- ^ Specifies the number of rows to return.
    , dsphrNextToken :: Maybe Text
      -- ^ Specifies the next set of rows to return.
    , dsphrProductDescriptions :: [Text]
      -- ^ The description of the AMI.
    , dsphrStartTime :: Maybe UTCTime
      -- ^ The start date and time of the Spot Instance price history data.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeSpotPriceHistory

instance AWSRequest DescribeSpotPriceHistory where
    type Er DescribeSpotPriceHistory = EC2Error
    type Rs DescribeSpotPriceHistory = DescribeSpotPriceHistoryResponse
    request = getQuery service "DescribeSpotPriceHistory"

instance AWSPager DescribeSpotPriceHistory where
    next rq rs
        | Just x <- dsphrrsNextToken rs = Just $ rq { dsphrNextToken = Just x }
        | otherwise = Nothing

data DescribeSpotPriceHistoryResponse = DescribeSpotPriceHistoryResponse
    { dsphrrsNextToken :: Maybe Text
      -- ^ The string marking the next set of results returned. Displays empty if
      -- there are no more results to be returned.
    , dsphrrsSpotPriceHistory :: [SpotPrice]
    } deriving (Eq, Show, Generic)

instance FromXML DescribeSpotPriceHistoryResponse where
    fromXMLOptions = xmlOptions
