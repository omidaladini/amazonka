{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch.GetMetricStatistics
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets statistics for the specified metric. The maximum number of data points
-- returned from a single GetMetricStatistics request is 1,440. If a request
-- is made that generates more than 1,440 data points, Amazon CloudWatch
-- returns an error. In such a case, alter the request by narrowing the
-- specified time range or increasing the specified period. Alternatively,
-- make multiple requests across adjacent time ranges. Amazon CloudWatch
-- aggregates data points based on the length of the period that you specify.
-- For example, if you request statistics with a one-minute granularity,
-- Amazon CloudWatch aggregates data points with time stamps that fall within
-- the same one-minute period. In such a case, the data points queried can
-- greatly outnumber the data points returned. The maximum number of data
-- points that can be queried is 50,850; whereas the maximum number of data
-- points returned is 1,440. The following examples show various statistics
-- allowed by the data point query maximum of 50,850 when you call
-- GetMetricStatistics on Amazon EC2 instances with detailed (one-minute)
-- monitoring enabled: Statistics for up to 400 instances for a span of one
-- hour Statistics for up to 35 instances over a span of 24 hours Statistics
-- for up to 2 instances over a span of 2 weeks.
module Network.AWS.CloudWatch.GetMetricStatistics where

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

import Network.AWS.CloudWatch.Service
import Network.AWS.CloudWatch.Types

-- | Convenience method utilising default fields where applicable.
getMetricStatistics :: UTCTime
                    -> Text
                    -> Text
                    -> Int
                    -> UTCTime
                    -> [Statistic]
                    -> AWS (Either CloudWatchError GetMetricStatisticsResponse)
getMetricStatistics p1 p2 p3 p4 p5 p6 = undefined $ GetMetricStatistics
    { gmsiEndTime = p1
    , gmsiMetricName = p2
    , gmsiNamespace = p3
    , gmsiPeriod = p4
    , gmsiStartTime = p5
    , gmsiStatistics = p6
    , gmsiDimensions = []
    , gmsiUnit = Nothing
    }

data GetMetricStatistics = GetMetricStatistics
    { gmsiDimensions :: [Dimension]
      -- ^ A list of dimensions describing qualities of the metric.
    , gmsiEndTime :: !UTCTime
      -- ^ The time stamp to use for determining the last datapoint to return. The
      -- value specified is exclusive; results will include datapoints up to the
      -- time stamp specified.
    , gmsiMetricName :: !Text
      -- ^ The name of the metric.
    , gmsiNamespace :: !Text
      -- ^ The namespace of the metric.
    , gmsiPeriod :: !Int
      -- ^ The granularity, in seconds, of the returned datapoints. Period must be at
      -- least 60 seconds and must be a multiple of 60. The default value is 60.
    , gmsiStartTime :: !UTCTime
      -- ^ The time stamp to use for determining the first datapoint to return. The
      -- value specified is inclusive; results include datapoints with the time
      -- stamp specified. The specified start time is rounded down to the nearest
      -- value. Datapoints are returned for start times up to two weeks in the past.
      -- Specified start times that are more than two weeks in the past will not
      -- return datapoints for metrics that are older than two weeks.
    , gmsiStatistics :: [Statistic]
      -- ^ The metric statistics to return.
    , gmsiUnit :: Maybe StandardUnit
      -- ^ The unit for the metric.
    } deriving (Eq, Show, Generic)

instance ToQuery GetMetricStatistics

instance AWSRequest GetMetricStatistics where
    type Er GetMetricStatistics = CloudWatchError
    type Rs GetMetricStatistics = GetMetricStatisticsResponse
    request = getQuery service "GetMetricStatistics"

data GetMetricStatisticsResponse = GetMetricStatisticsResponse
    { gmsirsDatapoints :: [Datapoint]
      -- ^ The datapoints for the specified metric.
    , gmsirsLabel :: Maybe Text
      -- ^ A label describing the specified metric.
    } deriving (Eq, Show, Generic)

instance FromXML GetMetricStatisticsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "GetMetricStatisticsResponse"
        :| ["GetMetricStatisticsResult"]
