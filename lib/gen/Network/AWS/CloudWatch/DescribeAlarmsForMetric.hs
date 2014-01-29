{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch.DescribeAlarmsForMetric
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves all alarms for a single metric. Specify a statistic, period, or
-- unit to filter the set of alarms further.
module Network.AWS.CloudWatch.DescribeAlarmsForMetric where

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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
describeAlarmsForMetric :: Text
                        -> Text
                        -> DescribeAlarmsForMetric
describeAlarmsForMetric p1 p2 = undefined $ DescribeAlarmsForMetric
    { dafmiMetricName = p1
    , dafmiNamespace = p2
    , dafmiDimensions = []
    , dafmiPeriod = Nothing
    , dafmiStatistic = Nothing
    , dafmiUnit = Nothing
    }

data DescribeAlarmsForMetric = DescribeAlarmsForMetric
    { dafmiDimensions :: [Dimension]
      -- ^ The list of dimensions associated with the metric.
    , dafmiMetricName :: !Text
      -- ^ The name of the metric.
    , dafmiNamespace :: !Text
      -- ^ The namespace of the metric.
    , dafmiPeriod :: Maybe Int
      -- ^ The period in seconds over which the statistic is applied.
    , dafmiStatistic :: Maybe Statistic
      -- ^ The statistic for the metric.
    , dafmiUnit :: Maybe StandardUnit
      -- ^ The unit for the metric.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeAlarmsForMetric

instance AWSRequest DescribeAlarmsForMetric where
    type Er DescribeAlarmsForMetric = CloudWatchError
    type Rs DescribeAlarmsForMetric = DescribeAlarmsForMetricResponse
    request = getQuery service "DescribeAlarmsForMetric"

data DescribeAlarmsForMetricResponse = DescribeAlarmsForMetricResponse
    { dafmirsMetricAlarms :: [MetricAlarm]
      -- ^ A list of information for each alarm with the specified metric.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeAlarmsForMetricResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeAlarmsForMetricResponse"
        :| ["DescribeAlarmsForMetricResult"]
