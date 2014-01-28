-- Module      : Network.AWS.CloudWatch
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.CloudWatch
    (
    -- * Operations
    -- ** EnableAlarmActions
      module Network.AWS.CloudWatch.EnableAlarmActions
    -- ** PutMetricData
    , module Network.AWS.CloudWatch.PutMetricData
    -- ** DescribeAlarms
    , module Network.AWS.CloudWatch.DescribeAlarms
    -- ** ListMetrics
    , module Network.AWS.CloudWatch.ListMetrics
    -- ** DeleteAlarms
    , module Network.AWS.CloudWatch.DeleteAlarms
    -- ** DescribeAlarmHistory
    , module Network.AWS.CloudWatch.DescribeAlarmHistory
    -- ** GetMetricStatistics
    , module Network.AWS.CloudWatch.GetMetricStatistics
    -- ** DescribeAlarmsForMetric
    , module Network.AWS.CloudWatch.DescribeAlarmsForMetric
    -- ** DisableAlarmActions
    , module Network.AWS.CloudWatch.DisableAlarmActions
    -- ** PutMetricAlarm
    , module Network.AWS.CloudWatch.PutMetricAlarm
    -- ** SetAlarmState
    , module Network.AWS.CloudWatch.SetAlarmState

    -- * Types
    -- ** StatisticSet
    , StatisticSet (..)
    -- ** MetricDatum
    , MetricDatum (..)
    -- ** MetricAlarm
    , MetricAlarm (..)
    -- ** Metric
    , Metric (..)
    -- ** DimensionFilter
    , DimensionFilter (..)
    -- ** Dimension
    , Dimension (..)
    -- ** Datapoint
    , Datapoint (..)
    -- ** AlarmHistoryItem
    , AlarmHistoryItem (..)
    -- ** Statistic
    , Statistic (..)
    -- ** StateValue
    , StateValue (..)
    -- ** StandardUnit
    , StandardUnit (..)
    -- ** HistoryItemType
    , HistoryItemType (..)
    -- ** ComparisonOperator
    , ComparisonOperator (..)

    -- * Errors
    , CloudWatchError (..)
    ) where

import Network.AWS.CloudWatch.Service
import Network.AWS.CloudWatch.Types

import Network.AWS.CloudWatch.EnableAlarmActions
import Network.AWS.CloudWatch.PutMetricData
import Network.AWS.CloudWatch.DescribeAlarms
import Network.AWS.CloudWatch.ListMetrics
import Network.AWS.CloudWatch.DeleteAlarms
import Network.AWS.CloudWatch.DescribeAlarmHistory
import Network.AWS.CloudWatch.GetMetricStatistics
import Network.AWS.CloudWatch.DescribeAlarmsForMetric
import Network.AWS.CloudWatch.DisableAlarmActions
import Network.AWS.CloudWatch.PutMetricAlarm
import Network.AWS.CloudWatch.SetAlarmState
