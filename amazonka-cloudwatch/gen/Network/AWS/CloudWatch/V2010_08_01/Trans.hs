{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Module      : Network.AWS.CloudWatch.V2010_08_01.Trans
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon CloudWatch is a monitoring service for AWS cloud resources and the
-- applications you run on AWS. You can use Amazon CloudWatch to collect and
-- track metrics, collect and monitor log files, and set alarms. Amazon
-- CloudWatch can monitor AWS resources such as Amazon EC2 instances, Amazon
-- DynamoDB tables, and Amazon RDS DB instances, as well as custom metrics
-- generated by your applications and services, and any log files your
-- applications generate. You can use Amazon CloudWatch to gain system-wide
-- visibility into resource utilization, application performance, and
-- operational health. You can use these insights to react and keep your
-- application running smoothly.
module Network.AWS.CloudWatch.V2010_08_01.Trans
    (
    -- * DeleteAlarms
      deleteAlarms
    -- * DescribeAlarmHistory
    , describeAlarmHistory
    -- * DescribeAlarms
    , describeAlarms
    -- * DescribeAlarmsForMetric
    , describeAlarmsForMetric
    -- * DisableAlarmActions
    , disableAlarmActions
    -- * EnableAlarmActions
    , enableAlarmActions
    -- * GetMetricStatistics
    , getMetricStatistics
    -- * ListMetrics
    , listMetrics
    -- * PutMetricAlarm
    , putMetricAlarm
    -- * PutMetricData
    , putMetricData
    -- * SetAlarmState
    , setAlarmState

    -- * Re-exported
    , module Network.AWS.CloudWatch.V2010_08_01
    ) where

import Control.Monad.Trans.AWS
import Network.AWS.Prelude
import Network.AWS.CloudWatch.V2010_08_01

-- | Deletes all specified alarms. In the event of an error, no alarms are
-- deleted.
--
-- See: 'Network.AWS.CloudWatch.V2010_08_01.DeleteAlarms'
deleteAlarms :: ( MonadCatch m
                , MonadResource m
                , MonadError Error m
                , MonadReader Env m
                , AWSRequest a
                )
             => [Text] -- ^ 'daAlarmNames'
             -> State DeleteAlarms a
             -> m DeleteAlarmsResponse
deleteAlarms p1 s =
    send $ (mkDeleteAlarms p1) &~ s

-- | Retrieves history for the specified alarm. Filter alarms by date range or
-- item type. If an alarm name is not specified, Amazon CloudWatch returns
-- histories for all of the owner's alarms. Amazon CloudWatch retains the
-- history of an alarm for two weeks, whether or not you delete the alarm.
--
-- See: 'Network.AWS.CloudWatch.V2010_08_01.DescribeAlarmHistory'
describeAlarmHistory :: ( MonadCatch m
                        , MonadResource m
                        , MonadError Error m
                        , MonadReader Env (ResumableSource m)
                        , AWSPager a
                        )
                     => State DescribeAlarmHistory a
                     -> ResumableSource m DescribeAlarmHistoryResponse
describeAlarmHistory s =
    paginate $ (mkDescribeAlarmHistory) &~ s

-- | Retrieves alarms with the specified names. If no name is specified, all
-- alarms for the user are returned. Alarms can be retrieved by using only a
-- prefix for the alarm name, the alarm state, or a prefix for any action.
--
-- See: 'Network.AWS.CloudWatch.V2010_08_01.DescribeAlarms'
describeAlarms :: ( MonadCatch m
                  , MonadResource m
                  , MonadError Error m
                  , MonadReader Env (ResumableSource m)
                  , AWSPager a
                  )
               => State DescribeAlarms a
               -> ResumableSource m DescribeAlarmsResponse
describeAlarms s =
    paginate $ (mkDescribeAlarms) &~ s

-- | Retrieves all alarms for a single metric. Specify a statistic, period, or
-- unit to filter the set of alarms further.
--
-- See: 'Network.AWS.CloudWatch.V2010_08_01.DescribeAlarmsForMetric'
describeAlarmsForMetric :: ( MonadCatch m
                           , MonadResource m
                           , MonadError Error m
                           , MonadReader Env m
                           , AWSRequest a
                           )
                        => Text -- ^ 'dafmMetricName'
                        -> Text -- ^ 'dafmNamespace'
                        -> State DescribeAlarmsForMetric a
                        -> m DescribeAlarmsForMetricResponse
describeAlarmsForMetric p1 p2 s =
    send $ (mkDescribeAlarmsForMetric p1 p2) &~ s

-- | Disables actions for the specified alarms. When an alarm's actions are
-- disabled the alarm's state may change, but none of the alarm's actions will
-- execute.
--
-- See: 'Network.AWS.CloudWatch.V2010_08_01.DisableAlarmActions'
disableAlarmActions :: ( MonadCatch m
                       , MonadResource m
                       , MonadError Error m
                       , MonadReader Env m
                       , AWSRequest a
                       )
                    => [Text] -- ^ 'daaAlarmNames'
                    -> State DisableAlarmActions a
                    -> m DisableAlarmActionsResponse
disableAlarmActions p1 s =
    send $ (mkDisableAlarmActions p1) &~ s

-- | Enables actions for the specified alarms.
--
-- See: 'Network.AWS.CloudWatch.V2010_08_01.EnableAlarmActions'
enableAlarmActions :: ( MonadCatch m
                      , MonadResource m
                      , MonadError Error m
                      , MonadReader Env m
                      , AWSRequest a
                      )
                   => [Text] -- ^ 'eaaAlarmNames'
                   -> State EnableAlarmActions a
                   -> m EnableAlarmActionsResponse
enableAlarmActions p1 s =
    send $ (mkEnableAlarmActions p1) &~ s

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
--
-- See: 'Network.AWS.CloudWatch.V2010_08_01.GetMetricStatistics'
getMetricStatistics :: ( MonadCatch m
                       , MonadResource m
                       , MonadError Error m
                       , MonadReader Env m
                       , AWSRequest a
                       )
                    => Text -- ^ 'gmsNamespace'
                    -> Text -- ^ 'gmsMetricName'
                    -> ISO8601 -- ^ 'gmsStartTime'
                    -> ISO8601 -- ^ 'gmsEndTime'
                    -> Integer -- ^ 'gmsPeriod'
                    -> List1 Statistic -- ^ 'gmsStatistics'
                    -> State GetMetricStatistics a
                    -> m GetMetricStatisticsResponse
getMetricStatistics p1 p2 p4 p5 p6 p7 s =
    send $ (mkGetMetricStatistics p1 p2 p4 p5 p6 p7) &~ s

-- | Returns a list of valid metrics stored for the AWS account owner. Returned
-- metrics can be used with GetMetricStatistics to obtain statistical data for
-- a given metric. Up to 500 results are returned for any one call. To
-- retrieve further results, use returned NextToken values with subsequent
-- ListMetrics operations. If you create a metric with the PutMetricData
-- action, allow up to fifteen minutes for the metric to appear in calls to
-- the ListMetrics action.
--
-- See: 'Network.AWS.CloudWatch.V2010_08_01.ListMetrics'
listMetrics :: ( MonadCatch m
               , MonadResource m
               , MonadError Error m
               , MonadReader Env (ResumableSource m)
               , AWSPager a
               )
            => State ListMetrics a
            -> ResumableSource m ListMetricsResponse
listMetrics s =
    paginate $ (mkListMetrics) &~ s

-- | Creates or updates an alarm and associates it with the specified Amazon
-- CloudWatch metric. Optionally, this operation can associate one or more
-- Amazon Simple Notification Service resources with the alarm. When this
-- operation creates an alarm, the alarm state is immediately set to
-- INSUFFICIENT_DATA. The alarm is evaluated and its StateValue is set
-- appropriately. Any actions associated with the StateValue is then executed.
-- When updating an existing alarm, its StateValue is left unchanged.
--
-- See: 'Network.AWS.CloudWatch.V2010_08_01.PutMetricAlarm'
putMetricAlarm :: ( MonadCatch m
                  , MonadResource m
                  , MonadError Error m
                  , MonadReader Env m
                  , AWSRequest a
                  )
               => Text -- ^ 'pmaAlarmName'
               -> Integer -- ^ 'pmaPeriod'
               -> Integer -- ^ 'pmaEvaluationPeriods'
               -> Double -- ^ 'pmaThreshold'
               -> ComparisonOperator -- ^ 'pmaComparisonOperator'
               -> Text -- ^ 'pmaMetricName'
               -> Text -- ^ 'pmaNamespace'
               -> Statistic -- ^ 'pmaStatistic'
               -> State PutMetricAlarm a
               -> m PutMetricAlarmResponse
putMetricAlarm p1 p11 p13 p14 p15 p7 p8 p9 s =
    send $ (mkPutMetricAlarm p1 p11 p13 p14 p15 p7 p8 p9) &~ s

-- | Publishes metric data points to Amazon CloudWatch. Amazon Cloudwatch
-- associates the data points with the specified metric. If the specified
-- metric does not exist, Amazon CloudWatch creates the metric. If you create
-- a metric with the PutMetricData action, allow up to fifteen minutes for the
-- metric to appear in calls to the ListMetrics action. The size of a
-- PutMetricData request is limited to 8 KB for HTTP GET requests and 40 KB
-- for HTTP POST requests. Although the Value parameter accepts numbers of
-- type Double, Amazon CloudWatch truncates values with very large exponents.
-- Values with base-10 exponents greater than 126 (1 x 10^126) are truncated.
-- Likewise, values with base-10 exponents less than -130 (1 x 10^-130) are
-- also truncated.
--
-- See: 'Network.AWS.CloudWatch.V2010_08_01.PutMetricData'
putMetricData :: ( MonadCatch m
                 , MonadResource m
                 , MonadError Error m
                 , MonadReader Env m
                 , AWSRequest a
                 )
              => Text -- ^ 'pmdNamespace'
              -> [MetricDatum] -- ^ 'pmdMetricData'
              -> State PutMetricData a
              -> m PutMetricDataResponse
putMetricData p1 p2 s =
    send $ (mkPutMetricData p1 p2) &~ s

-- | Temporarily sets the state of an alarm. When the updated StateValue differs
-- from the previous value, the action configured for the appropriate state is
-- invoked. This is not a permanent change. The next periodic alarm check (in
-- about a minute) will set the alarm to its actual state.
--
-- See: 'Network.AWS.CloudWatch.V2010_08_01.SetAlarmState'
setAlarmState :: ( MonadCatch m
                 , MonadResource m
                 , MonadError Error m
                 , MonadReader Env m
                 , AWSRequest a
                 )
              => Text -- ^ 'sasAlarmName'
              -> StateValue -- ^ 'sasStateValue'
              -> Text -- ^ 'sasStateReason'
              -> State SetAlarmState a
              -> m SetAlarmStateResponse
setAlarmState p1 p2 p3 s =
    send $ (mkSetAlarmState p1 p2 p3) &~ s