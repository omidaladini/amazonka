{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.CloudWatch.Types where

import Data.ByteString      (ByteString)
import Data.Hashable
import Data.Monoid
import Data.List.NonEmpty
import Data.Text            (Text)
import Data.HashMap.Strict  (HashMap)
import Data.Time
import GHC.Generics         (Generic)
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.CloudWatch.Service

-- | A set of statistical values describing the metric.
data StatisticSet = StatisticSet
    { ssMaximum :: !Double
      -- ^ The maximum value of the sample set.
    , ssMinimum :: !Double
      -- ^ The minimum value of the sample set.
    , ssSampleCount :: !Double
      -- ^ The number of samples used for the statistic set.
    , ssSum :: !Double
      -- ^ The sum of values for the sample set.
    } deriving (Eq, Show, Generic)

instance ToQuery StatisticSet

instance FromXML StatisticSet where
    fromXMLOptions = xmlOptions

instance ToXML StatisticSet where
    toXMLOptions = xmlOptions

-- | The MetricDatum data type encapsulates the information sent with
-- PutMetricData to either create a new metric or add new values to be
-- aggregated into an existing metric.
data MetricDatum = MetricDatum
    { mdDimensions :: [Dimension]
      -- ^ A list of dimensions associated with the metric.
    , mdMetricName :: !Text
      -- ^ The name of the metric.
    , mdStatisticValues :: Maybe StatisticSet
      -- ^ A set of statistical values describing the metric.
    , mdTimestamp :: Maybe UTCTime
      -- ^ The time stamp used for the metric. If not specified, the default value is
      -- set to the time the metric data was received.
    , mdUnit :: Maybe StandardUnit
      -- ^ The unit of the metric.
    , mdValue :: Maybe Double
      -- ^ The value for the metric. Although the Value parameter accepts numbers of
      -- type Double, Amazon CloudWatch truncates values with very large exponents.
      -- Values with base-10 exponents greater than 126 (1 x 10^126) are truncated.
      -- Likewise, values with base-10 exponents less than -130 (1 x 10^-130) are
      -- also truncated.
    } deriving (Eq, Show, Generic)

instance ToQuery MetricDatum

instance FromXML MetricDatum where
    fromXMLOptions = xmlOptions

instance ToXML MetricDatum where
    toXMLOptions = xmlOptions

-- | The MetricAlarm data type represents an alarm. You can use PutMetricAlarm
-- to create or update an alarm.
data MetricAlarm = MetricAlarm
    { maActionsEnabled :: Maybe Bool
      -- ^ Indicates whether actions should be executed during any changes to the
      -- alarm's state.
    , maAlarmActions :: [ResourceName]
      -- ^ The list of actions to execute when this alarm transitions into an ALARM
      -- state from any other state. Each action is specified as an Amazon Resource
      -- Number (ARN). Currently the only actions supported are publishing to an
      -- Amazon SNS topic and triggering an Auto Scaling policy.
    , maAlarmArn :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the alarm.
    , maAlarmConfigurationUpdatedTimestamp :: Maybe UTCTime
      -- ^ The time stamp of the last update to the alarm configuration.
    , maAlarmDescription :: Maybe Text
      -- ^ The description for the alarm.
    , maAlarmName :: Maybe Text
      -- ^ The name of the alarm.
    , maComparisonOperator :: Maybe ComparisonOperator
      -- ^ The arithmetic operation to use when comparing the specified Statistic and
      -- Threshold. The specified Statistic value is used as the first operand.
    , maDimensions :: [Dimension]
      -- ^ The list of dimensions associated with the alarm's associated metric.
    , maEvaluationPeriods :: Maybe Int
      -- ^ The number of periods over which data is compared to the specified
      -- threshold.
    , maInsufficientDataActions :: [ResourceName]
      -- ^ The list of actions to execute when this alarm transitions into an
      -- INSUFFICIENT_DATA state from any other state. Each action is specified as
      -- an Amazon Resource Number (ARN). Currently the only actions supported are
      -- publishing to an Amazon SNS topic or triggering an Auto Scaling policy.
    , maMetricName :: Maybe Text
      -- ^ The name of the alarm's metric.
    , maNamespace :: Maybe Text
      -- ^ The namespace of alarm's associated metric.
    , maOKActions :: [ResourceName]
      -- ^ The list of actions to execute when this alarm transitions into an OK state
      -- from any other state. Each action is specified as an Amazon Resource Number
      -- (ARN). Currently the only actions supported are publishing to an Amazon SNS
      -- topic and triggering an Auto Scaling policy.
    , maPeriod :: Maybe Int
      -- ^ The period in seconds over which the statistic is applied.
    , maStateReason :: Maybe Text
      -- ^ A human-readable explanation for the alarm's state.
    , maStateReasonData :: Maybe Text
      -- ^ An explanation for the alarm's state in machine-readable JSON format.
    , maStateUpdatedTimestamp :: Maybe UTCTime
      -- ^ The time stamp of the last update to the alarm's state.
    , maStateValue :: Maybe StateValue
      -- ^ The state value for the alarm.
    , maStatistic :: Maybe Statistic
      -- ^ The statistic to apply to the alarm's associated metric.
    , maThreshold :: Maybe Double
      -- ^ The value against which the specified statistic is compared.
    , maUnit :: Maybe StandardUnit
      -- ^ The unit of the alarm's associated metric.
    } deriving (Eq, Show, Generic)

instance ToQuery MetricAlarm

instance FromXML MetricAlarm where
    fromXMLOptions = xmlOptions

instance ToXML MetricAlarm where
    toXMLOptions = xmlOptions

-- | The Metric data type contains information about a specific metric. If you
-- call ListMetrics, Amazon CloudWatch returns information contained by this
-- data type.
data Metric = Metric
    { mDimensions :: [Dimension]
      -- ^ A list of dimensions associated with the metric.
    , mMetricName :: Maybe Text
      -- ^ The name of the metric.
    , mNamespace :: Maybe Text
      -- ^ The namespace of the metric.
    } deriving (Eq, Show, Generic)

instance ToQuery Metric

instance FromXML Metric where
    fromXMLOptions = xmlOptions

instance ToXML Metric where
    toXMLOptions = xmlOptions

-- | The DimensionFilter data type is used to filter ListMetrics results.
data DimensionFilter = DimensionFilter
    { dfName :: !Text
      -- ^ The dimension name to be matched.
    , dfValue :: Maybe Text
      -- ^ The value of the dimension to be matched. Specifying a Name without
      -- specifying a Value returns all values associated with that Name.
    } deriving (Eq, Show, Generic)

instance ToQuery DimensionFilter

instance FromXML DimensionFilter where
    fromXMLOptions = xmlOptions

instance ToXML DimensionFilter where
    toXMLOptions = xmlOptions

-- | The Dimension data type further expands on the identity of a metric using a
-- Name, Value pair.
data Dimension = Dimension
    { eName :: !Text
      -- ^ The name of the dimension.
    , eValue :: !Text
      -- ^ The value representing the dimension measurement.
    } deriving (Eq, Show, Generic)

instance ToQuery Dimension

instance FromXML Dimension where
    fromXMLOptions = xmlOptions

instance ToXML Dimension where
    toXMLOptions = xmlOptions

-- | The Datapoint data type encapsulates the statistical data that Amazon
-- CloudWatch computes from metric data.
data Datapoint = Datapoint
    { dAverage :: Maybe Double
      -- ^ The average of metric values that correspond to the datapoint.
    , dMaximum :: Maybe Double
      -- ^ The maximum of the metric value used for the datapoint.
    , dMinimum :: Maybe Double
      -- ^ The minimum metric value used for the datapoint.
    , dSampleCount :: Maybe Double
      -- ^ The number of metric values that contributed to the aggregate value of this
      -- datapoint.
    , dSum :: Maybe Double
      -- ^ The sum of metric values used for the datapoint.
    , dTimestamp :: Maybe UTCTime
      -- ^ The time stamp used for the datapoint.
    , dUnit :: Maybe StandardUnit
      -- ^ The standard unit used for the datapoint.
    } deriving (Eq, Show, Generic)

instance ToQuery Datapoint

instance FromXML Datapoint where
    fromXMLOptions = xmlOptions

instance ToXML Datapoint where
    toXMLOptions = xmlOptions

-- | The AlarmHistoryItem data type contains descriptive information about the
-- history of a specific alarm. If you call DescribeAlarmHistory, Amazon
-- CloudWatch returns this data type as part of the DescribeAlarmHistoryResult
-- data type.
data AlarmHistoryItem = AlarmHistoryItem
    { ahiAlarmName :: Maybe Text
      -- ^ The descriptive name for the alarm.
    , ahiHistoryData :: Maybe Text
      -- ^ Machine-readable data about the alarm in JSON format.
    , ahiHistoryItemType :: Maybe HistoryItemType
      -- ^ The type of alarm history item.
    , ahiHistorySummary :: Maybe Text
      -- ^ A human-readable summary of the alarm history.
    , ahiTimestamp :: Maybe UTCTime
      -- ^ The time stamp for the alarm history item.
    } deriving (Eq, Show, Generic)

instance ToQuery AlarmHistoryItem

instance FromXML AlarmHistoryItem where
    fromXMLOptions = xmlOptions

instance ToXML AlarmHistoryItem where
    toXMLOptions = xmlOptions

-- | The statistic to apply to the alarm's associated metric.
data Statistic
    = StatisticAverage
    | StatisticMaximum
    | StatisticMinimum
    | StatisticSampleCount
    | StatisticSum
      deriving (Eq, Ord, Generic)

instance Hashable Statistic

instance FromText Statistic where
    fromText "Average" = Right StatisticAverage
    fromText "Maximum" = Right StatisticMaximum
    fromText "Minimum" = Right StatisticMinimum
    fromText "SampleCount" = Right StatisticSampleCount
    fromText "Sum" = Right StatisticSum
    fromText e = fromTextFail $ "Unrecognised Statistic: " <> e

instance Read Statistic where
    readsPrec _ = fromTextRead

instance ToText Statistic where
    toText StatisticAverage = "Average"
    toText StatisticMaximum = "Maximum"
    toText StatisticMinimum = "Minimum"
    toText StatisticSampleCount = "SampleCount"
    toText StatisticSum = "Sum"

instance Show Statistic where
    show = toTextShow

instance ToQuery Statistic where
    toQuery = toTextQuery

instance FromXML Statistic where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML Statistic where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The state value to be used in matching alarms.
data StateValue
    = StateValueALARM
    | StateValueINSUFFICIENT_DATA
    | StateValueOK
      deriving (Eq, Ord, Generic)

instance Hashable StateValue

instance FromText StateValue where
    fromText "ALARM" = Right StateValueALARM
    fromText "INSUFFICIENT_DATA" = Right StateValueINSUFFICIENT_DATA
    fromText "OK" = Right StateValueOK
    fromText e = fromTextFail $ "Unrecognised StateValue: " <> e

instance Read StateValue where
    readsPrec _ = fromTextRead

instance ToText StateValue where
    toText StateValueALARM = "ALARM"
    toText StateValueINSUFFICIENT_DATA = "INSUFFICIENT_DATA"
    toText StateValueOK = "OK"

instance Show StateValue where
    show = toTextShow

instance ToQuery StateValue where
    toQuery = toTextQuery

instance FromXML StateValue where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML StateValue where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The unit of the metric.
data StandardUnit
    = StandardUnitBits
    | StandardUnitBits_Second
    | StandardUnitBytes
    | StandardUnitBytes_Second
    | StandardUnitCount
    | StandardUnitCount_Second
    | StandardUnitGigabits
    | StandardUnitGigabits_Second
    | StandardUnitGigabytes
    | StandardUnitGigabytes_Second
    | StandardUnitKilobits
    | StandardUnitKilobits_Second
    | StandardUnitKilobytes
    | StandardUnitKilobytes_Second
    | StandardUnitMegabits
    | StandardUnitMegabits_Second
    | StandardUnitMegabytes
    | StandardUnitMegabytes_Second
    | StandardUnitMicroseconds
    | StandardUnitMilliseconds
    | StandardUnitNone
    | StandardUnitPercent
    | StandardUnitSeconds
    | StandardUnitTerabits
    | StandardUnitTerabits_Second
    | StandardUnitTerabytes
    | StandardUnitTerabytes_Second
      deriving (Eq, Ord, Generic)

instance Hashable StandardUnit

instance FromText StandardUnit where
    fromText "Bits" = Right StandardUnitBits
    fromText "Bits/Second" = Right StandardUnitBits_Second
    fromText "Bytes" = Right StandardUnitBytes
    fromText "Bytes/Second" = Right StandardUnitBytes_Second
    fromText "Count" = Right StandardUnitCount
    fromText "Count/Second" = Right StandardUnitCount_Second
    fromText "Gigabits" = Right StandardUnitGigabits
    fromText "Gigabits/Second" = Right StandardUnitGigabits_Second
    fromText "Gigabytes" = Right StandardUnitGigabytes
    fromText "Gigabytes/Second" = Right StandardUnitGigabytes_Second
    fromText "Kilobits" = Right StandardUnitKilobits
    fromText "Kilobits/Second" = Right StandardUnitKilobits_Second
    fromText "Kilobytes" = Right StandardUnitKilobytes
    fromText "Kilobytes/Second" = Right StandardUnitKilobytes_Second
    fromText "Megabits" = Right StandardUnitMegabits
    fromText "Megabits/Second" = Right StandardUnitMegabits_Second
    fromText "Megabytes" = Right StandardUnitMegabytes
    fromText "Megabytes/Second" = Right StandardUnitMegabytes_Second
    fromText "Microseconds" = Right StandardUnitMicroseconds
    fromText "Milliseconds" = Right StandardUnitMilliseconds
    fromText "None" = Right StandardUnitNone
    fromText "Percent" = Right StandardUnitPercent
    fromText "Seconds" = Right StandardUnitSeconds
    fromText "Terabits" = Right StandardUnitTerabits
    fromText "Terabits/Second" = Right StandardUnitTerabits_Second
    fromText "Terabytes" = Right StandardUnitTerabytes
    fromText "Terabytes/Second" = Right StandardUnitTerabytes_Second
    fromText e = fromTextFail $ "Unrecognised StandardUnit: " <> e

instance Read StandardUnit where
    readsPrec _ = fromTextRead

instance ToText StandardUnit where
    toText StandardUnitBits = "Bits"
    toText StandardUnitBits_Second = "Bits/Second"
    toText StandardUnitBytes = "Bytes"
    toText StandardUnitBytes_Second = "Bytes/Second"
    toText StandardUnitCount = "Count"
    toText StandardUnitCount_Second = "Count/Second"
    toText StandardUnitGigabits = "Gigabits"
    toText StandardUnitGigabits_Second = "Gigabits/Second"
    toText StandardUnitGigabytes = "Gigabytes"
    toText StandardUnitGigabytes_Second = "Gigabytes/Second"
    toText StandardUnitKilobits = "Kilobits"
    toText StandardUnitKilobits_Second = "Kilobits/Second"
    toText StandardUnitKilobytes = "Kilobytes"
    toText StandardUnitKilobytes_Second = "Kilobytes/Second"
    toText StandardUnitMegabits = "Megabits"
    toText StandardUnitMegabits_Second = "Megabits/Second"
    toText StandardUnitMegabytes = "Megabytes"
    toText StandardUnitMegabytes_Second = "Megabytes/Second"
    toText StandardUnitMicroseconds = "Microseconds"
    toText StandardUnitMilliseconds = "Milliseconds"
    toText StandardUnitNone = "None"
    toText StandardUnitPercent = "Percent"
    toText StandardUnitSeconds = "Seconds"
    toText StandardUnitTerabits = "Terabits"
    toText StandardUnitTerabits_Second = "Terabits/Second"
    toText StandardUnitTerabytes = "Terabytes"
    toText StandardUnitTerabytes_Second = "Terabytes/Second"

instance Show StandardUnit where
    show = toTextShow

instance ToQuery StandardUnit where
    toQuery = toTextQuery

instance FromXML StandardUnit where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML StandardUnit where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The type of alarm histories to retrieve.
data HistoryItemType
    = HistoryItemTypeAction
    | HistoryItemTypeConfigurationUpdate
    | HistoryItemTypeStateUpdate
      deriving (Eq, Ord, Generic)

instance Hashable HistoryItemType

instance FromText HistoryItemType where
    fromText "Action" = Right HistoryItemTypeAction
    fromText "ConfigurationUpdate" = Right HistoryItemTypeConfigurationUpdate
    fromText "StateUpdate" = Right HistoryItemTypeStateUpdate
    fromText e = fromTextFail $ "Unrecognised HistoryItemType: " <> e

instance Read HistoryItemType where
    readsPrec _ = fromTextRead

instance ToText HistoryItemType where
    toText HistoryItemTypeAction = "Action"
    toText HistoryItemTypeConfigurationUpdate = "ConfigurationUpdate"
    toText HistoryItemTypeStateUpdate = "StateUpdate"

instance Show HistoryItemType where
    show = toTextShow

instance ToQuery HistoryItemType where
    toQuery = toTextQuery

instance FromXML HistoryItemType where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML HistoryItemType where
    toXMLOptions = xmlOptions
    toXML        = toTextXML

-- | The arithmetic operation to use when comparing the specified Statistic and
-- Threshold. The specified Statistic value is used as the first operand.
data ComparisonOperator
    = ComparisonOperatorGreaterThanOrEqualToThreshold
    | ComparisonOperatorGreaterThanThreshold
    | ComparisonOperatorLessThanOrEqualToThreshold
    | ComparisonOperatorLessThanThreshold
      deriving (Eq, Ord, Generic)

instance Hashable ComparisonOperator

instance FromText ComparisonOperator where
    fromText "GreaterThanOrEqualToThreshold" = Right ComparisonOperatorGreaterThanOrEqualToThreshold
    fromText "GreaterThanThreshold" = Right ComparisonOperatorGreaterThanThreshold
    fromText "LessThanOrEqualToThreshold" = Right ComparisonOperatorLessThanOrEqualToThreshold
    fromText "LessThanThreshold" = Right ComparisonOperatorLessThanThreshold
    fromText e = fromTextFail $ "Unrecognised ComparisonOperator: " <> e

instance Read ComparisonOperator where
    readsPrec _ = fromTextRead

instance ToText ComparisonOperator where
    toText ComparisonOperatorGreaterThanOrEqualToThreshold = "GreaterThanOrEqualToThreshold"
    toText ComparisonOperatorGreaterThanThreshold = "GreaterThanThreshold"
    toText ComparisonOperatorLessThanOrEqualToThreshold = "LessThanOrEqualToThreshold"
    toText ComparisonOperatorLessThanThreshold = "LessThanThreshold"

instance Show ComparisonOperator where
    show = toTextShow

instance ToQuery ComparisonOperator where
    toQuery = toTextQuery

instance FromXML ComparisonOperator where
    fromXMLOptions = xmlOptions
    fromXML        = fromTextXML

instance ToXML ComparisonOperator where
    toXMLOptions = xmlOptions
    toXML        = toTextXML
