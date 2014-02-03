{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch.PutMetricAlarm
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates or updates an alarm and associates it with the specified Amazon
-- CloudWatch metric. Optionally, this operation can associate one or more
-- Amazon Simple Notification Service resources with the alarm. When this
-- operation creates an alarm, the alarm state is immediately set to
-- INSUFFICIENT_DATA. The alarm is evaluated and its StateValue is set
-- appropriately. Any actions associated with the StateValue is then executed.
-- When updating an existing alarm, its StateValue is left unchanged.
module Network.AWS.CloudWatch.PutMetricAlarm where

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
putMetricAlarm :: Text
               -> ComparisonOperator
               -> Int
               -> Text
               -> Text
               -> Int
               -> Statistic
               -> Double
               -> PutMetricAlarm
putMetricAlarm p1 p2 p3 p4 p5 p6 p7 p8 = PutMetricAlarm
    { pmaiAlarmName = p1
    , pmaiComparisonOperator = p2
    , pmaiEvaluationPeriods = p3
    , pmaiMetricName = p4
    , pmaiNamespace = p5
    , pmaiPeriod = p6
    , pmaiStatistic = p7
    , pmaiThreshold = p8
    , pmaiActionsEnabled = Nothing
    , pmaiAlarmActions = []
    , pmaiAlarmDescription = Nothing
    , pmaiDimensions = []
    , pmaiInsufficientDataActions = []
    , pmaiOKActions = []
    , pmaiUnit = Nothing
    }

data PutMetricAlarm = PutMetricAlarm
    { pmaiActionsEnabled :: Maybe Bool
      -- ^ Indicates whether or not actions should be executed during any changes to
      -- the alarm's state.
    , pmaiAlarmActions :: [ResourceName]
      -- ^ The list of actions to execute when this alarm transitions into an ALARM
      -- state from any other state. Each action is specified as an Amazon Resource
      -- Number (ARN). Currently the only action supported is publishing to an
      -- Amazon SNS topic or an Amazon Auto Scaling policy.
    , pmaiAlarmDescription :: Maybe Text
      -- ^ The description for the alarm.
    , pmaiAlarmName :: !Text
      -- ^ The descriptive name for the alarm. This name must be unique within the
      -- user's AWS account.
    , pmaiComparisonOperator :: !ComparisonOperator
      -- ^ The arithmetic operation to use when comparing the specified Statistic and
      -- Threshold. The specified Statistic value is used as the first operand.
    , pmaiDimensions :: [Dimension]
      -- ^ The dimensions for the alarm's associated metric.
    , pmaiEvaluationPeriods :: !Int
      -- ^ The number of periods over which data is compared to the specified
      -- threshold.
    , pmaiInsufficientDataActions :: [ResourceName]
      -- ^ The list of actions to execute when this alarm transitions into an
      -- INSUFFICIENT_DATA state from any other state. Each action is specified as
      -- an Amazon Resource Number (ARN). Currently the only action supported is
      -- publishing to an Amazon SNS topic or an Amazon Auto Scaling policy.
    , pmaiMetricName :: !Text
      -- ^ The name for the alarm's associated metric.
    , pmaiNamespace :: !Text
      -- ^ The namespace for the alarm's associated metric.
    , pmaiOKActions :: [ResourceName]
      -- ^ The list of actions to execute when this alarm transitions into an OK state
      -- from any other state. Each action is specified as an Amazon Resource Number
      -- (ARN). Currently the only action supported is publishing to an Amazon SNS
      -- topic or an Amazon Auto Scaling policy.
    , pmaiPeriod :: !Int
      -- ^ The period in seconds over which the specified statistic is applied.
    , pmaiStatistic :: !Statistic
      -- ^ The statistic to apply to the alarm's associated metric.
    , pmaiThreshold :: !Double
      -- ^ The value against which the specified statistic is compared.
    , pmaiUnit :: Maybe StandardUnit
      -- ^ The unit for the alarm's associated metric.
    } deriving (Eq, Show, Generic)

instance ToQuery PutMetricAlarm

instance AWSRequest PutMetricAlarm where
    type Er PutMetricAlarm = CloudWatchError
    type Rs PutMetricAlarm = PutMetricAlarmResponse
    request = getQuery service "PutMetricAlarm"

data PutMetricAlarmResponse = PutMetricAlarmResponse
    deriving (Eq, Show, Generic)

instance FromXML PutMetricAlarmResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot PutMetricAlarmResponse
