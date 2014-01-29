{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.PutScheduledUpdateGroupAction
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates or updates a scheduled scaling action for an Auto Scaling group.
-- When updating a scheduled scaling action, if you leave a parameter
-- unspecified, the corresponding value remains unchanged in the affected Auto
-- Scaling group. For information on creating or updating a scheduled action
-- for your Auto Scaling group, see Scale Based on a Schedule. Auto Scaling
-- supports the date and time expressed in "YYYY-MM-DDThh:mm:ssZ" format in
-- UTC/GMT only. Schedule based on a specific date and time
-- https://autoscaling.amazonaws.com/?AutoScalingGroupName=my-test-asg
-- &ScheduledActionName=ScaleUp &StartTime=2013-05-25T08:00:00Z
-- &DesiredCapacity=3 &Version=2011-01-01
-- &Action=PutScheduledUpdateGroupAction &AUTHPARAMS
-- 3bc8c9bc-6a62-11e2-8a51-4b8a1EXAMPLE Recurring Schedule
-- https://autoscaling.amazonaws.com/?AutoScalingGroupName=my-test-asg
-- &ScheduledActionName=scaleup-schedule-year &Recurrence="30 0 1 1,6,12 *"
-- &DesiredCapacity=3 &Version=2011-01-01
-- &Action=PutScheduledUpdateGroupAction &AUTHPARAMS
-- 3bc8c9bc-6a62-11e2-8a51-4b8a1EXAMPLE.
module Network.AWS.AutoScaling.PutScheduledUpdateGroupAction where

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

import Network.AWS.AutoScaling.Service
import Network.AWS.AutoScaling.Types

-- | Convenience method utilising default fields where applicable.
putScheduledUpdateGroupAction :: ResourceName
                              -> Text
                              -> AWS (Either AutoScalingError PutScheduledUpdateGroupActionResponse)
putScheduledUpdateGroupAction p1 p2 = undefined $ PutScheduledUpdateGroupAction
    { psugatAutoScalingGroupName = p1
    , psugatScheduledActionName = p2
    , psugatDesiredCapacity = Nothing
    , psugatEndTime = Nothing
    , psugatMaxSize = Nothing
    , psugatMinSize = Nothing
    , psugatRecurrence = Nothing
    , psugatStartTime = Nothing
    , psugatTime = Nothing
    }

data PutScheduledUpdateGroupAction = PutScheduledUpdateGroupAction
    { psugatAutoScalingGroupName :: !ResourceName
      -- ^ The name or ARN of the Auto Scaling group.
    , psugatDesiredCapacity :: Maybe Int
      -- ^ The number of Amazon EC2 instances that should be running in the group.
    , psugatEndTime :: Maybe UTCTime
      -- ^ The time for this action to end.
    , psugatMaxSize :: Maybe Int
      -- ^ The maximum size for the Auto Scaling group.
    , psugatMinSize :: Maybe Int
      -- ^ The minimum size for the new Auto Scaling group.
    , psugatRecurrence :: Maybe Text
      -- ^ The time when recurring future actions will start. Start time is specified
      -- by the user following the Unix cron syntax format. For information about
      -- cron syntax, go to Wikipedia, The Free Encyclopedia. When StartTime and
      -- EndTime are specified with Recurrence, they form the boundaries of when the
      -- recurring action will start and stop.
    , psugatScheduledActionName :: !Text
      -- ^ The name of this scaling action.
    , psugatStartTime :: Maybe UTCTime
      -- ^ The time for this action to start, as in --start-time 2010-06-01T00:00:00Z.
      -- If you try to schedule your action in the past, Auto Scaling returns an
      -- error message. When StartTime and EndTime are specified with Recurrence,
      -- they form the boundaries of when the recurring action will start and stop.
    , psugatTime :: Maybe UTCTime
      -- ^ Time is deprecated. The time for this action to start. Time is an alias for
      -- StartTime and can be specified instead of StartTime, or vice versa. If both
      -- Time and StartTime are specified, their values should be identical.
      -- Otherwise, PutScheduledUpdateGroupAction will return an error.
    } deriving (Eq, Show, Generic)

instance ToQuery PutScheduledUpdateGroupAction

instance AWSRequest PutScheduledUpdateGroupAction where
    type Er PutScheduledUpdateGroupAction = AutoScalingError
    type Rs PutScheduledUpdateGroupAction = PutScheduledUpdateGroupActionResponse
    request = getQuery service "PutScheduledUpdateGroupAction"

data PutScheduledUpdateGroupActionResponse = PutScheduledUpdateGroupActionResponse
    deriving (Eq, Show, Generic)

instance FromXML PutScheduledUpdateGroupActionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "PutScheduledUpdateGroupActionResponse"
        :| ["PutScheduledUpdateGroupActionResult"]
