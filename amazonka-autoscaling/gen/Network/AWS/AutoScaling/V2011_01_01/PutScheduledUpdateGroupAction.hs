{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.PutScheduledUpdateGroupAction
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
module Network.AWS.AutoScaling.V2011_01_01.PutScheduledUpdateGroupAction
    (
    -- * Request
      PutScheduledUpdateGroupAction
    -- ** Request constructor
    , putScheduledUpdateGroupAction
    -- ** Request lenses
    , psugatAutoScalingGroupName
    , psugatScheduledActionName
    , psugatDesiredCapacity
    , psugatMaxSize
    , psugatMinSize
    , psugatTime
    , psugatStartTime
    , psugatEndTime
    , psugatRecurrence

    -- * Response
    , PutScheduledUpdateGroupActionResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'PutScheduledUpdateGroupAction' request.
putScheduledUpdateGroupAction :: Text -- ^ 'psugatAutoScalingGroupName'
                              -> Text -- ^ 'psugatScheduledActionName'
                              -> PutScheduledUpdateGroupAction
putScheduledUpdateGroupAction p1 p2 = PutScheduledUpdateGroupAction
    { _psugatAutoScalingGroupName = p1
    , _psugatScheduledActionName = p2
    , _psugatDesiredCapacity = Nothing
    , _psugatMaxSize = Nothing
    , _psugatMinSize = Nothing
    , _psugatTime = Nothing
    , _psugatStartTime = Nothing
    , _psugatEndTime = Nothing
    , _psugatRecurrence = Nothing
    }

data PutScheduledUpdateGroupAction = PutScheduledUpdateGroupAction
    { _psugatAutoScalingGroupName :: Text
      -- ^ The name or ARN of the Auto Scaling group.
    , _psugatScheduledActionName :: Text
      -- ^ The name of this scaling action.
    , _psugatDesiredCapacity :: Maybe Integer
      -- ^ The number of Amazon EC2 instances that should be running in the
      -- group.
    , _psugatMaxSize :: Maybe Integer
      -- ^ The maximum size for the Auto Scaling group.
    , _psugatMinSize :: Maybe Integer
      -- ^ The minimum size for the new Auto Scaling group.
    , _psugatTime :: Maybe ISO8601
      -- ^ Time is deprecated. The time for this action to start. Time is an
      -- alias for StartTime and can be specified instead of StartTime, or
      -- vice versa. If both Time and StartTime are specified, their
      -- values should be identical. Otherwise,
      -- PutScheduledUpdateGroupAction will return an error.
    , _psugatStartTime :: Maybe ISO8601
      -- ^ The time for this action to start, as in --start-time
      -- 2010-06-01T00:00:00Z. If you try to schedule your action in the
      -- past, Auto Scaling returns an error message. When StartTime and
      -- EndTime are specified with Recurrence, they form the boundaries
      -- of when the recurring action will start and stop.
    , _psugatEndTime :: Maybe ISO8601
      -- ^ The time for this action to end.
    , _psugatRecurrence :: Maybe Text
      -- ^ The time when recurring future actions will start. Start time is
      -- specified by the user following the Unix cron syntax format. For
      -- information about cron syntax, go to Wikipedia, The Free
      -- Encyclopedia. When StartTime and EndTime are specified with
      -- Recurrence, they form the boundaries of when the recurring action
      -- will start and stop.
    } deriving (Show, Generic)

-- | The name or ARN of the Auto Scaling group.
psugatAutoScalingGroupName
    :: Functor f
    => (Text
    -> f (Text))
    -> PutScheduledUpdateGroupAction
    -> f PutScheduledUpdateGroupAction
psugatAutoScalingGroupName f x =
    (\y -> x { _psugatAutoScalingGroupName = y })
       <$> f (_psugatAutoScalingGroupName x)
{-# INLINE psugatAutoScalingGroupName #-}

-- | The name of this scaling action.
psugatScheduledActionName
    :: Functor f
    => (Text
    -> f (Text))
    -> PutScheduledUpdateGroupAction
    -> f PutScheduledUpdateGroupAction
psugatScheduledActionName f x =
    (\y -> x { _psugatScheduledActionName = y })
       <$> f (_psugatScheduledActionName x)
{-# INLINE psugatScheduledActionName #-}

-- | The number of Amazon EC2 instances that should be running in the group.
psugatDesiredCapacity
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> PutScheduledUpdateGroupAction
    -> f PutScheduledUpdateGroupAction
psugatDesiredCapacity f x =
    (\y -> x { _psugatDesiredCapacity = y })
       <$> f (_psugatDesiredCapacity x)
{-# INLINE psugatDesiredCapacity #-}

-- | The maximum size for the Auto Scaling group.
psugatMaxSize
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> PutScheduledUpdateGroupAction
    -> f PutScheduledUpdateGroupAction
psugatMaxSize f x =
    (\y -> x { _psugatMaxSize = y })
       <$> f (_psugatMaxSize x)
{-# INLINE psugatMaxSize #-}

-- | The minimum size for the new Auto Scaling group.
psugatMinSize
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> PutScheduledUpdateGroupAction
    -> f PutScheduledUpdateGroupAction
psugatMinSize f x =
    (\y -> x { _psugatMinSize = y })
       <$> f (_psugatMinSize x)
{-# INLINE psugatMinSize #-}

-- | Time is deprecated. The time for this action to start. Time is an alias for
-- StartTime and can be specified instead of StartTime, or vice versa. If both
-- Time and StartTime are specified, their values should be identical.
-- Otherwise, PutScheduledUpdateGroupAction will return an error.
psugatTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> PutScheduledUpdateGroupAction
    -> f PutScheduledUpdateGroupAction
psugatTime f x =
    (\y -> x { _psugatTime = y })
       <$> f (_psugatTime x)
{-# INLINE psugatTime #-}

-- | The time for this action to start, as in --start-time 2010-06-01T00:00:00Z.
-- If you try to schedule your action in the past, Auto Scaling returns an
-- error message. When StartTime and EndTime are specified with Recurrence,
-- they form the boundaries of when the recurring action will start and stop.
psugatStartTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> PutScheduledUpdateGroupAction
    -> f PutScheduledUpdateGroupAction
psugatStartTime f x =
    (\y -> x { _psugatStartTime = y })
       <$> f (_psugatStartTime x)
{-# INLINE psugatStartTime #-}

-- | The time for this action to end.
psugatEndTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> PutScheduledUpdateGroupAction
    -> f PutScheduledUpdateGroupAction
psugatEndTime f x =
    (\y -> x { _psugatEndTime = y })
       <$> f (_psugatEndTime x)
{-# INLINE psugatEndTime #-}

-- | The time when recurring future actions will start. Start time is specified
-- by the user following the Unix cron syntax format. For information about
-- cron syntax, go to Wikipedia, The Free Encyclopedia. When StartTime and
-- EndTime are specified with Recurrence, they form the boundaries of when the
-- recurring action will start and stop.
psugatRecurrence
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PutScheduledUpdateGroupAction
    -> f PutScheduledUpdateGroupAction
psugatRecurrence f x =
    (\y -> x { _psugatRecurrence = y })
       <$> f (_psugatRecurrence x)
{-# INLINE psugatRecurrence #-}

instance ToQuery PutScheduledUpdateGroupAction where
    toQuery = genericQuery def

data PutScheduledUpdateGroupActionResponse = PutScheduledUpdateGroupActionResponse
    deriving (Eq, Show, Generic)

instance AWSRequest PutScheduledUpdateGroupAction where
    type Sv PutScheduledUpdateGroupAction = AutoScaling
    type Rs PutScheduledUpdateGroupAction = PutScheduledUpdateGroupActionResponse

    request = post "PutScheduledUpdateGroupAction"
    response _ = nullaryResponse PutScheduledUpdateGroupActionResponse
