{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.DescribeScheduledActions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists all the actions scheduled for your Auto Scaling group that haven't
-- been executed. To see a list of actions already executed, see the activity
-- record returned in DescribeScalingActivities.
module Network.AWS.AutoScaling.V2011_01_01.DescribeScheduledActions
    (
    -- * Request
      DescribeScheduledActions
    -- ** Request constructor
    , describeScheduledActions
    -- ** Request lenses
    , dsavMaxRecords
    , dsavAutoScalingGroupName
    , dsavScheduledActionNames
    , dsavStartTime
    , dsavEndTime
    , dsavNextToken

    -- * Response
    , DescribeScheduledActionsResponse
    -- ** Response lenses
    , satScheduledUpdateGroupActions
    , satNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeScheduledActions' request.
describeScheduledActions :: DescribeScheduledActions
describeScheduledActions = DescribeScheduledActions
    { _dsavMaxRecords = Nothing
    , _dsavAutoScalingGroupName = Nothing
    , _dsavScheduledActionNames = mempty
    , _dsavStartTime = Nothing
    , _dsavEndTime = Nothing
    , _dsavNextToken = Nothing
    }

data DescribeScheduledActions = DescribeScheduledActions
    { _dsavMaxRecords :: Maybe Integer
      -- ^ The maximum number of scheduled actions to return.
    , _dsavAutoScalingGroupName :: Maybe Text
      -- ^ The name of the Auto Scaling group.
    , _dsavScheduledActionNames :: [Text]
      -- ^ A list of scheduled actions to be described. If this list is
      -- omitted, all scheduled actions are described. The list of
      -- requested scheduled actions cannot contain more than 50 items. If
      -- an auto scaling group name is provided, the results are limited
      -- to that group. If unknown scheduled actions are requested, they
      -- are ignored with no error.
    , _dsavStartTime :: Maybe ISO8601
      -- ^ The earliest scheduled start time to return. If scheduled action
      -- names are provided, this field will be ignored.
    , _dsavEndTime :: Maybe ISO8601
      -- ^ The latest scheduled start time to return. If scheduled action
      -- names are provided, this field is ignored.
    , _dsavNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned
      -- results.
    } deriving (Show, Generic)

-- | The maximum number of scheduled actions to return.
dsavMaxRecords
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DescribeScheduledActions
    -> f DescribeScheduledActions
dsavMaxRecords f x =
    (\y -> x { _dsavMaxRecords = y })
       <$> f (_dsavMaxRecords x)
{-# INLINE dsavMaxRecords #-}

-- | The name of the Auto Scaling group.
dsavAutoScalingGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeScheduledActions
    -> f DescribeScheduledActions
dsavAutoScalingGroupName f x =
    (\y -> x { _dsavAutoScalingGroupName = y })
       <$> f (_dsavAutoScalingGroupName x)
{-# INLINE dsavAutoScalingGroupName #-}

-- | A list of scheduled actions to be described. If this list is omitted, all
-- scheduled actions are described. The list of requested scheduled actions
-- cannot contain more than 50 items. If an auto scaling group name is
-- provided, the results are limited to that group. If unknown scheduled
-- actions are requested, they are ignored with no error.
dsavScheduledActionNames
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> DescribeScheduledActions
    -> f DescribeScheduledActions
dsavScheduledActionNames f x =
    (\y -> x { _dsavScheduledActionNames = y })
       <$> f (_dsavScheduledActionNames x)
{-# INLINE dsavScheduledActionNames #-}

-- | The earliest scheduled start time to return. If scheduled action names are
-- provided, this field will be ignored.
dsavStartTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> DescribeScheduledActions
    -> f DescribeScheduledActions
dsavStartTime f x =
    (\y -> x { _dsavStartTime = y })
       <$> f (_dsavStartTime x)
{-# INLINE dsavStartTime #-}

-- | The latest scheduled start time to return. If scheduled action names are
-- provided, this field is ignored.
dsavEndTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> DescribeScheduledActions
    -> f DescribeScheduledActions
dsavEndTime f x =
    (\y -> x { _dsavEndTime = y })
       <$> f (_dsavEndTime x)
{-# INLINE dsavEndTime #-}

-- | A string that marks the start of the next batch of returned results.
dsavNextToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeScheduledActions
    -> f DescribeScheduledActions
dsavNextToken f x =
    (\y -> x { _dsavNextToken = y })
       <$> f (_dsavNextToken x)
{-# INLINE dsavNextToken #-}

instance ToQuery DescribeScheduledActions where
    toQuery = genericQuery def

data DescribeScheduledActionsResponse = DescribeScheduledActionsResponse
    { _satScheduledUpdateGroupActions :: [ScheduledUpdateGroupAction]
      -- ^ A list of scheduled actions designed to update an Auto Scaling
      -- group.
    , _satNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned
      -- results.
    } deriving (Show, Generic)

-- | A list of scheduled actions designed to update an Auto Scaling group.
satScheduledUpdateGroupActions
    :: Functor f
    => ([ScheduledUpdateGroupAction]
    -> f ([ScheduledUpdateGroupAction]))
    -> DescribeScheduledActionsResponse
    -> f DescribeScheduledActionsResponse
satScheduledUpdateGroupActions f x =
    (\y -> x { _satScheduledUpdateGroupActions = y })
       <$> f (_satScheduledUpdateGroupActions x)
{-# INLINE satScheduledUpdateGroupActions #-}

-- | A string that marks the start of the next batch of returned results.
satNextToken
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeScheduledActionsResponse
    -> f DescribeScheduledActionsResponse
satNextToken f x =
    (\y -> x { _satNextToken = y })
       <$> f (_satNextToken x)
{-# INLINE satNextToken #-}

instance FromXML DescribeScheduledActionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeScheduledActions where
    type Sv DescribeScheduledActions = AutoScaling
    type Rs DescribeScheduledActions = DescribeScheduledActionsResponse

    request = post "DescribeScheduledActions"
    response _ = xmlResponse

instance AWSPager DescribeScheduledActions where
    next rq rs = (\x -> rq { _dsavNextToken = Just x })
        <$> (_satNextToken rs)
