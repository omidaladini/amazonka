{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DescribeScheduledActions
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
module Network.AWS.AutoScaling.DescribeScheduledActions where

import Network.AWS.Core
import Network.AWS.AutoScaling.Service
import Network.AWS.AutoScaling.Types

data DescribeScheduledActions = DescribeScheduledActions
    { dsatdAutoScalingGroupName :: Maybe ResourceName
      -- ^ The name of the Auto Scaling group.
    , dsatdEndTime :: Maybe UTCTime
      -- ^ The latest scheduled start time to return. If scheduled action names are
      -- provided, this field is ignored.
    , dsatdMaxRecords :: Maybe Int
      -- ^ The maximum number of scheduled actions to return.
    , dsatdNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned results.
    , dsatdScheduledActionNames :: [ResourceName]
      -- ^ A list of scheduled actions to be described. If this list is omitted, all
      -- scheduled actions are described. The list of requested scheduled actions
      -- cannot contain more than 50 items. If an auto scaling group name is
      -- provided, the results are limited to that group. If unknown scheduled
      -- actions are requested, they are ignored with no error.
    , dsatdStartTime :: Maybe UTCTime
      -- ^ The earliest scheduled start time to return. If scheduled action names are
      -- provided, this field will be ignored.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeScheduledActions

instance AWSRequest DescribeScheduledActions where
    type Er DescribeScheduledActions = AutoScalingError
    type Rs DescribeScheduledActions = DescribeScheduledActionsResponse
    request  = postQuery service "DescribeScheduledActions"
    response = responseXML

instance AWSPager DescribeScheduledActions where
    next rq rs
        | Just x <- dsatdrNextToken rs = Just $ rq { dsatdNextToken = Just x }
        | otherwise = Nothing

data DescribeScheduledActionsResponse = DescribeScheduledActionsResponse
    { dsatdrNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned results.
    , dsatdrScheduledUpdateGroupActions :: [ScheduledUpdateGroupAction]
      -- ^ A list of scheduled actions designed to update an Auto Scaling group.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeScheduledActionsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeScheduledActionsResponse"
        :| ["DescribeScheduledActionsResult"]