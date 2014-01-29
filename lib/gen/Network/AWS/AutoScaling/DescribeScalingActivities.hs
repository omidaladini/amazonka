{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DescribeScalingActivities
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the scaling activities for the specified Auto Scaling group. If the
-- specified ActivityIds list is empty, all the activities from the past six
-- weeks are returned. Activities are sorted by the start time. Activities
-- still in progress appear first on the list. This action supports
-- pagination. If the response includes a token, there are more records
-- available. To get the additional records, repeat the request with the
-- response token as the NextToken parameter.
-- https://autoscaling.amazonaws.com/?AutoScalingGroupName=my-test-asg
-- &MaxRecords=20 &Version=2011-01-01 &Action=DescribeScalingActivities
-- &AUTHPARAMS Failed 0 063308ae-aa22-4a9b-94f4-9faeEXAMPLE
-- 2012-04-12T17:32:07.882Z my-test-asg At 2012-04-12T17:31:30Z a user request
-- created an AutoScalingGroup changing the desired capacity from 0 to 1. At
-- 2012-04-12T17:32:07Z an instance was started in response to a difference
-- between desired and actual capacity, increasing the capacity from 0 to 1.
-- {} Launching a new EC2 instance. Status Reason: The image id 'ami-4edb0327'
-- does not exist. Launching EC2 instance failed. 2012-04-12T17:32:08Z The
-- image id 'ami-4edb0327' does not exist. Launching EC2 instance failed.
-- 7a641adc-84c5-11e1-a8a5-217ebEXAMPLE.
module Network.AWS.AutoScaling.DescribeScalingActivities where

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
describeScalingActivities :: AWS (Either AutoScalingError DescribeScalingActivitiesResponse)
describeScalingActivities = undefined $ DescribeScalingActivities
    { dsavActivityIds = []
    , dsavAutoScalingGroupName = Nothing
    , dsavMaxRecords = Nothing
    , dsavNextToken = Nothing
    }

data DescribeScalingActivities = DescribeScalingActivities
    { dsavActivityIds :: [Text]
      -- ^ A list containing the activity IDs of the desired scaling activities. If
      -- this list is omitted, all activities are described. If an
      -- AutoScalingGroupName is provided, the results are limited to that group.
      -- The list of requested activities cannot contain more than 50 items. If
      -- unknown activities are requested, they are ignored with no error.
    , dsavAutoScalingGroupName :: Maybe ResourceName
      -- ^ The name of the AutoScalingGroup.
    , dsavMaxRecords :: Maybe Int
      -- ^ The maximum number of scaling activities to return.
    , dsavNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned results for
      -- pagination.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeScalingActivities

instance AWSRequest DescribeScalingActivities where
    type Er DescribeScalingActivities = AutoScalingError
    type Rs DescribeScalingActivities = DescribeScalingActivitiesResponse
    request = getQuery service "DescribeScalingActivities"

instance AWSPager DescribeScalingActivities where
    next rq rs
        | Just x <- dsavrsNextToken rs = Just $ rq { dsavNextToken = Just x }
        | otherwise = Nothing

data DescribeScalingActivitiesResponse = DescribeScalingActivitiesResponse
    { dsavrsActivities :: [Activity]
      -- ^ A list of the requested scaling activities.
    , dsavrsNextToken :: Maybe Text
      -- ^ Acts as a paging mechanism for large result sets. Set to a non-empty string
      -- if there are additional results waiting to be returned. Pass this in to
      -- subsequent calls to return additional results.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeScalingActivitiesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeScalingActivitiesResponse"
        :| ["DescribeScalingActivitiesResult"]
