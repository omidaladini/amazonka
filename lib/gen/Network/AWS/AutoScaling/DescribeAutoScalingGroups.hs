{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DescribeAutoScalingGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a full description of each Auto Scaling group in the given list.
-- This includes all Amazon EC2 instances that are members of the group. If a
-- list of names is not provided, the service returns the full details of all
-- Auto Scaling groups. This action supports pagination by returning a token
-- if there are more pages to retrieve. To get the next page, call this action
-- again with the returned token as the NextToken parameter.
-- https://autoscaling.amazonaws.com/?AutoScalingGroupNames.member.1=my-test-asg-lbs
-- &MaxRecords=20 &Version=2011-01-01 &Action=DescribeAutoScalingGroups
-- &AUTHPARAMS my-test-asg-lbs ELB 2013-05-06T17:47:15.107Z my-test-lc 2
-- us-east-1b us-east-1a my-test-asg-loadbalancer 2 120 300
-- arn:aws:autoscaling:us-east-1:803981987763:autoScalingGroup:ca861182-c8f9-4ca7-b1eb-cd35505f5ebb
-- :autoScalingGroupName/my-test-asg-lbs Default 10
-- 0f02a07d-b677-11e2-9eb0-dd50EXAMPLE.
module Network.AWS.AutoScaling.DescribeAutoScalingGroups where

import Network.AWS.Core
import Network.AWS.AutoScaling.Service
import Network.AWS.AutoScaling.Types

data DescribeAutoScalingGroups = DescribeAutoScalingGroups
    { asgntAutoScalingGroupNames :: [ResourceName]
      -- ^ A list of Auto Scaling group names.
    , asgntMaxRecords :: Maybe Int
      -- ^ The maximum number of records to return.
    , asgntNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned results.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeAutoScalingGroups

instance AWSRequest DescribeAutoScalingGroups where
    type Er DescribeAutoScalingGroups = AutoScalingError
    type Rs DescribeAutoScalingGroups = DescribeAutoScalingGroupsResponse
    request = getQuery service "DescribeAutoScalingGroups"

instance AWSPager DescribeAutoScalingGroups where
    next rq rs
        | Just x <- asgntrNextToken rs = Just $ rq { asgntNextToken = Just x }
        | otherwise = Nothing

data DescribeAutoScalingGroupsResponse = DescribeAutoScalingGroupsResponse
    { asgntrAutoScalingGroups :: [AutoScalingGroup]
      -- ^ A list of Auto Scaling groups.
    , asgntrNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned results.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeAutoScalingGroupsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeAutoScalingGroupsResponse"
        :| ["DescribeAutoScalingGroupsResult"]
