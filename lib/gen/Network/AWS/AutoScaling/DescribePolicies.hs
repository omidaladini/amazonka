{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DescribePolicies
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns descriptions of what each policy does. This action supports
-- pagination. If the response includes a token, there are more records
-- available. To get the additional records, repeat the request with the
-- response token as the NextToken parameter.
-- https://autoscaling.amazonaws.com/?AutoScalingGroupName=my-test-asg
-- &MaxRecords=20 &Version=2011-01-01 &Action=DescribePolicies &AUTHPARAMS
-- arn:aws:autoscaling:us-east-1:803981987763:scalingPolicy:c322
-- 761b-3172-4d56-9a21-0ed9d6161d67:autoScalingGroupName/my-test-asg:policyName/MyScaleDownPolicy
-- ChangeInCapacity -1 MyScaleDownPolicy my-test-asg 60 TestQueue
-- arn:aws:cloudwatch:us-east-1:803981987763:alarm:TestQueue
-- arn:aws:autoscaling:us-east-1:803981987763:scalingPolicy:c55a5cdd-9be0-435b-b60b-a8dd313159f5:autoScalingGroupName/my-test-asg:policyName/MyScaleUpPolicy
-- ChangeInCapacity 1 MyScaleUpPolicy my-test-asg 60 TestQueue
-- arn:aws:cloudwatch:us-east-1:803981987763:alarm:TestQueue
-- ec3bffad-b739-11e2-b38d-15fbEXAMPLE.
module Network.AWS.AutoScaling.DescribePolicies where

import Network.AWS.Core
import Network.AWS.AutoScaling.Service
import Network.AWS.AutoScaling.Types

data DescribePolicies = DescribePolicies
    { dpuAutoScalingGroupName :: Maybe ResourceName
      -- ^ The name of the Auto Scaling group.
    , dpuMaxRecords :: Maybe Int
      -- ^ The maximum number of policies that will be described with each call.
    , dpuNextToken :: Maybe Text
      -- ^ A string that is used to mark the start of the next batch of returned
      -- results for pagination.
    , dpuPolicyNames :: [ResourceName]
      -- ^ A list of policy names or policy ARNs to be described. If this list is
      -- omitted, all policy names are described. If an auto scaling group name is
      -- provided, the results are limited to that group. The list of requested
      -- policy names cannot contain more than 50 items. If unknown policy names are
      -- requested, they are ignored with no error.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribePolicies

instance AWSRequest DescribePolicies where
    type Er DescribePolicies = AutoScalingError
    type Rs DescribePolicies = DescribePoliciesResponse
    request = getQuery service "DescribePolicies"

instance AWSPager DescribePolicies where
    next rq rs
        | Just x <- dpursNextToken rs = Just $ rq { dpuNextToken = Just x }
        | otherwise = Nothing

data DescribePoliciesResponse = DescribePoliciesResponse
    { dpursNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned results.
    , dpursScalingPolicies :: [ScalingPolicy]
      -- ^ A list of scaling policies.
    } deriving (Eq, Show, Generic)

instance FromXML DescribePoliciesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribePoliciesResponse"
        :| ["DescribePoliciesResult"]
