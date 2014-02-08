{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DescribeAutoScalingInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a description of each Auto Scaling instance in the InstanceIds
-- list. If a list is not provided, the service returns the full details of
-- all instances up to a maximum of 50. By default, the service returns a list
-- of 20 items. This action supports pagination by returning a token if there
-- are more pages to retrieve. To get the next page, call this action again
-- with the returned token as the NextToken parameter.
-- https://autoscaling.amazonaws.com/?MaxRecords=20
-- &InstanceIds.member.1=i-78e0d40b &Version=2011-01-01
-- &Action=DescribeAutoScalingInstances &AUTHPARAMS HEALTHY my-test-asg
-- us-east-1e i-78e0d40b my-test-lc InService
-- df992dc3-b72f-11e2-81e1-750aa6EXAMPLE.
module Network.AWS.AutoScaling.DescribeAutoScalingInstances where

import Network.AWS.Core
import Network.AWS.AutoScaling.Service
import Network.AWS.AutoScaling.Types

data DescribeAutoScalingInstances = DescribeAutoScalingInstances
    { dasitInstanceIds :: [Text]
      -- ^ The list of Auto Scaling instances to describe. If this list is omitted,
      -- all auto scaling instances are described. The list of requested instances
      -- cannot contain more than 50 items. If unknown instances are requested, they
      -- are ignored with no error.
    , dasitMaxRecords :: Maybe Int
      -- ^ The maximum number of Auto Scaling instances to be described with each
      -- call.
    , dasitNextToken :: Maybe Text
      -- ^ The token returned by a previous call to indicate that there is more data
      -- available.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeAutoScalingInstances

instance AWSRequest DescribeAutoScalingInstances where
    type Er DescribeAutoScalingInstances = AutoScalingError
    type Rs DescribeAutoScalingInstances = DescribeAutoScalingInstancesResponse
    request = getQuery service "DescribeAutoScalingInstances"

instance AWSPager DescribeAutoScalingInstances where
    next rq rs
        | Just x <- dasitrsNextToken rs = Just $ rq { dasitNextToken = Just x }
        | otherwise = Nothing

data DescribeAutoScalingInstancesResponse = DescribeAutoScalingInstancesResponse
    { dasitrsAutoScalingInstances :: [AutoScalingInstanceDetails]
      -- ^ A list of Auto Scaling instances.
    , dasitrsNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned results.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeAutoScalingInstancesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeAutoScalingInstancesResponse"
        :| ["DescribeAutoScalingInstancesResult"]
