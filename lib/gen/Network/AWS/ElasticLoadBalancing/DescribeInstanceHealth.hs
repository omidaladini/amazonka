{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticLoadBalancing.DescribeInstanceHealth
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the current state of the specified instances registered with the
-- specified load balancer. If no instances are specified, the state of all
-- the instances registered with the load balancer is returned. You must
-- provide the same account credentials as those that were used to create the
-- load balancer. Description of a healthy (InService) instance
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=MyLoadBalancer
-- &Version=2012-06-01 &Action=DescribeInstanceHealth &AUTHPARAMS N/A
-- i-90d8c2a5 InService N/A 1549581b-12b7-11e3-895e-1334aEXAMPLE Description
-- of an instance with registration in progress
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=my-test-loadbalancer
-- &Version=2012-06-01 &Action=DescribeInstanceHealth &AUTHPARAMS Instance
-- registration is still in progress. i-315b7e51 OutOfService ELB
-- 1549581b-12b7-11e3-895e-1334aEXAMPLE Description of an unhealthy
-- (OutOfService) instance
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=MyLoadBalancer-2
-- &Version=2012-06-01 &Action=DescribeInstanceHealth &AUTHPARAMS Instance has
-- failed at least the UnhealthyThreshold number of health checks
-- consecutively. i-fda142c9 OutOfService Instance
-- 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE.
module Network.AWS.ElasticLoadBalancing.DescribeInstanceHealth where

import Network.AWS.Core
import Network.AWS.ElasticLoadBalancing.Service
import Network.AWS.ElasticLoadBalancing.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
describeInstanceHealth :: Text
                       -- ^ The name of the load balancer.
                       -> DescribeInstanceHealth
describeInstanceHealth p1 = DescribeInstanceHealth
    { depsiLoadBalancerName = p1
    , depsiInstances = []
    }

data DescribeInstanceHealth = DescribeInstanceHealth
    { depsiInstances :: [Instance]
      -- ^ A list of instance IDs whose states are being queried.
    , depsiLoadBalancerName :: !Text
      -- ^ The name of the load balancer.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeInstanceHealth

instance AWSRequest DescribeInstanceHealth where
    type Er DescribeInstanceHealth = ElasticLoadBalancingError
    type Rs DescribeInstanceHealth = DescribeInstanceHealthResponse
    request = getQuery service "DescribeInstanceHealth"

data DescribeInstanceHealthResponse = DescribeInstanceHealthResponse
    { depsirsInstanceStates :: [InstanceState]
      -- ^ A list containing health information for the specified instances.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeInstanceHealthResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeInstanceHealthResponse"
        :| ["DescribeInstanceHealthResult"]
