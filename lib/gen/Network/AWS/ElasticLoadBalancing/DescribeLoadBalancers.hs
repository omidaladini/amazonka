{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticLoadBalancing.DescribeLoadBalancers
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns detailed configuration information for all the load balancers
-- created for the account. If you specify load balancer names, the action
-- returns configuration information of the specified load balancers. In order
-- to retrieve this information, you must provide the same account credentials
-- that was used to create the load balancer. Description of a specified load
-- balancer
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerNames.member.1=MyLoadBalancer
-- &Version=2012-06-01 &Action=DescribeLoadBalancers &AUTHPARAMS
-- MyLoadBalancer 2013-05-24T21:15:31.280Z 90 HTTP:80/ 2 60 10 HTTP 80 HTTP 80
-- i-e4cbe38d us-east-1a ZZZZZZZZZZZ123X
-- MyLoadBalancer-123456789.us-east-1.elb.amazonaws.com internet-facing
-- amazon-elb amazon-elb-sg
-- MyLoadBalancer-123456789.us-east-1.elb.amazonaws.com
-- 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE.
module Network.AWS.ElasticLoadBalancing.DescribeLoadBalancers where

import Network.AWS.Core
import Network.AWS.ElasticLoadBalancing.Service
import Network.AWS.ElasticLoadBalancing.Types

data DescribeLoadBalancers = DescribeLoadBalancers
    { dapiLoadBalancerNames :: [Text]
      -- ^ A list of load balancer names associated with the account.
    , dapiMarker :: Maybe Text
      -- ^ An optional parameter reserved for future use.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeLoadBalancers

instance AWSRequest DescribeLoadBalancers where
    type Er DescribeLoadBalancers = ElasticLoadBalancingError
    type Rs DescribeLoadBalancers = DescribeLoadBalancersResponse
    request = getQuery service "DescribeLoadBalancers"

instance AWSPager DescribeLoadBalancers where
    next rq rs
        | Just x <- dapirsNextMarker rs = Just $ rq { dapiMarker = Just x }
        | otherwise = Nothing

data DescribeLoadBalancersResponse = DescribeLoadBalancersResponse
    { dapirsLoadBalancerDescriptions :: [LoadBalancerDescription]
      -- ^ A list of load balancer description structures.
    , dapirsNextMarker :: Maybe Text
      -- ^ An optional parameter reserved for future use.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeLoadBalancersResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeLoadBalancersResponse"
        :| ["DescribeLoadBalancersResult"]
