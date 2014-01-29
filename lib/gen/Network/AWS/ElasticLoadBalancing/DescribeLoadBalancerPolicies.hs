{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticLoadBalancing.DescribeLoadBalancerPolicies
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns detailed descriptions of the policies. If you specify a load
-- balancer name, the action returns the descriptions of all the policies
-- created for the load balancer. If you specify a policy name associated with
-- your load balancer, the action returns the description of that policy. If
-- you don't specify a load balancer name, the action returns descriptions of
-- the specified sample policies, or descriptions of all the sample policies.
-- The names of the sample policies have the ELBSample- prefix. Description of
-- all the policies associated with a load balancer
-- https://elasticloadbalancing.amazonaws.com/?LoadBalancerName=MyLoadBalancer
-- &Version=2012-06-01 &Action=DescribeLoadBalancerPolicies &AUTHPARAMS
-- MyDurationStickyPolicy LBCookieStickinessPolicyType CookieExpirationPeriod
-- 60 MyAppStickyPolicy AppCookieStickinessPolicyType CookieName MyAppCookie
-- 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE Description of a specified policy
-- associated with the load balancer
-- https://elasticloadbalancing.amazonaws.com/?PolicyNames.member.1=EnableProxyProtocol
-- &LoadBalancerName=my-test-loadbalancer &Version=2012-06-01
-- &Action=DescribeLoadBalancerPolicies &AUTHPARAMS EnableProxyProtocol
-- ProxyProtocolPolicyType ProxyProtocol true
-- 1549581b-12b7-11e3-895e-1334aEXAMPLE.
module Network.AWS.ElasticLoadBalancing.DescribeLoadBalancerPolicies where

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

import Network.AWS.ElasticLoadBalancing.Service
import Network.AWS.ElasticLoadBalancing.Types

-- | Convenience method utilising default fields where applicable.
describeLoadBalancerPolicies :: AWS (Either ElasticLoadBalancingError DescribeLoadBalancerPoliciesResponse)
describeLoadBalancerPolicies = undefined $ DescribeLoadBalancerPolicies
    { dlbpiLoadBalancerName = Nothing
    , dlbpiPolicyNames = []
    }

data DescribeLoadBalancerPolicies = DescribeLoadBalancerPolicies
    { dlbpiLoadBalancerName :: Maybe Text
      -- ^ The mnemonic name associated with the load balancer. If no name is
      -- specified, the operation returns the attributes of either all the sample
      -- policies pre-defined by Elastic Load Balancing or the specified sample
      -- polices.
    , dlbpiPolicyNames :: [Text]
      -- ^ The names of load balancer policies you've created or Elastic Load
      -- Balancing sample policy names.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeLoadBalancerPolicies

instance AWSRequest DescribeLoadBalancerPolicies where
    type Er DescribeLoadBalancerPolicies = ElasticLoadBalancingError
    type Rs DescribeLoadBalancerPolicies = DescribeLoadBalancerPoliciesResponse
    request = getQuery service "DescribeLoadBalancerPolicies"

data DescribeLoadBalancerPoliciesResponse = DescribeLoadBalancerPoliciesResponse
    { dlbpirsPolicyDescriptions :: [PolicyDescription]
      -- ^ A list of policy description structures.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeLoadBalancerPoliciesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeLoadBalancerPoliciesResponse"
        :| ["DescribeLoadBalancerPoliciesResult"]
