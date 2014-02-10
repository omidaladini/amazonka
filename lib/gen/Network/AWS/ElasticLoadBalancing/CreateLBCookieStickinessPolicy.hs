{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticLoadBalancing.CreateLBCookieStickinessPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Generates a stickiness policy with sticky session lifetimes controlled by
-- the lifetime of the browser (user-agent) or a specified expiration period.
-- This policy can be associated only with HTTP/HTTPS listeners. When a load
-- balancer implements this policy, the load balancer uses a special cookie to
-- track the backend server instance for each request. When the load balancer
-- receives a request, it first checks to see if this cookie is present in the
-- request. If so, the load balancer sends the request to the application
-- server specified in the cookie. If not, the load balancer sends the request
-- to a server that is chosen based on the existing load balancing algorithm.
-- A cookie is inserted into the response for binding subsequent requests from
-- the same user to that server. The validity of the cookie is based on the
-- cookie expiration time, which is specified in the policy configuration. For
-- more information, see Enabling Duration-Based Session Stickiness in the
-- Elastic Load Balancing Developer Guide.
-- https://elasticloadbalancing.amazonaws.com/?CookieExpirationPeriod=60
-- &LoadBalancerName=MyLoadBalancer&PolicyName=MyDurationStickyPolicy
-- &Version=2012-06-01 &Action=CreateLBCookieStickinessPolicy &AUTHPARAMS
-- 99a693e9-12b8-11e3-9ad6-bf3e4EXAMPLE.
module Network.AWS.ElasticLoadBalancing.CreateLBCookieStickinessPolicy where

import Network.AWS.Core
import Network.AWS.ElasticLoadBalancing.Service
import Network.AWS.ElasticLoadBalancing.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
createLBCookieStickinessPolicy :: Text
                               -- ^ The name associated with the load balancer.
                               -> Text
                               -- ^ The name of the policy being created. The name must be unique within the
                               -- set of policies for this load balancer.
                               -> CreateLBCookieStickinessPolicy
createLBCookieStickinessPolicy p1 p2 = CreateLBCookieStickinessPolicy
    { clbcspiLoadBalancerName = p1
    , clbcspiPolicyName = p2
    , clbcspiCookieExpirationPeriod = Nothing
    }

data CreateLBCookieStickinessPolicy = CreateLBCookieStickinessPolicy
    { clbcspiCookieExpirationPeriod :: Maybe Integer
      -- ^ The time period in seconds after which the cookie should be considered
      -- stale. Not specifying this parameter indicates that the sticky session will
      -- last for the duration of the browser session.
    , clbcspiLoadBalancerName :: !Text
      -- ^ The name associated with the load balancer.
    , clbcspiPolicyName :: !Text
      -- ^ The name of the policy being created. The name must be unique within the
      -- set of policies for this load balancer.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateLBCookieStickinessPolicy

instance AWSRequest CreateLBCookieStickinessPolicy where
    type Er CreateLBCookieStickinessPolicy = ElasticLoadBalancingError
    type Rs CreateLBCookieStickinessPolicy = CreateLBCookieStickinessPolicyResponse
    request  = postQuery service "CreateLBCookieStickinessPolicy"
    response = responseXML

data CreateLBCookieStickinessPolicyResponse = CreateLBCookieStickinessPolicyResponse
    deriving (Eq, Show, Generic)

instance FromXML CreateLBCookieStickinessPolicyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateLBCookieStickinessPolicyResponse"
