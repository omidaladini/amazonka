{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.V2012_06_01.CreateAppCookieStickinessPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Generates a stickiness policy with sticky session lifetimes that follow
-- that of an application-generated cookie. This policy can be associated only
-- with HTTP/HTTPS listeners. This policy is similar to the policy created by
-- CreateLBCookieStickinessPolicy, except that the lifetime of the special
-- Elastic Load Balancing cookie follows the lifetime of the
-- application-generated cookie specified in the policy configuration. The
-- load balancer only inserts a new stickiness cookie when the application
-- response includes a new application cookie. If the application cookie is
-- explicitly removed or expires, the session stops being sticky until a new
-- application cookie is issued. An application client must receive and send
-- two cookies: the application-generated cookie and the special Elastic Load
-- Balancing cookie named AWSELB. This is the default behavior for many common
-- web browsers. For more information, see Enabling Application-Controlled
-- Session Stickiness in the Elastic Load Balancing Developer Guide.
-- https://elasticloadbalancing.amazonaws.com/?CookieName=MyAppCookie
-- &LoadBalancerName=MyLoadBalancer &PolicyName=MyAppStickyPolicy
-- &Version=2012-06-01 &Action=CreateAppCookieStickinessPolicy &AUTHPARAMS
-- 99a693e9-12b8-11e3-9ad6-bf3e4EXAMPLE.
module Network.AWS.ELB.V2012_06_01.CreateAppCookieStickinessPolicy
    (
    -- * Request
      CreateAppCookieStickinessPolicy
    -- ** Request constructor
    , createAppCookieStickinessPolicy
    -- ** Request lenses
    , cacspiLoadBalancerName
    , cacspiCookieName
    , cacspiPolicyName

    -- * Response
    , CreateAppCookieStickinessPolicyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateAppCookieStickinessPolicy' request.
createAppCookieStickinessPolicy :: Text -- ^ 'cacspiLoadBalancerName'
                                -> Text -- ^ 'cacspiCookieName'
                                -> Text -- ^ 'cacspiPolicyName'
                                -> CreateAppCookieStickinessPolicy
createAppCookieStickinessPolicy p1 p2 p3 = CreateAppCookieStickinessPolicy
    { _cacspiLoadBalancerName = p1
    , _cacspiCookieName = p2
    , _cacspiPolicyName = p3
    }

data CreateAppCookieStickinessPolicy = CreateAppCookieStickinessPolicy
    { _cacspiLoadBalancerName :: Text
      -- ^ The name of the load balancer.
    , _cacspiCookieName :: Text
      -- ^ Name of the application cookie used for stickiness.
    , _cacspiPolicyName :: Text
      -- ^ The name of the policy being created. The name must be unique
      -- within the set of policies for this load balancer.
    } deriving (Show, Generic)

-- | The name of the load balancer.
cacspiLoadBalancerName
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateAppCookieStickinessPolicy
    -> f CreateAppCookieStickinessPolicy
cacspiLoadBalancerName f x =
    (\y -> x { _cacspiLoadBalancerName = y })
       <$> f (_cacspiLoadBalancerName x)
{-# INLINE cacspiLoadBalancerName #-}

-- | Name of the application cookie used for stickiness.
cacspiCookieName
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateAppCookieStickinessPolicy
    -> f CreateAppCookieStickinessPolicy
cacspiCookieName f x =
    (\y -> x { _cacspiCookieName = y })
       <$> f (_cacspiCookieName x)
{-# INLINE cacspiCookieName #-}

-- | The name of the policy being created. The name must be unique within the
-- set of policies for this load balancer.
cacspiPolicyName
    :: Functor f
    => (Text
    -> f (Text))
    -> CreateAppCookieStickinessPolicy
    -> f CreateAppCookieStickinessPolicy
cacspiPolicyName f x =
    (\y -> x { _cacspiPolicyName = y })
       <$> f (_cacspiPolicyName x)
{-# INLINE cacspiPolicyName #-}

instance ToQuery CreateAppCookieStickinessPolicy where
    toQuery = genericQuery def

data CreateAppCookieStickinessPolicyResponse = CreateAppCookieStickinessPolicyResponse
    deriving (Eq, Show, Generic)

instance AWSRequest CreateAppCookieStickinessPolicy where
    type Sv CreateAppCookieStickinessPolicy = ELB
    type Rs CreateAppCookieStickinessPolicy = CreateAppCookieStickinessPolicyResponse

    request = post "CreateAppCookieStickinessPolicy"
    response _ = nullaryResponse CreateAppCookieStickinessPolicyResponse
