{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticLoadBalancing.CreateLoadBalancerPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new policy that contains the necessary attributes depending on
-- the policy type. Policies are settings that are saved for your load
-- balancer and that can be applied to the front-end listener, or the back-end
-- application server, depending on your policy type.
-- https://elasticloadbalancing.amazonaws.com/?PolicyAttributes.member.1.AttributeName=ProxyProtocol
-- &PolicyAttributes.member.1.AttributeValue=true
-- &PolicyTypeName=ProxyProtocolPolicyType
-- &LoadBalancerName=my-test-loadbalancer &PolicyName=EnableProxyProtocol
-- &Version=2012-06-01 &Action=CreateLoadBalancerPolicy &AUTHPARAMS
-- 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE.
module Network.AWS.ElasticLoadBalancing.CreateLoadBalancerPolicy where

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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
createLoadBalancerPolicy :: Text
                         -> Text
                         -> Text
                         -> CreateLoadBalancerPolicy
createLoadBalancerPolicy p1 p2 p3 = CreateLoadBalancerPolicy
    { clbpiLoadBalancerName = p1
    , clbpiPolicyName = p2
    , clbpiPolicyTypeName = p3
    , clbpiPolicyAttributes = []
    }

data CreateLoadBalancerPolicy = CreateLoadBalancerPolicy
    { clbpiLoadBalancerName :: !Text
      -- ^ The name associated with the LoadBalancer for which the policy is being
      -- created.
    , clbpiPolicyAttributes :: [PolicyAttribute]
      -- ^ A list of attributes associated with the policy being created.
    , clbpiPolicyName :: !Text
      -- ^ The name of the load balancer policy being created. The name must be unique
      -- within the set of policies for this load balancer.
    , clbpiPolicyTypeName :: !Text
      -- ^ The name of the base policy type being used to create this policy. To get
      -- the list of policy types, use the DescribeLoadBalancerPolicyTypes action.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateLoadBalancerPolicy

instance AWSRequest CreateLoadBalancerPolicy where
    type Er CreateLoadBalancerPolicy = ElasticLoadBalancingError
    type Rs CreateLoadBalancerPolicy = CreateLoadBalancerPolicyResponse
    request = getQuery service "CreateLoadBalancerPolicy"

data CreateLoadBalancerPolicyResponse = CreateLoadBalancerPolicyResponse
    deriving (Eq, Show, Generic)

instance FromXML CreateLoadBalancerPolicyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot CreateLoadBalancerPolicyResponse
