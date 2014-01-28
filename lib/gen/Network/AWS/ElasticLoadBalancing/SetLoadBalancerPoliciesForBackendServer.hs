{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticLoadBalancing.SetLoadBalancerPoliciesForBackendServer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Replaces the current set of policies associated with a port on which the
-- back-end server is listening with a new set of policies. After the policies
-- have been created using CreateLoadBalancerPolicy, they can be applied here
-- as a list. At this time, only the back-end server authentication policy
-- type can be applied to the back-end ports; this policy type is composed of
-- multiple public key policies. The SetLoadBalancerPoliciesForBackendServer
-- replaces the current set of policies associated with the specified instance
-- port. Every time you use this action to enable the policies, use the
-- PolicyNames parameter to list all the policies you want to enable.
-- https://elasticloadbalancing.amazonaws.com/?InstancePort=80
-- &PolicyNames.member.1=EnableProxyProtocol
-- &PolicyNames.member.2=MyPolicyName2 &PolicyNames.member.3=MyPolicyName3
-- &LoadBalancerName=my-test-loadbalancer &Version=2012-06-01
-- &Action=SetLoadBalancerPoliciesForBackendServer &AUTHPARAMS
-- 0eb9b381-dde0-11e2-8d78-6ddbaEXAMPLE You can use DescribeLoadBalancers or
-- DescribeLoadBalancerPolicies action to verify that the policy has been
-- associated with the back-end server.
module Network.AWS.ElasticLoadBalancing.SetLoadBalancerPoliciesForBackendServer where

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

data SetLoadBalancerPoliciesForBackendServer = SetLoadBalancerPoliciesForBackendServer
    { slbpfbsiInstancePort :: !Int
      -- ^ The port number associated with the back-end server.
    , slbpfbsiLoadBalancerName :: !Text
      -- ^ The mnemonic name associated with the load balancer. This name must be
      -- unique within the set of your load balancers.
    , slbpfbsiPolicyNames :: [Text]
      -- ^ List of policy names to be set. If the list is empty, then all current
      -- polices are removed from the back-end server.
    } deriving (Eq, Show, Generic)

instance ToQuery SetLoadBalancerPoliciesForBackendServer

instance AWSRequest SetLoadBalancerPoliciesForBackendServer where
    type Er SetLoadBalancerPoliciesForBackendServer = ElasticLoadBalancingError
    type Rs SetLoadBalancerPoliciesForBackendServer = SetLoadBalancerPoliciesForBackendServerResponse
    request = getQuery service "SetLoadBalancerPoliciesForBackendServer"

data SetLoadBalancerPoliciesForBackendServerResponse = SetLoadBalancerPoliciesForBackendServerResponse
    deriving (Eq, Show, Generic)

instance FromXML SetLoadBalancerPoliciesForBackendServerResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "SetLoadBalancerPoliciesForBackendServerResponse"
        :| ["SetLoadBalancerPoliciesForBackendServerResult"]
