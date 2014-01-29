{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticLoadBalancing.SetLoadBalancerPoliciesOfListener
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Associates, updates, or disables a policy with a listener on the load
-- balancer. You can associate multiple policies with a listener. Associate
-- MySSLNegotiationPolicy with the load balancer port 443 on the
-- MyInternalLoadbalancer load balancer.
-- https://elasticloadbalancing.amazonaws.com/?PolicyNames.member.1=MySSLNegotiationPolicy
-- &LoadBalancerName=MyInternalLoadBalancer &LoadBalancerPort=443
-- &Version=2012-06-01 &Action=SetLoadBalancerPoliciesOfListener &AUTHPARAMS
-- azonaws.com/doc/2012-06-01/"> 07b1ecbc-1100-11e3-acaf-dd7edEXAMPLE.
module Network.AWS.ElasticLoadBalancing.SetLoadBalancerPoliciesOfListener where

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
setLoadBalancerPoliciesOfListener :: Text
                                  -> Int
                                  -> [Text]
                                  -> SetLoadBalancerPoliciesOfListener
setLoadBalancerPoliciesOfListener p1 p2 p3 = undefined $ SetLoadBalancerPoliciesOfListener
    { slbpoliLoadBalancerName = p1
    , slbpoliLoadBalancerPort = p2
    , slbpoliPolicyNames = p3
    }

data SetLoadBalancerPoliciesOfListener = SetLoadBalancerPoliciesOfListener
    { slbpoliLoadBalancerName :: !Text
      -- ^ The name of the load balancer.
    , slbpoliLoadBalancerPort :: !Int
      -- ^ The external port of the load balancer to associate the policy.
    , slbpoliPolicyNames :: [Text]
      -- ^ List of policies to be associated with the listener. If the list is empty,
      -- the current policy is removed from the listener.
    } deriving (Eq, Show, Generic)

instance ToQuery SetLoadBalancerPoliciesOfListener

instance AWSRequest SetLoadBalancerPoliciesOfListener where
    type Er SetLoadBalancerPoliciesOfListener = ElasticLoadBalancingError
    type Rs SetLoadBalancerPoliciesOfListener = SetLoadBalancerPoliciesOfListenerResponse
    request = getQuery service "SetLoadBalancerPoliciesOfListener"

data SetLoadBalancerPoliciesOfListenerResponse = SetLoadBalancerPoliciesOfListenerResponse
    deriving (Eq, Show, Generic)

instance FromXML SetLoadBalancerPoliciesOfListenerResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "SetLoadBalancerPoliciesOfListenerResponse"
        :| ["SetLoadBalancerPoliciesOfListenerResult"]
