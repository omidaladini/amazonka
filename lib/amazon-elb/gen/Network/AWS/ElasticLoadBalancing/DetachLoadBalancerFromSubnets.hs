{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticLoadBalancing.DetachLoadBalancerFromSubnets
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes subnets from the set of configured subnets in the Amazon Virtual
-- Private Cloud (Amazon VPC) for the load balancer. After a subnet is removed
-- all of the EC2 instances registered with the load balancer that are in the
-- removed subnet will go into the OutOfService state. When a subnet is
-- removed, the load balancer will balance the traffic among the remaining
-- routable subnets for the load balancer.
-- https://elasticloadbalancing.amazonaws.com/?Subnets.member.1=subnet-119f0078
-- &LoadBalancerName=my-test-vpc-loadbalancer &Version=2012-06-01
-- &Action=DetachLoadBalancerFromSubnets &AUTHPARAMS subnet-159f007c
-- subnet-3561b05e 07b1ecbc-1100-11e3-acaf-dd7edEXAMPLE.
module Network.AWS.ElasticLoadBalancing.DetachLoadBalancerFromSubnets where

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
detachLoadBalancerFromSubnets :: Text
                              -> [Text]
                              -> DetachLoadBalancerFromSubnets
detachLoadBalancerFromSubnets p1 p2 = DetachLoadBalancerFromSubnets
    { dlbfsiLoadBalancerName = p1
    , dlbfsiSubnets = p2
    }

data DetachLoadBalancerFromSubnets = DetachLoadBalancerFromSubnets
    { dlbfsiLoadBalancerName :: !Text
      -- ^ The name associated with the load balancer to be detached.
    , dlbfsiSubnets :: [Text]
      -- ^ A list of subnet IDs to remove from the set of configured subnets for the
      -- load balancer.
    } deriving (Eq, Show, Generic)

instance ToQuery DetachLoadBalancerFromSubnets

instance AWSRequest DetachLoadBalancerFromSubnets where
    type Er DetachLoadBalancerFromSubnets = ElasticLoadBalancingError
    type Rs DetachLoadBalancerFromSubnets = DetachLoadBalancerFromSubnetsResponse
    request = getQuery service "DetachLoadBalancerFromSubnets"

data DetachLoadBalancerFromSubnetsResponse = DetachLoadBalancerFromSubnetsResponse
    { dlbfsirsSubnets :: [Text]
      -- ^ A list of subnet IDs the load balancer is now attached to.
    } deriving (Eq, Show, Generic)

instance FromXML DetachLoadBalancerFromSubnetsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DetachLoadBalancerFromSubnetsResponse"
        :| ["DetachLoadBalancerFromSubnetsResult"]
