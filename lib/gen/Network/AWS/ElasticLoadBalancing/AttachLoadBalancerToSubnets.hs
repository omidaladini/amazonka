{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticLoadBalancing.AttachLoadBalancerToSubnets
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds one or more subnets to the set of configured subnets in the Amazon
-- Virtual Private Cloud (Amazon VPC) for the load balancer. The load
-- balancers evenly distribute requests across all of the registered subnets.
-- For more information, see Deploy Elastic Load Balancing in Amazon VPC in
-- the Elastic Load Balancing Developer Guide.
-- https://elasticloadbalancing.amazonaws.com/?Subnets.member.1=subnet-3561b05e
-- &LoadBalancerName=my-test-vpc-loadbalancer &Version=2012-06-01
-- &Action=AttachLoadBalancerToSubnets &AUTHPARAMS subnet-119f0078
-- subnet-3561b05e 07b1ecbc-1100-11e3-acaf-dd7edEXAMPLE.
module Network.AWS.ElasticLoadBalancing.AttachLoadBalancerToSubnets where

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
attachLoadBalancerToSubnets :: Text
                            -> [Text]
                            -> AttachLoadBalancerToSubnets
attachLoadBalancerToSubnets p1 p2 = undefined $ AttachLoadBalancerToSubnets
    { albtsiLoadBalancerName = p1
    , albtsiSubnets = p2
    }

data AttachLoadBalancerToSubnets = AttachLoadBalancerToSubnets
    { albtsiLoadBalancerName :: !Text
      -- ^ The name associated with the load balancer. The name must be unique within
      -- the set of load balancers associated with your AWS account.
    , albtsiSubnets :: [Text]
      -- ^ A list of subnet IDs to add for the load balancer. You can add only one
      -- subnet per Availability Zone.
    } deriving (Eq, Show, Generic)

instance ToQuery AttachLoadBalancerToSubnets

instance AWSRequest AttachLoadBalancerToSubnets where
    type Er AttachLoadBalancerToSubnets = ElasticLoadBalancingError
    type Rs AttachLoadBalancerToSubnets = AttachLoadBalancerToSubnetsResponse
    request = getQuery service "AttachLoadBalancerToSubnets"

data AttachLoadBalancerToSubnetsResponse = AttachLoadBalancerToSubnetsResponse
    { albtsirsSubnets :: [Text]
      -- ^ A list of subnet IDs attached to the load balancer.
    } deriving (Eq, Show, Generic)

instance FromXML AttachLoadBalancerToSubnetsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "AttachLoadBalancerToSubnetsResponse"
        :| ["AttachLoadBalancerToSubnetsResult"]
