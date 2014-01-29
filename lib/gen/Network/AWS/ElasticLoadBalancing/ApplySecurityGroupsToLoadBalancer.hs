{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticLoadBalancing.ApplySecurityGroupsToLoadBalancer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Associates one or more security groups with your load balancer in Amazon
-- Virtual Private Cloud (Amazon VPC). The provided security group IDs will
-- override any currently applied security groups. For more information, see
-- Manage Security Groups in Amazon VPC in the Elastic Load Balancing
-- Developer Guide.
-- https://elasticloadbalancing.amazonaws.com/?SecurityGroups.member.1=sg-123456789
-- &LoadBalancerName=my-test-vpc-loadbalancer &Version=2012-06-01
-- &Action=ApplySecurityGroupsToLoadBalancer &AUTHPARAMS sg-123456789
-- 06b5decc-102a-11e3-9ad6-bf3e4EXAMPLE.
module Network.AWS.ElasticLoadBalancing.ApplySecurityGroupsToLoadBalancer where

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
applySecurityGroupsToLoadBalancer :: Text
                                  -> [Text]
                                  -> ApplySecurityGroupsToLoadBalancer
applySecurityGroupsToLoadBalancer p1 p2 = undefined $ ApplySecurityGroupsToLoadBalancer
    { asgtlbiLoadBalancerName = p1
    , asgtlbiSecurityGroups = p2
    }

data ApplySecurityGroupsToLoadBalancer = ApplySecurityGroupsToLoadBalancer
    { asgtlbiLoadBalancerName :: !Text
      -- ^ The name associated with the load balancer. The name must be unique within
      -- the set of load balancers associated with your AWS account.
    , asgtlbiSecurityGroups :: [Text]
      -- ^ A list of security group IDs to associate with your load balancer in VPC.
      -- The security group IDs must be provided as the ID and not the security
      -- group name (For example, sg-1234).
    } deriving (Eq, Show, Generic)

instance ToQuery ApplySecurityGroupsToLoadBalancer

instance AWSRequest ApplySecurityGroupsToLoadBalancer where
    type Er ApplySecurityGroupsToLoadBalancer = ElasticLoadBalancingError
    type Rs ApplySecurityGroupsToLoadBalancer = ApplySecurityGroupsToLoadBalancerResponse
    request = getQuery service "ApplySecurityGroupsToLoadBalancer"

data ApplySecurityGroupsToLoadBalancerResponse = ApplySecurityGroupsToLoadBalancerResponse
    { asgtlbirsSecurityGroups :: [Text]
      -- ^ A list of security group IDs associated with your load balancer.
    } deriving (Eq, Show, Generic)

instance FromXML ApplySecurityGroupsToLoadBalancerResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ApplySecurityGroupsToLoadBalancerResponse"
        :| ["ApplySecurityGroupsToLoadBalancerResult"]
