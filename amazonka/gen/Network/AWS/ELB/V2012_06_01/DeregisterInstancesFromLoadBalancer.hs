{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ELB.V2012_06_01.DeregisterInstancesFromLoadBalancer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deregisters instances from the load balancer. Once the instance is
-- deregistered, it will stop receiving traffic from the load balancer. In
-- order to successfully call this API, the same account credentials as those
-- used to create the load balancer must be provided. For more information,
-- see De-register and Register Amazon EC2 Instances in the Elastic Load
-- Balancing Developer Guide. You can use DescribeLoadBalancers to verify if
-- the instance is deregistered from the load balancer. Deregister instance
-- i-e3677ad7 from MyHTTPSLoadBalancer load balancer.
-- https://elasticloadbalancing.amazonaws.com/?Instances.member.1.InstanceId=i-e3677ad7
-- &LoadBalancerName=MyHTTPSLoadBalancer &Version=2012-06-01
-- &Action=DeregisterInstancesFromLoadBalancer &AUTHPARAMS i-6ec63d59
-- 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE.
module Network.AWS.ELB.V2012_06_01.DeregisterInstancesFromLoadBalancer where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.ELB.V2012_06_01.Types
import Network.AWS.Prelude

data DeregisterInstancesFromLoadBalancer = DeregisterInstancesFromLoadBalancer
    { _depiLoadBalancerName :: Text
      -- ^ The name associated with the load balancer.
    , _depiInstances :: [Instance]
      -- ^ A list of EC2 instance IDs consisting of all instances to be
      -- deregistered.
    } deriving (Generic)

makeLenses ''DeregisterInstancesFromLoadBalancer

instance ToQuery DeregisterInstancesFromLoadBalancer where
    toQuery = genericToQuery def

data DeregisterInstancesFromLoadBalancerResponse = DeregisterInstancesFromLoadBalancerResponse
    { _depoInstances :: [Instance]
      -- ^ An updated list of remaining instances registered with the load
      -- balancer.
    } deriving (Generic)

makeLenses ''DeregisterInstancesFromLoadBalancerResponse

instance FromXML DeregisterInstancesFromLoadBalancerResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DeregisterInstancesFromLoadBalancer where
    type Sv DeregisterInstancesFromLoadBalancer = ELB
    type Rs DeregisterInstancesFromLoadBalancer = DeregisterInstancesFromLoadBalancerResponse

    request = post "DeregisterInstancesFromLoadBalancer"
    response _ = xmlResponse