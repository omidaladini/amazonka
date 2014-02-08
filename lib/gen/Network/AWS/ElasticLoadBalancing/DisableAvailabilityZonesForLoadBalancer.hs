{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticLoadBalancing.DisableAvailabilityZonesForLoadBalancer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes the specified EC2 Availability Zones from the set of configured
-- Availability Zones for the load balancer. There must be at least one
-- Availability Zone registered with a load balancer at all times. Once an
-- Availability Zone is removed, all the instances registered with the load
-- balancer that are in the removed Availability Zone go into the OutOfService
-- state. Upon Availability Zone removal, the load balancer attempts to
-- equally balance the traffic among its remaining usable Availability Zones.
-- Trying to remove an Availability Zone that was not associated with the load
-- balancer does nothing. For more information, see Disable an Availability
-- Zone from a Load-Balanced Application in the Elastic Load Balancing
-- Developer Guide.
-- https://elasticloadbalancing.amazonaws.com/?AvailabilityZones.member.1=us-east-1a
-- &LoadBalancerName=MyHTTPSLoadBalancer &Version=2012-06-01
-- &Action=DisableAvailabilityZonesForLoadBalancer &AUTHPARAMS us-east-1b
-- ba6267d5-2566-11e3-9c6d-eb728EXAMPLE.
module Network.AWS.ElasticLoadBalancing.DisableAvailabilityZonesForLoadBalancer where

import Network.AWS.Core
import Network.AWS.ElasticLoadBalancing.Service
import Network.AWS.ElasticLoadBalancing.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
disableAvailabilityZonesForLoadBalancer :: [Text]
                                        -- ^ A list of Availability Zones to be removed from the load balancer. There
                                        -- must be at least one Availability Zone registered with a load balancer at
                                        -- all times. Specified Availability Zones must be in the same region.
                                        -> Text
                                        -- ^ The name associated with the load balancer.
                                        -> DisableAvailabilityZonesForLoadBalancer
disableAvailabilityZonesForLoadBalancer p1 p2 = DisableAvailabilityZonesForLoadBalancer
    { raziAvailabilityZones = p1
    , raziLoadBalancerName = p2
    }

data DisableAvailabilityZonesForLoadBalancer = DisableAvailabilityZonesForLoadBalancer
    { raziAvailabilityZones :: [Text]
      -- ^ A list of Availability Zones to be removed from the load balancer. There
      -- must be at least one Availability Zone registered with a load balancer at
      -- all times. Specified Availability Zones must be in the same region.
    , raziLoadBalancerName :: !Text
      -- ^ The name associated with the load balancer.
    } deriving (Eq, Show, Generic)

instance ToQuery DisableAvailabilityZonesForLoadBalancer

instance AWSRequest DisableAvailabilityZonesForLoadBalancer where
    type Er DisableAvailabilityZonesForLoadBalancer = ElasticLoadBalancingError
    type Rs DisableAvailabilityZonesForLoadBalancer = DisableAvailabilityZonesForLoadBalancerResponse
    request = getQuery service "DisableAvailabilityZonesForLoadBalancer"

data DisableAvailabilityZonesForLoadBalancerResponse = DisableAvailabilityZonesForLoadBalancerResponse
    { razirsAvailabilityZones :: [Text]
      -- ^ A list of updated Availability Zones for the load balancer.
    } deriving (Eq, Show, Generic)

instance FromXML DisableAvailabilityZonesForLoadBalancerResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DisableAvailabilityZonesForLoadBalancerResponse"
        :| ["DisableAvailabilityZonesForLoadBalancerResult"]
