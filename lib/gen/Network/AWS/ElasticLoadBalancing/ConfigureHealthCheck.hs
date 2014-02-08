{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticLoadBalancing.ConfigureHealthCheck
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Specifies the health check settings to use for evaluating the health state
-- of your back-end instances. For more information, see Health Check in the
-- Elastic Load Balancing Developer Guide.
-- https://elasticloadbalancing.amazonaws.com/?HealthCheck.HealthyThreshold=2
-- &HealthCheck.UnhealthyThreshold=2 &LoadBalancerName=MyLoadBalancer
-- &HealthCheck.Target=HTTP:80/ping &HealthCheck.Interval=30
-- &HealthCheck.Timeout=3 &Version=2012-06-01 &Action=ConfigureHealthCheck
-- &AUTHPARAMS 30 HTTP:80/ping 2 3 2 83c88b9d-12b7-11e3-8b82-87b12EXAMPLE.
module Network.AWS.ElasticLoadBalancing.ConfigureHealthCheck where

import Network.AWS.Core
import Network.AWS.ElasticLoadBalancing.Service
import Network.AWS.ElasticLoadBalancing.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
configureHealthCheck :: HealthCheck
                     -> Text
                     -> ConfigureHealthCheck
configureHealthCheck p1 p2 = ConfigureHealthCheck
    { chciHealthCheck = p1
    , chciLoadBalancerName = p2
    }

data ConfigureHealthCheck = ConfigureHealthCheck
    { chciHealthCheck :: HealthCheck
      -- ^ A structure containing the configuration information for the new
      -- healthcheck.
    , chciLoadBalancerName :: !Text
      -- ^ The mnemonic name associated with the load balancer. The name must be
      -- unique within the set of load balancers associated with your AWS account.
    } deriving (Eq, Show, Generic)

instance ToQuery ConfigureHealthCheck

instance AWSRequest ConfigureHealthCheck where
    type Er ConfigureHealthCheck = ElasticLoadBalancingError
    type Rs ConfigureHealthCheck = ConfigureHealthCheckResponse
    request = getQuery service "ConfigureHealthCheck"

data ConfigureHealthCheckResponse = ConfigureHealthCheckResponse
    { chcirsHealthCheck :: Maybe HealthCheck
      -- ^ The updated healthcheck for the instances.
    } deriving (Eq, Show, Generic)

instance FromXML ConfigureHealthCheckResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ConfigureHealthCheckResponse"
        :| ["ConfigureHealthCheckResult"]
