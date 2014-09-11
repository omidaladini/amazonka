{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ELB.ConfigureHealthCheck
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
module Network.AWS.ELB.ConfigureHealthCheck
    (
    -- * Request
      ConfigureHealthCheck
    -- ** Request constructor
    , mkConfigureHealthCheck
    -- ** Request lenses
    , chcLoadBalancerName
    , chcHealthCheck

    -- * Response
    , ConfigureHealthCheckResponse
    -- ** Response constructor
    , mkConfigureHealthCheckResponse
    -- ** Response lenses
    , chcrHealthCheck
    ) where

import Network.AWS.Request.Query
import Network.AWS.ELB.Types
import Network.AWS.Prelude

-- | Input for the ConfigureHealthCheck action.
data ConfigureHealthCheck = ConfigureHealthCheck
    { _chcLoadBalancerName :: !Text
    , _chcHealthCheck :: HealthCheck
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ConfigureHealthCheck' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LoadBalancerName ::@ @Text@
--
-- * @HealthCheck ::@ @HealthCheck@
--
mkConfigureHealthCheck :: Text -- ^ 'chcLoadBalancerName'
                       -> HealthCheck -- ^ 'chcHealthCheck'
                       -> ConfigureHealthCheck
mkConfigureHealthCheck p1 p2 = ConfigureHealthCheck
    { _chcLoadBalancerName = p1
    , _chcHealthCheck = p2
    }

-- | The mnemonic name associated with the load balancer. The name must be
-- unique within the set of load balancers associated with your AWS account.
chcLoadBalancerName :: Lens' ConfigureHealthCheck Text
chcLoadBalancerName =
    lens _chcLoadBalancerName (\s a -> s { _chcLoadBalancerName = a })

-- | A structure containing the configuration information for the new
-- healthcheck.
chcHealthCheck :: Lens' ConfigureHealthCheck HealthCheck
chcHealthCheck = lens _chcHealthCheck (\s a -> s { _chcHealthCheck = a })

instance ToQuery ConfigureHealthCheck where
    toQuery = genericQuery def

-- | The output for the ConfigureHealthCheck action.
newtype ConfigureHealthCheckResponse = ConfigureHealthCheckResponse
    { _chcrHealthCheck :: Maybe HealthCheck
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ConfigureHealthCheckResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @HealthCheck ::@ @Maybe HealthCheck@
--
mkConfigureHealthCheckResponse :: ConfigureHealthCheckResponse
mkConfigureHealthCheckResponse = ConfigureHealthCheckResponse
    { _chcrHealthCheck = Nothing
    }

-- | The updated healthcheck for the instances.
chcrHealthCheck :: Lens' ConfigureHealthCheckResponse (Maybe HealthCheck)
chcrHealthCheck = lens _chcrHealthCheck (\s a -> s { _chcrHealthCheck = a })

instance FromXML ConfigureHealthCheckResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ConfigureHealthCheck where
    type Sv ConfigureHealthCheck = ELB
    type Rs ConfigureHealthCheck = ConfigureHealthCheckResponse

    request = post "ConfigureHealthCheck"
    response _ = xmlResponse