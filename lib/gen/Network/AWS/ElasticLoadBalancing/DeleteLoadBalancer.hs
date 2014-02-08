{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticLoadBalancing.DeleteLoadBalancer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified load balancer. If attempting to recreate the load
-- balancer, you must reconfigure all the settings. The DNS name associated
-- with a deleted load balancer will no longer be usable. Once deleted, the
-- name and associated DNS record of the load balancer no longer exist and
-- traffic sent to any of its IP addresses will no longer be delivered to
-- back-end instances. To successfully call this API, you must provide the
-- same account credentials as were used to create the load balancer. By
-- design, if the load balancer does not exist or has already been deleted, a
-- call to DeleteLoadBalancer action still succeeds.
module Network.AWS.ElasticLoadBalancing.DeleteLoadBalancer where

import Network.AWS.Core
import Network.AWS.ElasticLoadBalancing.Service
import Network.AWS.ElasticLoadBalancing.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteLoadBalancer :: Text
                   -- ^ The name associated with the load balancer.
                   -> DeleteLoadBalancer
deleteLoadBalancer p1 = DeleteLoadBalancer
    { dapjLoadBalancerName = p1
    }

data DeleteLoadBalancer = DeleteLoadBalancer
    { dapjLoadBalancerName :: !Text
      -- ^ The name associated with the load balancer.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteLoadBalancer

instance AWSRequest DeleteLoadBalancer where
    type Er DeleteLoadBalancer = ElasticLoadBalancingError
    type Rs DeleteLoadBalancer = DeleteLoadBalancerResponse
    request = getQuery service "DeleteLoadBalancer"

data DeleteLoadBalancerResponse = DeleteLoadBalancerResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteLoadBalancerResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteLoadBalancerResponse"
