{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticLoadBalancing.DeleteLoadBalancerListeners
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes listeners from the load balancer for the specified port.
module Network.AWS.ElasticLoadBalancing.DeleteLoadBalancerListeners where

import Network.AWS.Core
import Network.AWS.ElasticLoadBalancing.Service
import Network.AWS.ElasticLoadBalancing.Types

data DeleteLoadBalancerListeners = DeleteLoadBalancerListeners
    { dlbliLoadBalancerName :: !Text
      -- ^ The mnemonic name associated with the load balancer.
    , dlbliLoadBalancerPorts :: [Int]
      -- ^ The client port number(s) of the load balancer listener(s) to be removed.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteLoadBalancerListeners

instance AWSRequest DeleteLoadBalancerListeners where
    type Er DeleteLoadBalancerListeners = ElasticLoadBalancingError
    type Rs DeleteLoadBalancerListeners = DeleteLoadBalancerListenersResponse
    request  = postQuery service "DeleteLoadBalancerListeners"
    response = responseXML

data DeleteLoadBalancerListenersResponse = DeleteLoadBalancerListenersResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteLoadBalancerListenersResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteLoadBalancerListenersResponse"
