{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticLoadBalancing.ModifyLoadBalancerAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies the attributes of a specified load balancer.
module Network.AWS.ElasticLoadBalancing.ModifyLoadBalancerAttributes where

import Network.AWS.Core
import Network.AWS.ElasticLoadBalancing.Service
import Network.AWS.ElasticLoadBalancing.Types

data ModifyLoadBalancerAttributes = ModifyLoadBalancerAttributes
    { mlbaiLoadBalancerAttributes :: LoadBalancerAttributes
      -- ^ Attributes of the load balancer.
    , mlbaiLoadBalancerName :: !Text
      -- ^ The name of the load balancer.
    } deriving (Eq, Show, Generic)

instance ToQuery ModifyLoadBalancerAttributes

instance AWSRequest ModifyLoadBalancerAttributes where
    type Er ModifyLoadBalancerAttributes = ElasticLoadBalancingError
    type Rs ModifyLoadBalancerAttributes = ModifyLoadBalancerAttributesResponse
    request  = postQuery service "ModifyLoadBalancerAttributes"
    response = responseXML

data ModifyLoadBalancerAttributesResponse = ModifyLoadBalancerAttributesResponse
    deriving (Eq, Show, Generic)

instance FromXML ModifyLoadBalancerAttributesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ModifyLoadBalancerAttributesResponse"
