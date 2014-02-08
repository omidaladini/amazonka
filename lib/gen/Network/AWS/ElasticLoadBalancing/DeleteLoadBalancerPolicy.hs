{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticLoadBalancing.DeleteLoadBalancerPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a policy from the load balancer. The specified policy must not be
-- enabled for any listeners.
module Network.AWS.ElasticLoadBalancing.DeleteLoadBalancerPolicy where

import Network.AWS.Core
import Network.AWS.ElasticLoadBalancing.Service
import Network.AWS.ElasticLoadBalancing.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteLoadBalancerPolicy :: Text
                         -- ^ The mnemonic name associated with the load balancer.
                         -> Text
                         -- ^ The mnemonic name for the policy being deleted.
                         -> DeleteLoadBalancerPolicy
deleteLoadBalancerPolicy p1 p2 = DeleteLoadBalancerPolicy
    { dlbpjLoadBalancerName = p1
    , dlbpjPolicyName = p2
    }

data DeleteLoadBalancerPolicy = DeleteLoadBalancerPolicy
    { dlbpjLoadBalancerName :: !Text
      -- ^ The mnemonic name associated with the load balancer.
    , dlbpjPolicyName :: !Text
      -- ^ The mnemonic name for the policy being deleted.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteLoadBalancerPolicy

instance AWSRequest DeleteLoadBalancerPolicy where
    type Er DeleteLoadBalancerPolicy = ElasticLoadBalancingError
    type Rs DeleteLoadBalancerPolicy = DeleteLoadBalancerPolicyResponse
    request = getQuery service "DeleteLoadBalancerPolicy"

data DeleteLoadBalancerPolicyResponse = DeleteLoadBalancerPolicyResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteLoadBalancerPolicyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteLoadBalancerPolicyResponse"
