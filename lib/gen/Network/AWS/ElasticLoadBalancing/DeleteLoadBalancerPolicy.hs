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
    fromXMLRoot    = fromNestedRoot
         $ "DeleteLoadBalancerPolicyResponse"
        :| ["DeleteLoadBalancerPolicyResult"]
