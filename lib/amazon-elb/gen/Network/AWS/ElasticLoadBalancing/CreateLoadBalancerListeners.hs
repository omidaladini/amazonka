{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElasticLoadBalancing.CreateLoadBalancerListeners
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates one or more listeners on a load balancer for the specified port. If
-- a listener with the given port does not already exist, it will be created;
-- otherwise, the properties of the new listener must match the properties of
-- the existing listener. For more information, see Add a Listener to Your
-- Load Balancer in the Elastic Load Balancing Developer Guide. Create HTTPS
-- load balancer listener in EC2-Classic
-- https://elasticloadbalancing.amazonaws.com/?Listeners.member.1.Protocol=https
-- &Listeners.member.1.LoadBalancerPort=443
-- &Listeners.member.1.InstancePort=443
-- &Listeners.member.1.InstanceProtocol=https
-- &Listeners.member.1.SSLCertificateId=arn:aws:iam::123456789012:server-certificate/servercert
-- &LoadBalancerName=MyHTTPSLoadBalancer &Version=2012-06-01
-- &Action=CreateLoadBalancerListeners &AUTHPARAMS
-- 1549581b-12b7-11e3-895e-1334aEXAMPLE.
module Network.AWS.ElasticLoadBalancing.CreateLoadBalancerListeners where

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
createLoadBalancerListeners :: [Listener]
                            -> Text
                            -> CreateLoadBalancerListeners
createLoadBalancerListeners p1 p2 = CreateLoadBalancerListeners
    { clbliListeners = p1
    , clbliLoadBalancerName = p2
    }

data CreateLoadBalancerListeners = CreateLoadBalancerListeners
    { clbliListeners :: [Listener]
      -- ^ A list of LoadBalancerPort, InstancePort, Protocol, and SSLCertificateId
      -- items.
    , clbliLoadBalancerName :: !Text
      -- ^ The name of the load balancer.
    } deriving (Eq, Show, Generic)

instance ToQuery CreateLoadBalancerListeners

instance AWSRequest CreateLoadBalancerListeners where
    type Er CreateLoadBalancerListeners = ElasticLoadBalancingError
    type Rs CreateLoadBalancerListeners = CreateLoadBalancerListenersResponse
    request = getQuery service "CreateLoadBalancerListeners"

data CreateLoadBalancerListenersResponse = CreateLoadBalancerListenersResponse
    deriving (Eq, Show, Generic)

instance FromXML CreateLoadBalancerListenersResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot CreateLoadBalancerListenersResponse
