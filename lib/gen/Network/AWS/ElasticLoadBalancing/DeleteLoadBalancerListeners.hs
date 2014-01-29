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

-- | Convenience method utilising default fields where applicable.
deleteLoadBalancerListeners :: Text
                            -> [Int]
                            -> AWS (Either ElasticLoadBalancingError DeleteLoadBalancerListenersResponse)
deleteLoadBalancerListeners p1 p2 = undefined $ DeleteLoadBalancerListeners
    { dlbliLoadBalancerName = p1
    , dlbliLoadBalancerPorts = p2
    }

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
    request = getQuery service "DeleteLoadBalancerListeners"

data DeleteLoadBalancerListenersResponse = DeleteLoadBalancerListenersResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteLoadBalancerListenersResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DeleteLoadBalancerListenersResponse"
        :| ["DeleteLoadBalancerListenersResult"]
