{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.AttachElasticLoadBalancer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Attaches an Elastic Load Balancing load balancer to a specified layer. You
-- must create the Elastic Load Balancing instance separately, by using the
-- Elastic Load Balancing console, API, or CLI. For more information, see
-- Elastic Load Balancing Developer Guide. Required Permissions: To use this
-- action, an IAM user must have a Manage permissions level for the stack, or
-- an attached policy that explicitly grants permissions. For more information
-- on user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.AttachElasticLoadBalancer where

import           Control.Monad
import           Data.Aeson
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.OpsWorks.Service
import Network.AWS.OpsWorks.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
attachElasticLoadBalancer :: Text
                          -> Text
                          -> AttachElasticLoadBalancer
attachElasticLoadBalancer p1 p2 = undefined $ AttachElasticLoadBalancer
    { aelbrElasticLoadBalancerName = p1
    , aelbrLayerId = p2
    }

data AttachElasticLoadBalancer = AttachElasticLoadBalancer
    { aelbrElasticLoadBalancerName :: !Text
      -- ^ The Elastic Load Balancing instance's name.
    , aelbrLayerId :: !Text
      -- ^ The ID of the layer that the Elastic Load Balancing instance is to be
      -- attached to.
    } deriving (Eq, Show, Generic)

instance ToJSON AttachElasticLoadBalancer

instance AWSRequest AttachElasticLoadBalancer where
    type Er AttachElasticLoadBalancer = OpsWorksError
    type Rs AttachElasticLoadBalancer = AttachElasticLoadBalancerResponse
    request  = getJSON service
    response = responseJSON

data AttachElasticLoadBalancerResponse = AttachElasticLoadBalancerResponse
    deriving (Eq, Show, Generic)

instance FromJSON AttachElasticLoadBalancerResponse
