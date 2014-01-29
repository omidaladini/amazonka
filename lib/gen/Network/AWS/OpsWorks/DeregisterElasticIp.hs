{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.DeregisterElasticIp
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deregisters a specified Elastic IP address. The address can then be
-- registered by another stack. For more information, see Resource Management.
-- Required Permissions: To use this action, an IAM user must have a Manage
-- permissions level for the stack, or an attached policy that explicitly
-- grants permissions. For more information on user permissions, see Managing
-- User Permissions.
module Network.AWS.OpsWorks.DeregisterElasticIp where

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

-- | Convenience method utilising default fields where applicable.
deregisterElasticIp :: Text
                    -> AWS (Either OpsWorksError DeregisterElasticIpResponse)
deregisterElasticIp p1 = undefined $ DeregisterElasticIp
    { deisElasticIp = p1
    }

data DeregisterElasticIp = DeregisterElasticIp
    { deisElasticIp :: !Text
      -- ^ The Elastic IP address.
    } deriving (Eq, Show, Generic)

instance ToJSON DeregisterElasticIp

instance AWSRequest DeregisterElasticIp where
    type Er DeregisterElasticIp = OpsWorksError
    type Rs DeregisterElasticIp = DeregisterElasticIpResponse
    request  = getJSON service
    response = responseJSON

data DeregisterElasticIpResponse = DeregisterElasticIpResponse
    deriving (Eq, Show, Generic)

instance FromJSON DeregisterElasticIpResponse
