{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.RegisterElasticIp
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Registers an Elastic IP address with a specified stack. An address can be
-- registered with only one stack at a time. If the address is already
-- registered, you must first deregister it by calling DeregisterElasticIp.
-- For more information, see Resource Management. Required Permissions: To use
-- this action, an IAM user must have a Manage permissions level for the
-- stack, or an attached policy that explicitly grants permissions. For more
-- information on user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.RegisterElasticIp where

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
registerElasticIp :: Text
                  -> Text
                  -> RegisterElasticIp
registerElasticIp p1 p2 = undefined $ RegisterElasticIp
    { reirElasticIp = p1
    , reirStackId = p2
    }

data RegisterElasticIp = RegisterElasticIp
    { reirElasticIp :: !Text
      -- ^ The Elastic IP address.
    , reirStackId :: !Text
      -- ^ The stack ID.
    } deriving (Eq, Show, Generic)

instance ToJSON RegisterElasticIp

instance AWSRequest RegisterElasticIp where
    type Er RegisterElasticIp = OpsWorksError
    type Rs RegisterElasticIp = RegisterElasticIpResponse
    request  = getJSON service
    response = responseJSON

data RegisterElasticIpResponse = RegisterElasticIpResponse
    { reirrsElasticIp :: Maybe Text
      -- ^ The Elastic IP address.
    } deriving (Eq, Show, Generic)

instance FromJSON RegisterElasticIpResponse
