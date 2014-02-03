{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.SetLoadBasedAutoScaling
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Specify the load-based auto scaling configuration for a specified layer.
-- For more information, see Managing Load with Time-based and Load-based
-- Instances. To use load-based auto scaling, you must create a set of
-- load-based auto scaling instances. Load-based auto scaling operates only on
-- the instances from that set, so you must ensure that you have created
-- enough instances to handle the maximum anticipated load. Required
-- Permissions: To use this action, an IAM user must have a Manage permissions
-- level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see Managing User
-- Permissions.
module Network.AWS.OpsWorks.SetLoadBasedAutoScaling where

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
setLoadBasedAutoScaling :: Text
                        -> SetLoadBasedAutoScaling
setLoadBasedAutoScaling p1 = SetLoadBasedAutoScaling
    { slbasrLayerId = p1
    , slbasrDownScaling = Nothing
    , slbasrEnable = Nothing
    , slbasrUpScaling = Nothing
    }

data SetLoadBasedAutoScaling = SetLoadBasedAutoScaling
    { slbasrDownScaling :: Maybe AutoScalingThresholds
      -- ^ An AutoScalingThresholds object with the downscaling threshold
      -- configuration. If the load falls below these thresholds for a specified
      -- amount of time, AWS OpsWorks stops a specified number of instances.
    , slbasrEnable :: Maybe Bool
      -- ^ Enables load-based auto scaling for the layer.
    , slbasrLayerId :: !Text
      -- ^ The layer ID.
    , slbasrUpScaling :: Maybe AutoScalingThresholds
      -- ^ An AutoScalingThresholds object with the upscaling threshold configuration.
      -- If the load exceeds these thresholds for a specified amount of time, AWS
      -- OpsWorks starts a specified number of instances.
    } deriving (Eq, Show, Generic)

instance ToJSON SetLoadBasedAutoScaling where
    toJSON = genericToJSON jsonOptions

instance AWSRequest SetLoadBasedAutoScaling where
    type Er SetLoadBasedAutoScaling = OpsWorksError
    type Rs SetLoadBasedAutoScaling = SetLoadBasedAutoScalingResponse
    request  = getJSON service
    response = responseJSON

data SetLoadBasedAutoScalingResponse = SetLoadBasedAutoScalingResponse
    deriving (Eq, Show, Generic)

instance FromJSON SetLoadBasedAutoScalingResponse where
    fromJSON = genericFromJSON jsonOptions

