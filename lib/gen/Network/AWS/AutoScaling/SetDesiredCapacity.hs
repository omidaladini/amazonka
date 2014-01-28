{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.SetDesiredCapacity
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the desired size of the specified AutoScalingGroup.
-- https://autoscaling.amazonaws.com/?AutoScalingGroupName=my-test-asg
-- &HonorCooldown=false &DesiredCapacity=2 &Version=2011-01-01
-- &Action=SetDesiredCapacity &AUTHPARAMS
-- 9fb7e2db-6998-11e2-a985-57c82EXAMPLE.
module Network.AWS.AutoScaling.SetDesiredCapacity where

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

import Network.AWS.AutoScaling.Service
import Network.AWS.AutoScaling.Types

data SetDesiredCapacity = SetDesiredCapacity
    { sdctAutoScalingGroupName :: !ResourceName
      -- ^ The name of the Auto Scaling group.
    , sdctDesiredCapacity :: !Int
      -- ^ The new capacity setting for the Auto Scaling group.
    , sdctHonorCooldown :: Maybe Bool
      -- ^ By default, SetDesiredCapacity overrides any cooldown period associated
      -- with the Auto Scaling group. Set to True if you want Auto Scaling to wait
      -- for the cooldown period associated with the Auto Scaling group to complete
      -- before initiating a scaling activity to set your Auto Scaling group to the
      -- new capacity setting.
    } deriving (Eq, Show, Generic)

instance ToQuery SetDesiredCapacity

instance AWSRequest SetDesiredCapacity where
    type Er SetDesiredCapacity = AutoScalingError
    type Rs SetDesiredCapacity = SetDesiredCapacityResponse
    request = getQuery service "SetDesiredCapacity"

data SetDesiredCapacityResponse = SetDesiredCapacityResponse
    deriving (Eq, Show, Generic)

instance FromXML SetDesiredCapacityResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "SetDesiredCapacityResponse"
        :| ["SetDesiredCapacityResult"]
