{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.SetInstanceHealth
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the health status of a specified instance that belongs to any of your
-- Auto Scaling groups. For more information, see Configure Health Checks for
-- Your Auto Scaling group.
module Network.AWS.AutoScaling.SetInstanceHealth where

import Network.AWS.Core
import Network.AWS.AutoScaling.Service
import Network.AWS.AutoScaling.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
setInstanceHealth :: Text
                  -- ^ The health status of the instance. Set to Healthy if you want the instance
                  -- to remain in service. Set to Unhealthy if you want the instance to be out
                  -- of service. Auto Scaling will terminate and replace the unhealthy instance.
                  -> Text
                  -- ^ The identifier of the Amazon EC2 instance.
                  -> SetInstanceHealth
setInstanceHealth p1 p2 = SetInstanceHealth
    { sihqHealthStatus = p1
    , sihqInstanceId = p2
    , sihqShouldRespectGracePeriod = Nothing
    }

data SetInstanceHealth = SetInstanceHealth
    { sihqHealthStatus :: !Text
      -- ^ The health status of the instance. Set to Healthy if you want the instance
      -- to remain in service. Set to Unhealthy if you want the instance to be out
      -- of service. Auto Scaling will terminate and replace the unhealthy instance.
    , sihqInstanceId :: !Text
      -- ^ The identifier of the Amazon EC2 instance.
    , sihqShouldRespectGracePeriod :: Maybe Bool
      -- ^ If the Auto Scaling group of the specified instance has a
      -- HealthCheckGracePeriod specified for the group, by default, this call will
      -- respect the grace period. Set this to False, if you do not want the call to
      -- respect the grace period associated with the group. For more information,
      -- see the HealthCheckGracePeriod parameter description in the
      -- CreateAutoScalingGroup action.
    } deriving (Eq, Show, Generic)

instance ToQuery SetInstanceHealth

instance AWSRequest SetInstanceHealth where
    type Er SetInstanceHealth = AutoScalingError
    type Rs SetInstanceHealth = SetInstanceHealthResponse
    request = getQuery service "SetInstanceHealth"

data SetInstanceHealthResponse = SetInstanceHealthResponse
    deriving (Eq, Show, Generic)

instance FromXML SetInstanceHealthResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SetInstanceHealthResponse"
