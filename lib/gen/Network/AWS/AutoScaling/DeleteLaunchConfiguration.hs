{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DeleteLaunchConfiguration
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified LaunchConfiguration. The specified launch
-- configuration must not be attached to an Auto Scaling group. When this call
-- completes, the launch configuration is no longer available for use.
-- https://autoscaling.amazonaws.com/?LaunchConfigurationName=my-test-lc
-- &Version=2011-01-01 &Action=DeleteLaunchConfiguration &AUTHPARAMS
-- 7347261f-97df-11e2-8756-35eEXAMPLE.
module Network.AWS.AutoScaling.DeleteLaunchConfiguration where

import Network.AWS.Core
import Network.AWS.AutoScaling.Service
import Network.AWS.AutoScaling.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
deleteLaunchConfiguration :: ResourceName
                          -> DeleteLaunchConfiguration
deleteLaunchConfiguration p1 = DeleteLaunchConfiguration
    { lcntLaunchConfigurationName = p1
    }

data DeleteLaunchConfiguration = DeleteLaunchConfiguration
    { lcntLaunchConfigurationName :: !ResourceName
      -- ^ The name of the launch configuration.
    } deriving (Eq, Show, Generic)

instance ToQuery DeleteLaunchConfiguration

instance AWSRequest DeleteLaunchConfiguration where
    type Er DeleteLaunchConfiguration = AutoScalingError
    type Rs DeleteLaunchConfiguration = DeleteLaunchConfigurationResponse
    request = getQuery service "DeleteLaunchConfiguration"

data DeleteLaunchConfigurationResponse = DeleteLaunchConfigurationResponse
    deriving (Eq, Show, Generic)

instance FromXML DeleteLaunchConfigurationResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteLaunchConfigurationResponse"
