{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.MonitorInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Enables monitoring for a running instance.
module Network.AWS.EC2.MonitorInstances where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
monitorInstances :: [Text]
                 -- ^ The list of Amazon EC2 instances on which to enable monitoring.
                 -> MonitorInstances
monitorInstances p1 = MonitorInstances
    { miInstanceIds = p1
    , miDryRun = Nothing
    }

data MonitorInstances = MonitorInstances
    { miDryRun :: Maybe Bool
    , miInstanceIds :: [Text]
      -- ^ The list of Amazon EC2 instances on which to enable monitoring.
    } deriving (Eq, Show, Generic)

instance ToQuery MonitorInstances

instance AWSRequest MonitorInstances where
    type Er MonitorInstances = EC2Error
    type Rs MonitorInstances = MonitorInstancesResponse
    request  = postQuery service "MonitorInstances"
    response = responseXML

data MonitorInstancesResponse = MonitorInstancesResponse
    { mirInstanceMonitorings :: [InstanceMonitoring]
      -- ^ A list of updated monitoring information for the instances specified in the
      -- request.
    } deriving (Eq, Show, Generic)

instance FromXML MonitorInstancesResponse where
    fromXMLOptions = xmlOptions
