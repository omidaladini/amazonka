{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.UnmonitorInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Disables monitoring for a running instance.
module Network.AWS.EC2.UnmonitorInstances where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
unmonitorInstances :: [Text]
                   -- ^ The list of Amazon EC2 instances on which to disable monitoring.
                   -> UnmonitorInstances
unmonitorInstances p1 = UnmonitorInstances
    { uirInstanceIds = p1
    , uirDryRun = Nothing
    }

data UnmonitorInstances = UnmonitorInstances
    { uirDryRun :: Maybe Bool
    , uirInstanceIds :: [Text]
      -- ^ The list of Amazon EC2 instances on which to disable monitoring.
    } deriving (Eq, Show, Generic)

instance ToQuery UnmonitorInstances

instance AWSRequest UnmonitorInstances where
    type Er UnmonitorInstances = EC2Error
    type Rs UnmonitorInstances = UnmonitorInstancesResponse
    request = getQuery service "UnmonitorInstances"

data UnmonitorInstancesResponse = UnmonitorInstancesResponse
    { uirrInstanceMonitorings :: [InstanceMonitoring]
      -- ^ A list of updated monitoring information for the instances specified in the
      -- request.
    } deriving (Eq, Show, Generic)

instance FromXML UnmonitorInstancesResponse where
    fromXMLOptions = xmlOptions
