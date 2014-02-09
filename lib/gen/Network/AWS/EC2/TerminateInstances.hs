{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.TerminateInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The TerminateInstances operation shuts down one or more instances. This
-- operation is idempotent; if you terminate an instance more than once, each
-- call will succeed. Terminated instances will remain visible after
-- termination (approximately one hour).
module Network.AWS.EC2.TerminateInstances where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
terminateInstances :: [Text]
                   -- ^ The list of instances to terminate.
                   -> TerminateInstances
terminateInstances p1 = TerminateInstances
    { tiInstanceIds = p1
    , tiDryRun = Nothing
    }

data TerminateInstances = TerminateInstances
    { tiDryRun :: Maybe Bool
    , tiInstanceIds :: [Text]
      -- ^ The list of instances to terminate.
    } deriving (Eq, Show, Generic)

instance ToQuery TerminateInstances

instance AWSRequest TerminateInstances where
    type Er TerminateInstances = EC2Error
    type Rs TerminateInstances = TerminateInstancesResponse
    request  = postQuery service "TerminateInstances"
    response = responseXML

data TerminateInstancesResponse = TerminateInstancesResponse
    { tirTerminatingInstances :: [InstanceStateChange]
      -- ^ The list of the terminating instances and details on how their state has
      -- changed.
    } deriving (Eq, Show, Generic)

instance FromXML TerminateInstancesResponse where
    fromXMLOptions = xmlOptions
