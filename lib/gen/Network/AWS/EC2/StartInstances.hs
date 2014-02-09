{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.StartInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Starts an instance that uses an Amazon EBS volume as its root device.
-- Instances that use Amazon EBS volumes as their root devices can be quickly
-- stopped and started. When an instance is stopped, the compute resources are
-- released and you are not billed for hourly instance usage. However, your
-- root partition Amazon EBS volume remains, continues to persist your data,
-- and you are charged for Amazon EBS volume usage. You can restart your
-- instance at any time. Performing this operation on an instance that uses an
-- instance store as its root device returns an error.
module Network.AWS.EC2.StartInstances where

import Network.AWS.Core
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
startInstances :: [Text]
               -- ^ The list of Amazon EC2 instances to start.
               -> StartInstances
startInstances p1 = StartInstances
    { sidInstanceIds = p1
    , sidAdditionalInfo = Nothing
    , sidDryRun = Nothing
    }

data StartInstances = StartInstances
    { sidAdditionalInfo :: Maybe Text
    , sidDryRun :: Maybe Bool
    , sidInstanceIds :: [Text]
      -- ^ The list of Amazon EC2 instances to start.
    } deriving (Eq, Show, Generic)

instance ToQuery StartInstances

instance AWSRequest StartInstances where
    type Er StartInstances = EC2Error
    type Rs StartInstances = StartInstancesResponse
    request = getQuery service "StartInstances"

data StartInstancesResponse = StartInstancesResponse
    { sidrStartingInstances :: [InstanceStateChange]
      -- ^ The list of the starting instances and details on how their state has
      -- changed.
    } deriving (Eq, Show, Generic)

instance FromXML StartInstancesResponse where
    fromXMLOptions = xmlOptions
