{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.StopInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Stops an instance that uses an Amazon EBS volume as its root device.
-- Instances that use Amazon EBS volumes as their root devices can be quickly
-- stopped and started. When an instance is stopped, the compute resources are
-- released and you are not billed for hourly instance usage. However, your
-- root partition Amazon EBS volume remains, continues to persist your data,
-- and you are charged for Amazon EBS volume usage. You can restart your
-- instance at any time. Before stopping an instance, make sure it is in a
-- state from which it can be restarted. Stopping an instance does not
-- preserve data stored in RAM. Performing this operation on an instance that
-- uses an instance store as its root device returns an error.
module Network.AWS.EC2.StopInstances where

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data StopInstances = StopInstances
    { sirDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , sirForce :: Maybe Bool
      -- ^ Forces the instance to stop. The instance will not have an opportunity to
      -- flush file system caches nor file system meta data. If you use this option,
      -- you must perform file system check and repair procedures. This option is
      -- not recommended for Windows instances.
    , sirInstanceIds :: [Text]
      -- ^ The list of Amazon EC2 instances to stop.
    } deriving (Eq, Show, Generic)

instance ToQuery StopInstances

instance AWSRequest StopInstances where
    type Er StopInstances = EC2Error
    type Rs StopInstances = StopInstancesResponse
    request = v2Query service GET "StopInstances"

data StopInstancesResponse = StopInstancesResponse
    { sirrsStoppingInstances :: [InstanceStateChange]
      -- ^ The list of the stopping instances and details on how their state has
      -- changed.
    } deriving (Eq, Show, Generic)

instance FromXML StopInstancesResponse where
    fromXMLOptions = xmlOptions
