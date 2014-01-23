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

import Data.ByteString      (ByteString)
import Data.List.NonEmpty   (NonEmpty(..))
import Data.Text            (Text)
import Data.Time            (UTCTime)
import Network.HTTP.Types   (StdMethod(..))
import Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)
import Network.AWS.EC2.Service
import Network.AWS.EC2.Types

data MonitorInstances = MonitorInstances
    { mirDryRun :: Maybe Bool
      -- ^ FIXME: Missing documentation
    , mirInstanceIds :: [Text]
      -- ^ The list of Amazon EC2 instances on which to enable monitoring.
    } deriving (Eq, Show, Generic)

instance ToQuery MonitorInstances

instance AWSRequest MonitorInstances where
    type Er MonitorInstances = EC2Error
    type Rs MonitorInstances = MonitorInstancesResponse
    request = v2Query service GET "MonitorInstances"

data MonitorInstancesResponse = MonitorInstancesResponse
    { mirrsInstanceMonitorings :: [InstanceMonitoring]
      -- ^ A list of updated monitoring information for the instances specified in the
      -- request.
    } deriving (Eq, Show, Generic)

instance FromXML MonitorInstancesResponse where
    fromXMLOptions = xmlOptions
