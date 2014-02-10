{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DisableMetricsCollection
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Disables monitoring of group metrics for the Auto Scaling group specified
-- in AutoScalingGroupName. You can specify the list of affected metrics with
-- the Metrics parameter.
module Network.AWS.AutoScaling.DisableMetricsCollection where

import Network.AWS.Core
import Network.AWS.AutoScaling.Service
import Network.AWS.AutoScaling.Types

-- | Convenience method utilising default fields to construct
-- the minimum required request.
disableMetricsCollection :: ResourceName
                         -- ^ The name or ARN of the Auto Scaling Group.
                         -> DisableMetricsCollection
disableMetricsCollection p1 = DisableMetricsCollection
    { dmcqAutoScalingGroupName = p1
    , dmcqMetrics = []
    }

data DisableMetricsCollection = DisableMetricsCollection
    { dmcqAutoScalingGroupName :: !ResourceName
      -- ^ The name or ARN of the Auto Scaling Group.
    , dmcqMetrics :: [Text]
      -- ^ The list of metrics to disable. If no metrics are specified, all metrics
      -- are disabled. The following metrics are supported: GroupMinSize
      -- GroupMaxSize GroupDesiredCapacity GroupInServiceInstances
      -- GroupPendingInstances GroupTerminatingInstances GroupTotalInstances.
    } deriving (Eq, Show, Generic)

instance ToQuery DisableMetricsCollection

instance AWSRequest DisableMetricsCollection where
    type Er DisableMetricsCollection = AutoScalingError
    type Rs DisableMetricsCollection = DisableMetricsCollectionResponse
    request  = postQuery service "DisableMetricsCollection"
    response = responseXML

data DisableMetricsCollectionResponse = DisableMetricsCollectionResponse
    deriving (Eq, Show, Generic)

instance FromXML DisableMetricsCollectionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DisableMetricsCollectionResponse"
