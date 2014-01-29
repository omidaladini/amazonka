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

-- | Convenience method utilising default fields where applicable.
disableMetricsCollection :: ResourceName
                         -> AWS (Either AutoScalingError DisableMetricsCollectionResponse)
disableMetricsCollection p1 = undefined $ DisableMetricsCollection
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
    request = getQuery service "DisableMetricsCollection"

data DisableMetricsCollectionResponse = DisableMetricsCollectionResponse
    deriving (Eq, Show, Generic)

instance FromXML DisableMetricsCollectionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DisableMetricsCollectionResponse"
        :| ["DisableMetricsCollectionResult"]
