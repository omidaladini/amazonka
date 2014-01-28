{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.EnableMetricsCollection
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Enables monitoring of group metrics for the Auto Scaling group specified in
-- AutoScalingGroupName. You can specify the list of enabled metrics with the
-- Metrics parameter. Auto Scaling metrics collection can be turned on only if
-- the InstanceMonitoring flag, in the Auto Scaling group's launch
-- configuration, is set to True.
module Network.AWS.AutoScaling.EnableMetricsCollection where

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

data EnableMetricsCollection = EnableMetricsCollection
    { emcqAutoScalingGroupName :: !ResourceName
      -- ^ The name or ARN of the Auto Scaling group.
    , emcqGranularity :: !Text
      -- ^ The granularity to associate with the metrics to collect. Currently, the
      -- only legal granularity is "1Minute".
    , emcqMetrics :: [Text]
      -- ^ The list of metrics to collect. If no metrics are specified, all metrics
      -- are enabled. The following metrics are supported: GroupMinSize GroupMaxSize
      -- GroupDesiredCapacity GroupInServiceInstances GroupPendingInstances
      -- GroupTerminatingInstances GroupTotalInstances.
    } deriving (Eq, Show, Generic)

instance ToQuery EnableMetricsCollection

instance AWSRequest EnableMetricsCollection where
    type Er EnableMetricsCollection = AutoScalingError
    type Rs EnableMetricsCollection = EnableMetricsCollectionResponse
    request = getQuery service "EnableMetricsCollection"

data EnableMetricsCollectionResponse = EnableMetricsCollectionResponse
    deriving (Eq, Show, Generic)

instance FromXML EnableMetricsCollectionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "EnableMetricsCollectionResponse"
        :| ["EnableMetricsCollectionResult"]
