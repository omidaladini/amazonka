{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DescribeMetricCollectionTypes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of metrics and a corresponding list of granularities for
-- each metric.
-- https://autoscaling.amazonaws.com/?Version=2011-01-01&Action=DescribeMetricCollectionTypes
-- &AUTHPARAMS oc/2011-01-01/"> GroupMinSize GroupMaxSize GroupDesiredCapacity
-- GroupInServiceInstances GroupPendingInstances GroupTerminatingInstances
-- GroupTotalInstances 1Minute 07f3fea2-bf3c-11e2-9b6f-f3cdbb80c073.
module Network.AWS.AutoScaling.DescribeMetricCollectionTypes where

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
describeMetricCollectionTypes :: AWS (Either AutoScalingError DescribeMetricCollectionTypesResponse)
describeMetricCollectionTypes = undefined DescribeMetricCollectionTypes

data DescribeMetricCollectionTypes = DescribeMetricCollectionTypes
    deriving (Eq, Show, Generic)

instance ToQuery DescribeMetricCollectionTypes

instance AWSRequest DescribeMetricCollectionTypes where
    type Er DescribeMetricCollectionTypes = AutoScalingError
    type Rs DescribeMetricCollectionTypes = DescribeMetricCollectionTypesResponse
    request = getQuery service "DescribeMetricCollectionTypes"

data DescribeMetricCollectionTypesResponse = DescribeMetricCollectionTypesResponse
    { dmctaGranularities :: [MetricGranularityType]
      -- ^ A list of granularities for the listed Metrics.
    , dmctaMetrics :: [MetricCollectionType]
      -- ^ The list of Metrics collected. The following metrics are supported:
      -- GroupMinSize GroupMaxSize GroupDesiredCapacity GroupInServiceInstances
      -- GroupPendingInstances GroupTerminatingInstances GroupTotalInstances.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeMetricCollectionTypesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeMetricCollectionTypesResponse"
        :| ["DescribeMetricCollectionTypesResult"]
