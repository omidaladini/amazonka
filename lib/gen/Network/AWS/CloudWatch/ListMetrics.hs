{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch.ListMetrics
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of valid metrics stored for the AWS account owner. Returned
-- metrics can be used with GetMetricStatistics to obtain statistical data for
-- a given metric. Up to 500 results are returned for any one call. To
-- retrieve further results, use returned NextToken values with subsequent
-- ListMetrics operations. If you create a metric with the PutMetricData
-- action, allow up to fifteen minutes for the metric to appear in calls to
-- the ListMetrics action.
module Network.AWS.CloudWatch.ListMetrics where

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

import Network.AWS.CloudWatch.Service
import Network.AWS.CloudWatch.Types

data ListMetrics = ListMetrics
    { lmiDimensions :: [DimensionFilter]
      -- ^ A list of dimensions to filter against.
    , lmiMetricName :: Maybe Text
      -- ^ The name of the metric to filter against.
    , lmiNamespace :: Maybe Text
      -- ^ The namespace to filter against.
    , lmiNextToken :: Maybe Text
      -- ^ The token returned by a previous call to indicate that there is more data
      -- available.
    } deriving (Eq, Show, Generic)

instance ToQuery ListMetrics

instance AWSRequest ListMetrics where
    type Er ListMetrics = CloudWatchError
    type Rs ListMetrics = ListMetricsResponse
    request = getQuery service "ListMetrics"

instance AWSPager ListMetrics where
    next rq rs
        | Just x <- lmirsNextToken rs = Just $ rq { lmiNextToken = Just x }
        | otherwise = Nothing

data ListMetricsResponse = ListMetricsResponse
    { lmirsMetrics :: [Metric]
      -- ^ A list of metrics used to generate statistics for an AWS account.
    , lmirsNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned results.
    } deriving (Eq, Show, Generic)

instance FromXML ListMetricsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "ListMetricsResponse"
        :| ["ListMetricsResult"]
