{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch.PutMetricData
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Publishes metric data points to Amazon CloudWatch. Amazon Cloudwatch
-- associates the data points with the specified metric. If the specified
-- metric does not exist, Amazon CloudWatch creates the metric. If you create
-- a metric with the PutMetricData action, allow up to fifteen minutes for the
-- metric to appear in calls to the ListMetrics action. The size of a
-- PutMetricData request is limited to 8 KB for HTTP GET requests and 40 KB
-- for HTTP POST requests. Although the Value parameter accepts numbers of
-- type Double, Amazon CloudWatch truncates values with very large exponents.
-- Values with base-10 exponents greater than 126 (1 x 10^126) are truncated.
-- Likewise, values with base-10 exponents less than -130 (1 x 10^-130) are
-- also truncated.
module Network.AWS.CloudWatch.PutMetricData where

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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
putMetricData :: [MetricDatum]
              -> Text
              -> PutMetricData
putMetricData p1 p2 = PutMetricData
    { pmdiMetricData = p1
    , pmdiNamespace = p2
    }

data PutMetricData = PutMetricData
    { pmdiMetricData :: [MetricDatum]
      -- ^ A list of data describing the metric.
    , pmdiNamespace :: !Text
      -- ^ The namespace for the metric data.
    } deriving (Eq, Show, Generic)

instance ToQuery PutMetricData

instance AWSRequest PutMetricData where
    type Er PutMetricData = CloudWatchError
    type Rs PutMetricData = PutMetricDataResponse
    request = getQuery service "PutMetricData"

data PutMetricDataResponse = PutMetricDataResponse
    deriving (Eq, Show, Generic)

instance FromXML PutMetricDataResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot PutMetricDataResponse
