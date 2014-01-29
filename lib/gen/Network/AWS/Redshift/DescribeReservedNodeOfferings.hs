{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DescribeReservedNodeOfferings
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of the available reserved node offerings by Amazon Redshift
-- with their descriptions including the node type, the fixed and recurring
-- costs of reserving the node and duration the node will be reserved for you.
-- These descriptions help you determine which reserve node offering you want
-- to purchase. You then use the unique offering ID in you call to
-- PurchaseReservedNodeOffering to reserve one or more nodes for your Amazon
-- Redshift cluster. For more information about managing parameter groups, go
-- to Purchasing Reserved Nodes in the Amazon Redshift Management Guide.
-- https://redshift.us-east-1.amazonaws.com/
-- ?Action=DescribeReservedNodeOfferings &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130117/us-east-1/redshift/aws4_request
-- &x-amz-date=20130117T232351Z
-- &x-amz-signedheaders=content-type;host;x-amz-date Heavy Utilization
-- 94608000 Hourly 0.21 12452.0 3a98bf7d-979a-49cc-b568-18f24315baf0 0.0
-- dw.hs1.8xlarge Heavy Utilization 31536000 Hourly 0.09 1815.0
-- d586503b-289f-408b-955b-9c95005d6908 0.0 dw.hs1.xlarge
-- f4a07e06-60fc-11e2-95d9-658e9466d117.
module Network.AWS.Redshift.DescribeReservedNodeOfferings where

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

import Network.AWS.Redshift.Service
import Network.AWS.Redshift.Types

-- | Convenience method utilising default fields where applicable.
describeReservedNodeOfferings :: AWS (Either RedshiftError DescribeReservedNodeOfferingsResponse)
describeReservedNodeOfferings = undefined $ DescribeReservedNodeOfferings
    { drnomMarker = Nothing
    , drnomMaxRecords = Nothing
    , drnomReservedNodeOfferingId = Nothing
    }

data DescribeReservedNodeOfferings = DescribeReservedNodeOfferings
    { drnomMarker :: Maybe Text
      -- ^ An optional marker returned by a previous DescribeReservedNodeOfferings
      -- request to indicate the first offering that the request will return. You
      -- can specify either a Marker parameter or a ClusterIdentifier parameter in a
      -- DescribeClusters request, but not both.
    , drnomMaxRecords :: Maybe Int
      -- ^ The maximum number of records to include in the response. If more records
      -- exist than the specified MaxRecords value, a marker is included in the
      -- response so that the remaining results may be retrieved. Default: 100
      -- Constraints: minimum 20, maximum 100.
    , drnomReservedNodeOfferingId :: Maybe Text
      -- ^ The unique identifier for the offering.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeReservedNodeOfferings

instance AWSRequest DescribeReservedNodeOfferings where
    type Er DescribeReservedNodeOfferings = RedshiftError
    type Rs DescribeReservedNodeOfferings = DescribeReservedNodeOfferingsResponse
    request = getQuery service "DescribeReservedNodeOfferings"

data DescribeReservedNodeOfferingsResponse = DescribeReservedNodeOfferingsResponse
    { drnomrsMarker :: Maybe Text
      -- ^ An optional marker returned by a previous DescribeReservedNodeOfferings
      -- request to indicate the first reserved node offering that the request will
      -- return.
    , drnomrsReservedNodeOfferings :: [ReservedNodeOffering]
      -- ^ A list of reserved node offerings.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeReservedNodeOfferingsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeReservedNodeOfferingsResponse"
        :| ["DescribeReservedNodeOfferingsResult"]
