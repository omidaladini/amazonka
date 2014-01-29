{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DescribeReservedNodes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the descriptions of the reserved nodes.
-- https://redshift.us-east-1.amazonaws.com/ ?Action=DescribeReservedNodes
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130125/us-east-1/redshift/aws4_request
-- &x-amz-date=20130125T202355Z
-- &x-amz-signedheaders=content-type;host;x-amz-date 2013-01-22T18:46:48.600Z
-- Medium Utilization 31536000 800.0 0.158 payment-pending dw.hs1.xlarge 1
-- 4357912c-9266-469d-beb0-0f1b775e1bc9 2013-01-22T20:09:16.630Z Heavy
-- Utilization 94608000 Hourly 0.21 12452.0 0.0 payment-pending dw.hs1.8xlarge
-- 2 93bbbca2-e88c-4b8b-a600-b64eaabf18a3 2013-01-23T21:49:32.517Z Medium
-- Utilization 31536000 800.0 0.158 payment-pending dw.hs1.xlarge 1
-- bbcd9749-f2ea-4d01-9b1b-b576f618eb4e 24dc90c8-672d-11e2-b2e1-8f41f0379151.
module Network.AWS.Redshift.DescribeReservedNodes where

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
describeReservedNodes :: AWS (Either RedshiftError DescribeReservedNodesResponse)
describeReservedNodes = undefined $ DescribeReservedNodes
    { drnmMarker = Nothing
    , drnmMaxRecords = Nothing
    , drnmReservedNodeId = Nothing
    }

data DescribeReservedNodes = DescribeReservedNodes
    { drnmMarker :: Maybe Text
      -- ^ An optional marker returned by a previous DescribeReservedNodes request to
      -- indicate the first parameter group that the current request will return.
    , drnmMaxRecords :: Maybe Int
      -- ^ The maximum number of records to include in the response. If more records
      -- exist than the specified MaxRecords value, a marker is included in the
      -- response so that the remaining results may be retrieved. Default: 100
      -- Constraints: minimum 20, maximum 100.
    , drnmReservedNodeId :: Maybe Text
      -- ^ Identifier for the node reservation.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeReservedNodes

instance AWSRequest DescribeReservedNodes where
    type Er DescribeReservedNodes = RedshiftError
    type Rs DescribeReservedNodes = DescribeReservedNodesResponse
    request = getQuery service "DescribeReservedNodes"

data DescribeReservedNodesResponse = DescribeReservedNodesResponse
    { drnmrsMarker :: Maybe Text
      -- ^ A marker that can be used to retrieve paginated results.
    , drnmrsReservedNodes :: [ReservedNode]
      -- ^ The list of reserved nodes.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeReservedNodesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeReservedNodesResponse"
        :| ["DescribeReservedNodesResult"]
