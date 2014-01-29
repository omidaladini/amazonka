{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DescribeOrderableClusterOptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of orderable cluster options. Before you create a new
-- cluster you can use this operation to find what options are available, such
-- as the EC2 Availability Zones (AZ) in the specific AWS region that you can
-- specify, and the node types you can request. The node types differ by
-- available storage, memory, CPU and price. With the cost involved you might
-- want to obtain a list of cluster options in the specific region and specify
-- values when creating a cluster. For more information about managing
-- clusters, go to Amazon Redshift Clusters in the Amazon Redshift Management
-- Guide https://redshift.us-east-1.amazonaws.com/
-- ?Action=DescribeOrderableClusterOptions &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20121207/us-east-1/redshift/aws4_request
-- &x-amz-date=20121207T225314Z
-- &x-amz-signedheaders=content-type;host;x-amz-date 1.0 multi-node
-- dw.hs1.8xlarge us-east-1a us-east-1c us-east-1d 1.0 multi-node
-- dw.hs1.xlarge us-east-1a us-east-1c us-east-1d 1.0 single-node
-- dw.hs1.xlarge us-east-1a us-east-1c us-east-1d
-- e37414cc-40c0-11e2-b6a0-df98b1a86860.
module Network.AWS.Redshift.DescribeOrderableClusterOptions where

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
describeOrderableClusterOptions :: AWS (Either RedshiftError DescribeOrderableClusterOptionsResponse)
describeOrderableClusterOptions = undefined $ DescribeOrderableClusterOptions
    { docomClusterVersion = Nothing
    , docomMarker = Nothing
    , docomMaxRecords = Nothing
    , docomNodeType = Nothing
    }

data DescribeOrderableClusterOptions = DescribeOrderableClusterOptions
    { docomClusterVersion :: Maybe Text
      -- ^ The version filter value. Specify this parameter to show only the available
      -- offerings matching the specified version. Default: All versions.
      -- Constraints: Must be one of the version returned from
      -- DescribeClusterVersions.
    , docomMarker :: Maybe Text
      -- ^ An optional marker returned from a previous DescribeOrderableClusterOptions
      -- request. If this parameter is specified, the response includes only records
      -- beyond the marker, up to the value specified by MaxRecords.
    , docomMaxRecords :: Maybe Int
      -- ^ The maximum number of records to include in the response. If more records
      -- exist than the specified MaxRecords value, a marker is included in the
      -- response so that the remaining results may be retrieved. Default: 100
      -- Constraints: minimum 20, maximum 100.
    , docomNodeType :: Maybe Text
      -- ^ The node type filter value. Specify this parameter to show only the
      -- available offerings matching the specified node type.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeOrderableClusterOptions

instance AWSRequest DescribeOrderableClusterOptions where
    type Er DescribeOrderableClusterOptions = RedshiftError
    type Rs DescribeOrderableClusterOptions = DescribeOrderableClusterOptionsResponse
    request = getQuery service "DescribeOrderableClusterOptions"

data DescribeOrderableClusterOptionsResponse = DescribeOrderableClusterOptionsResponse
    { docomrsMarker :: Maybe Text
      -- ^ A marker that can be used to retrieve paginated results.
    , docomrsOrderableClusterOptions :: [OrderableClusterOption]
      -- ^ An OrderableClusterOption structure containing information about orderable
      -- options for the Cluster.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeOrderableClusterOptionsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeOrderableClusterOptionsResponse"
        :| ["DescribeOrderableClusterOptionsResult"]
