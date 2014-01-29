{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DescribeClusterVersions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns descriptions of the available Amazon Redshift cluster versions. You
-- can call this operation even before creating any clusters to learn more
-- about the Amazon Redshift versions. For more information about managing
-- clusters, go to Amazon Redshift Clusters in the Amazon Redshift Management
-- Guide https://redshift.us-east-1.amazonaws.com/
-- ?Action=DescribeClusterVersions &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20121207/us-east-1/redshift/aws4_request
-- &x-amz-date=20121207T230708Z
-- &x-amz-signedheaders=content-type;host;x-amz-date redshift-1.0 Initial
-- release of redshift 1.0 d39cd5e5-40c2-11e2-8a25-eb010998df4e.
module Network.AWS.Redshift.DescribeClusterVersions where

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
describeClusterVersions :: AWS (Either RedshiftError DescribeClusterVersionsResponse)
describeClusterVersions = undefined $ DescribeClusterVersions
    { dcvmClusterParameterGroupFamily = Nothing
    , dcvmClusterVersion = Nothing
    , dcvmMarker = Nothing
    , dcvmMaxRecords = Nothing
    }

data DescribeClusterVersions = DescribeClusterVersions
    { dcvmClusterParameterGroupFamily :: Maybe Text
      -- ^ The name of a specific cluster parameter group family to return details
      -- for. Constraints: Must be 1 to 255 alphanumeric characters First character
      -- must be a letter Cannot end with a hyphen or contain two consecutive
      -- hyphens.
    , dcvmClusterVersion :: Maybe Text
      -- ^ The specific cluster version to return. Example: 1.0.
    , dcvmMarker :: Maybe Text
      -- ^ The marker returned from a previous request. If this parameter is
      -- specified, the response includes records beyond the marker only, up to
      -- MaxRecords.
    , dcvmMaxRecords :: Maybe Int
      -- ^ The maximum number of records to include in the response. If more than the
      -- MaxRecords value is available, a marker is included in the response so that
      -- the following results can be retrieved. Default: 100 Constraints: Value
      -- must be at least 20 and no more than 100.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeClusterVersions

instance AWSRequest DescribeClusterVersions where
    type Er DescribeClusterVersions = RedshiftError
    type Rs DescribeClusterVersions = DescribeClusterVersionsResponse
    request = getQuery service "DescribeClusterVersions"

data DescribeClusterVersionsResponse = DescribeClusterVersionsResponse
    { dcvmrsClusterVersions :: [ClusterVersion]
      -- ^ A list of Version elements.
    , dcvmrsMarker :: Maybe Text
      -- ^ The identifier returned to allow retrieval of paginated results.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeClusterVersionsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeClusterVersionsResponse"
        :| ["DescribeClusterVersionsResult"]
