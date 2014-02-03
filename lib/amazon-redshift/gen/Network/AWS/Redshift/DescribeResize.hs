{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DescribeResize
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about the last resize operation for the specified
-- cluster. If no resize operation has ever been initiated for the specified
-- cluster, a HTTP 404 error is returned. If a resize operation was initiated
-- and completed, the status of the resize remains as SUCCEEDED until the next
-- resize. A resize operation can be requested using ModifyCluster and
-- specifying a different number or type of nodes for the cluster.
-- https://redshift.us-east-1.amazonaws.com/ ?Action=DescribeResize
-- &ClusterIdentifier=examplecluster &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20121207/us-east-1/redshift/aws4_request
-- &x-amz-date=20121207T232427Z
-- &x-amz-signedheaders=content-type;host;x-amz-date multi-node SUCCEEDED
-- users venue sales listing event date category dw.hs1.xlarge 2
-- a6d59c61-a162-11e2-b2bc-fb54c9d11e09.
module Network.AWS.Redshift.DescribeResize where

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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
describeResize :: Text
               -> DescribeResize
describeResize p1 = DescribeResize
    { drmClusterIdentifier = p1
    }

data DescribeResize = DescribeResize
    { drmClusterIdentifier :: !Text
      -- ^ The unique identifier of a cluster whose resize progress you are
      -- requesting. This parameter isn't case-sensitive. By default, resize
      -- operations for all clusters defined for an AWS account are returned.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeResize

instance AWSRequest DescribeResize where
    type Er DescribeResize = RedshiftError
    type Rs DescribeResize = DescribeResizeResponse
    request = getQuery service "DescribeResize"

data DescribeResizeResponse = DescribeResizeResponse
    { drmrsImportTablesCompleted :: [Text]
      -- ^ The names of tables that have been completely imported . Valid Values: List
      -- of table names.
    , drmrsImportTablesInProgress :: [Text]
      -- ^ The names of tables that are being currently imported. Valid Values: List
      -- of table names.
    , drmrsImportTablesNotStarted :: [Text]
      -- ^ The names of tables that have not been yet imported. Valid Values: List of
      -- table names.
    , drmrsStatus :: Maybe Text
      -- ^ The status of the resize operation. Valid Values: NONE | IN_PROGRESS |
      -- FAILED | SUCCEEDED.
    , drmrsTargetClusterType :: Maybe Text
      -- ^ The cluster type after the resize is complete. Valid Values: multi-node |
      -- single-node.
    , drmrsTargetNodeType :: Maybe Text
      -- ^ The node type that the cluster will have after the resize is complete.
    , drmrsTargetNumberOfNodes :: Maybe Int
      -- ^ The number of nodes that the cluster will have after the resize is
      -- complete.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeResizeResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeResizeResponse"
        :| ["DescribeResizeResult"]
