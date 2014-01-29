{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DescribeClusterSnapshots
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns one or more snapshot objects, which contain metadata about your
-- cluster snapshots. By default, this operation returns information about all
-- snapshots of all clusters that are owned by you AWS customer account. No
-- information is returned for snapshots owned by inactive AWS customer
-- accounts. https://redshift.us-east-1.amazonaws.com/
-- ?Action=DescribeClusterSnapshots &ClusterIdentifier=examplecluster
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T011512Z
-- &x-amz-signedheaders=content-type;host;x-amz-date 5439
-- cm:examplecluster-2013-01-22-19-27-58 available automated 1.0
-- 2013-01-22T19:27:58.931Z 2 dev 2013-01-22T19:23:59.368Z us-east-1c
-- dw.hs1.xlarge examplecluster adminuser 5439 my-snapshot-123 available
-- manual 1.0 2013-01-23T01:09:03.149Z 2 dev 2013-01-22T19:23:59.368Z
-- us-east-1c dw.hs1.xlarge examplecluster adminuser
-- 56a9daf4-64fa-11e2-a8da-655adc216806.
module Network.AWS.Redshift.DescribeClusterSnapshots where

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
describeClusterSnapshots :: DescribeClusterSnapshots
describeClusterSnapshots = DescribeClusterSnapshots
    { dcsnClusterIdentifier = Nothing
    , dcsnEndTime = Nothing
    , dcsnMarker = Nothing
    , dcsnMaxRecords = Nothing
    , dcsnOwnerAccount = Nothing
    , dcsnSnapshotIdentifier = Nothing
    , dcsnSnapshotType = Nothing
    , dcsnStartTime = Nothing
    }

data DescribeClusterSnapshots = DescribeClusterSnapshots
    { dcsnClusterIdentifier :: Maybe Text
      -- ^ The identifier of the cluster for which information about snapshots is
      -- requested.
    , dcsnEndTime :: Maybe UTCTime
      -- ^ A time value that requests only snapshots created at or before the
      -- specified time. The time value is specified in ISO 8601 format. For more
      -- information about ISO 8601, go to the ISO8601 Wikipedia page. Example:
      -- 2012-07-16T18:00:00Z.
    , dcsnMarker :: Maybe Text
      -- ^ An optional marker returned by a previous DescribeClusterSnapshots request
      -- to indicate the first snapshot that the request will return.
    , dcsnMaxRecords :: Maybe Int
      -- ^ The maximum number of snapshot records to include in the response. If more
      -- records exist than the specified MaxRecords value, the response returns a
      -- marker that you can use in a subsequent DescribeClusterSnapshots request in
      -- order to retrieve the next set of snapshot records. Default: 100
      -- Constraints: Must be at least 20 and no more than 100.
    , dcsnOwnerAccount :: Maybe Text
      -- ^ The AWS customer account used to create or copy the snapshot. Use this
      -- field to filter the results to snapshots owned by a particular account. To
      -- describe snapshots you own, either specify your AWS customer account, or do
      -- not specify the parameter.
    , dcsnSnapshotIdentifier :: Maybe Text
      -- ^ The snapshot identifier of the snapshot about which to return information.
    , dcsnSnapshotType :: Maybe Text
      -- ^ The type of snapshots for which you are requesting information. By default,
      -- snapshots of all types are returned. Valid Values: automated | manual.
    , dcsnStartTime :: Maybe UTCTime
      -- ^ A value that requests only snapshots created at or after the specified
      -- time. The time value is specified in ISO 8601 format. For more information
      -- about ISO 8601, go to the ISO8601 Wikipedia page. Example:
      -- 2012-07-16T18:00:00Z.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeClusterSnapshots

instance AWSRequest DescribeClusterSnapshots where
    type Er DescribeClusterSnapshots = RedshiftError
    type Rs DescribeClusterSnapshots = DescribeClusterSnapshotsResponse
    request = getQuery service "DescribeClusterSnapshots"

data DescribeClusterSnapshotsResponse = DescribeClusterSnapshotsResponse
    { dcsnrsMarker :: Maybe Text
      -- ^ A marker that indicates the first snapshot that a subsequent
      -- DescribeClusterSnapshots request will return. The response returns a marker
      -- only if there are more snapshots to list than the current response can
      -- return.
    , dcsnrsSnapshots :: [Snapshot]
      -- ^ A list of Snapshot instances.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeClusterSnapshotsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeClusterSnapshotsResponse"
        :| ["DescribeClusterSnapshotsResult"]
