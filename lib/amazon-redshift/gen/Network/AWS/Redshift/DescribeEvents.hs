{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DescribeEvents
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns events related to clusters, security groups, snapshots, and
-- parameter groups for the past 14 days. Events specific to a particular
-- cluster, security group, snapshot or parameter group can be obtained by
-- providing the name as a parameter. By default, the past hour of events are
-- returned. https://redshift.us-east-1.amazonaws.com/ ?Action=DescribeEvents
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20121207/us-east-1/redshift/aws4_request
-- &x-amz-date=20121207T232427Z
-- &x-amz-signedheaders=content-type;host;x-amz-date Cluster security group
-- securitygroup1 has been updated. Changes need to be applied to all clusters
-- using this cluster security group. cluster-security-group
-- 2012-12-07T23:05:02.660Z securitygroup1
-- 3eeb9efe-40c5-11e2-816a-1bba29fad1f5.
module Network.AWS.Redshift.DescribeEvents where

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
describeEvents :: DescribeEvents
describeEvents = DescribeEvents
    { demDuration = Nothing
    , demEndTime = Nothing
    , demMarker = Nothing
    , demMaxRecords = Nothing
    , demSourceIdentifier = Nothing
    , demSourceType = Nothing
    , demStartTime = Nothing
    }

data DescribeEvents = DescribeEvents
    { demDuration :: Maybe Int
      -- ^ The number of minutes prior to the time of the request for which to
      -- retrieve events. For example, if the request is sent at 18:00 and you
      -- specify a duration of 60, then only events which have occurred after 17:00
      -- will be returned. Default: 60.
    , demEndTime :: Maybe UTCTime
      -- ^ The end of the time interval for which to retrieve events, specified in ISO
      -- 8601 format. For more information about ISO 8601, go to the ISO8601
      -- Wikipedia page. Example: 2009-07-08T18:00Z.
    , demMarker :: Maybe Text
      -- ^ An optional marker returned from a previous DescribeEvents request. If this
      -- parameter is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords.
    , demMaxRecords :: Maybe Int
      -- ^ The maximum number of records to include in the response. If more records
      -- exist than the specified MaxRecords value, a marker is included in the
      -- response so that the remaining results may be retrieved. Default: 100
      -- Constraints: Value must be at least 20 and no more than 100.
    , demSourceIdentifier :: Maybe Text
      -- ^ The identifier of the event source for which events will be returned. If
      -- this parameter is not specified, then all sources are included in the
      -- response. Constraints: If SourceIdentifier is supplied, SourceType must
      -- also be provided. Specify a cluster identifier when SourceType is cluster.
      -- Specify a cluster security group name when SourceType is
      -- cluster-security-group. Specify a cluster parameter group name when
      -- SourceType is cluster-parameter-group. Specify a cluster snapshot
      -- identifier when SourceType is cluster-snapshot.
    , demSourceType :: Maybe SourceType
      -- ^ The event source to retrieve events for. If no value is specified, all
      -- events are returned. Constraints: If SourceType is supplied,
      -- SourceIdentifier must also be provided. Specify cluster when
      -- SourceIdentifier is a cluster identifier. Specify cluster-security-group
      -- when SourceIdentifier is a cluster security group name. Specify
      -- cluster-parameter-group when SourceIdentifier is a cluster parameter group
      -- name. Specify cluster-snapshot when SourceIdentifier is a cluster snapshot
      -- identifier.
    , demStartTime :: Maybe UTCTime
      -- ^ The beginning of the time interval to retrieve events for, specified in ISO
      -- 8601 format. For more information about ISO 8601, go to the ISO8601
      -- Wikipedia page. Example: 2009-07-08T18:00Z.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeEvents

instance AWSRequest DescribeEvents where
    type Er DescribeEvents = RedshiftError
    type Rs DescribeEvents = DescribeEventsResponse
    request = getQuery service "DescribeEvents"

data DescribeEventsResponse = DescribeEventsResponse
    { demrsEvents :: [Event]
      -- ^ A list of Event instances.
    , demrsMarker :: Maybe Text
      -- ^ A marker at which to continue listing events in a new request. The response
      -- returns a marker if there are more events to list than returned in the
      -- response.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeEventsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeEventsResponse"
        :| ["DescribeEventsResult"]
