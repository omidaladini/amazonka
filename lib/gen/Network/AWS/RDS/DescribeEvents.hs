{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.DescribeEvents
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns events related to DB instances, DB security groups, DB snapshots,
-- and DB parameter groups for the past 14 days. Events specific to a
-- particular DB instance, DB security group, database snapshot, or DB
-- parameter group can be obtained by providing the name as a parameter. By
-- default, the past hour of events are returned. https://rds.amazonaws.com/
-- ?Action=DescribeEvents &Duration=1440 &MaxRecords=100 &Version=2013-05-15
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-02-15T20%3A00%3A44.420Z &AWSAccessKeyId= &Signature=
-- Applied change to security group db-security-group 2010-08-11T17:12:52.860Z
-- mydbsecuritygroup Database instance created db-instance
-- 2010-08-11T18:10:15.269Z mydbinstance3 Backing up database instance
-- db-instance 2010-08-11T18:10:34.690Z mydbinstance3 Backing up DB instance
-- db-instance 2010-08-11T18:25:52.263Z mynewdbinstance Creating user snapshot
-- db-snapshot 2010-08-11T18:25:52.263Z mynewdbsnapshot3
-- 95b948cd-bf45-11de-86a4-97241dfaadff.
module Network.AWS.RDS.DescribeEvents where

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

import Network.AWS.RDS.Service
import Network.AWS.RDS.Types

data DescribeEvents = DescribeEvents
    { demDuration :: Maybe Int
      -- ^ The number of minutes to retrieve events for. Default: 60.
    , demEndTime :: Maybe UTCTime
      -- ^ The end of the time interval for which to retrieve events, specified in ISO
      -- 8601 format. For more information about ISO 8601, go to the ISO8601
      -- Wikipedia page. Example: 2009-07-08T18:00Z.
    , demEventCategories :: [Text]
      -- ^ A list of event categories that trigger notifications for a event
      -- notification subscription.
    , demMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous DescribeEvents request.
      -- If this parameter is specified, the response includes only records beyond
      -- the marker, up to the value specified by MaxRecords.
    , demMaxRecords :: Maybe Int
      -- ^ The maximum number of records to include in the response. If more records
      -- exist than the specified MaxRecords value, a pagination token called a
      -- marker is included in the response so that the remaining results may be
      -- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
    , demSourceIdentifier :: Maybe Text
      -- ^ The identifier of the event source for which events will be returned. If
      -- not specified, then all sources are included in the response. Constraints:
      -- If SourceIdentifier is supplied, SourceType must also be provided. If the
      -- source type is DBInstance, then a DBInstanceIdentifier must be supplied. If
      -- the source type is DBSecurityGroup, a DBSecurityGroupName must be supplied.
      -- If the source type is DBParameterGroup, a DBParameterGroupName must be
      -- supplied. If the source type is DBSnapshot, a DBSnapshotIdentifier must be
      -- supplied. Cannot end with a hyphen or contain two consecutive hyphens.
    , demSourceType :: Maybe SourceType
      -- ^ The event source to retrieve events for. If no value is specified, all
      -- events are returned.
    , demStartTime :: Maybe UTCTime
      -- ^ The beginning of the time interval to retrieve events for, specified in ISO
      -- 8601 format. For more information about ISO 8601, go to the ISO8601
      -- Wikipedia page. Example: 2009-07-08T18:00Z.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeEvents

instance AWSRequest DescribeEvents where
    type Er DescribeEvents = RDSError
    type Rs DescribeEvents = DescribeEventsResponse
    request = getQuery service "DescribeEvents"

instance AWSPager DescribeEvents where
    next rq rs
        | Just x <- demrsMarker rs = Just $ rq { demMarker = Just x }
        | otherwise = Nothing

data DescribeEventsResponse = DescribeEventsResponse
    { demrsEvents :: [Event]
      -- ^ A list of Event instances.
    , demrsMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous Events request. If this
      -- parameter is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords .
    } deriving (Eq, Show, Generic)

instance FromXML DescribeEventsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeEventsResponse"
        :| ["DescribeEventsResult"]
