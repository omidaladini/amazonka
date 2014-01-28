{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.DescribeEvents
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeEvents operation returns events related to cache clusters,
-- cache security groups, and cache parameter groups. You can obtain events
-- specific to a particular cache cluster, cache security group, or cache
-- parameter group by providing the name as a parameter. By default, only the
-- events occurring within the last hour are returned; however, you can
-- retrieve up to 14 days' worth of events if necessary.
module Network.AWS.ElastiCache.DescribeEvents where

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

import Network.AWS.ElastiCache.Service
import Network.AWS.ElastiCache.Types

data DescribeEvents = DescribeEvents
    { demDuration :: Maybe Int
      -- ^ The number of minutes' worth of events to retrieve.
    , demEndTime :: Maybe UTCTime
      -- ^ The end of the time interval for which to retrieve events, specified in ISO
      -- 8601 format.
    , demMarker :: Maybe Text
      -- ^ An optional marker returned from a prior request. Use this marker for
      -- pagination of results from this operation. If this parameter is specified,
      -- the response includes only records beyond the marker, up to the value
      -- specified by MaxRecords.
    , demMaxRecords :: Maybe Int
      -- ^ The maximum number of records to include in the response. If more records
      -- exist than the specified MaxRecords value, a marker is included in the
      -- response so that the remaining results can be retrieved. Default:
      -- 100Constraints: minimum 20; maximum 100.
    , demSourceIdentifier :: Maybe Text
      -- ^ The identifier of the event source for which events will be returned. If
      -- not specified, then all sources are included in the response.
    , demSourceType :: Maybe SourceType
      -- ^ The event source to retrieve events for. If no value is specified, all
      -- events are returned. Valid values are: cache-cluster |
      -- cache-parameter-group | cache-security-group | cache-subnet-group.
    , demStartTime :: Maybe UTCTime
      -- ^ The beginning of the time interval to retrieve events for, specified in ISO
      -- 8601 format.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeEvents

instance AWSRequest DescribeEvents where
    type Er DescribeEvents = ElastiCacheError
    type Rs DescribeEvents = DescribeEventsResponse
    request = getQuery service "DescribeEvents"

instance AWSPager DescribeEvents where
    next rq rs
        | Just x <- demrsMarker rs = Just $ rq { demMarker = Just x }
        | otherwise = Nothing

data DescribeEventsResponse = DescribeEventsResponse
    { demrsEvents :: [Event]
      -- ^ A list of events. Each element in the list contains detailed information
      -- about one event.
    , demrsMarker :: Maybe Text
      -- ^ Provides an identifier to allow retrieval of paginated results.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeEventsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeEventsResponse"
        :| ["DescribeEventsResult"]
