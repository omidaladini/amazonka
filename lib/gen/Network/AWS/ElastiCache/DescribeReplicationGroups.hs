{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.DescribeReplicationGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeReplicationGroups operation returns information about a
-- particular replication group. If no identifier is specified,
-- DescribeReplicationGroups returns information about all replication groups.
module Network.AWS.ElastiCache.DescribeReplicationGroups where

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

-- | Convenience method utilising default fields where applicable.
describeReplicationGroups :: AWS (Either ElastiCacheError DescribeReplicationGroupsResponse)
describeReplicationGroups = undefined $ DescribeReplicationGroups
    { drgnMarker = Nothing
    , drgnMaxRecords = Nothing
    , drgnReplicationGroupId = Nothing
    }

data DescribeReplicationGroups = DescribeReplicationGroups
    { drgnMarker :: Maybe Text
      -- ^ An optional marker returned from a prior request. Use this marker for
      -- pagination of results from this operation. If this parameter is specified,
      -- the response includes only records beyond the marker, up to the value
      -- specified by MaxRecords.
    , drgnMaxRecords :: Maybe Int
      -- ^ The maximum number of records to include in the response. If more records
      -- exist than the specified MaxRecords value, a marker is included in the
      -- response so that the remaining results can be retrieved. Default:
      -- 100Constraints: minimum 20; maximum 100.
    , drgnReplicationGroupId :: Maybe Text
      -- ^ The identifier for the replication group to be described. This parameter is
      -- not case sensitive. If you do not specify this parameter, information about
      -- all replication groups is returned.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeReplicationGroups

instance AWSRequest DescribeReplicationGroups where
    type Er DescribeReplicationGroups = ElastiCacheError
    type Rs DescribeReplicationGroups = DescribeReplicationGroupsResponse
    request = getQuery service "DescribeReplicationGroups"

data DescribeReplicationGroupsResponse = DescribeReplicationGroupsResponse
    { drgnrsMarker :: Maybe Text
      -- ^ Provides an identifier to allow retrieval of paginated results.
    , drgnrsReplicationGroups :: [ReplicationGroup]
      -- ^ A list of replication groups. Each item in the list contains detailed
      -- information about one replication group.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeReplicationGroupsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeReplicationGroupsResponse"
        :| ["DescribeReplicationGroupsResult"]
