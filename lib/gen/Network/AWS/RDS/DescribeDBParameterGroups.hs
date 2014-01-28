{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.DescribeDBParameterGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of DBParameterGroup descriptions. If a DBParameterGroupName
-- is specified, the list will contain only the description of the specified
-- DB parameter group. https://rds.amazonaws.com/
-- ?Action=DescribeDBParameterGroups &DBParameterGroupName=myparamsgroup
-- &MaxRecords=100 &Version=2013-05-15 &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2011-02-15T17%3A54%3A32.899Z
-- &AWSAccessKeyId= &Signature= mysql5.1 Default parameter group for mysql5.1
-- default.mysql5.1 mysql5.1 My DB Param Group testdbparamgroup
-- cb8d9bb4-a02a-11df-bd60-c955b7d6e8e0.
module Network.AWS.RDS.DescribeDBParameterGroups where

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

data DescribeDBParameterGroups = DescribeDBParameterGroups
    { ddbpgmDBParameterGroupName :: Maybe Text
      -- ^ The name of a specific DB parameter group to return details for.
      -- Constraints: Must be 1 to 255 alphanumeric characters First character must
      -- be a letter Cannot end with a hyphen or contain two consecutive hyphens.
    , ddbpgmMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous
      -- DescribeDBParameterGroups request. If this parameter is specified, the
      -- response includes only records beyond the marker, up to the value specified
      -- by MaxRecords.
    , ddbpgmMaxRecords :: Maybe Int
      -- ^ The maximum number of records to include in the response. If more records
      -- exist than the specified MaxRecords value, a pagination token called a
      -- marker is included in the response so that the remaining results may be
      -- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeDBParameterGroups

instance AWSRequest DescribeDBParameterGroups where
    type Er DescribeDBParameterGroups = RDSError
    type Rs DescribeDBParameterGroups = DescribeDBParameterGroupsResponse
    request = getQuery service "DescribeDBParameterGroups"

instance AWSPager DescribeDBParameterGroups where
    next rq rs
        | Just x <- ddbpgmrsMarker rs = Just $ rq { ddbpgmMarker = Just x }
        | otherwise = Nothing

data DescribeDBParameterGroupsResponse = DescribeDBParameterGroupsResponse
    { ddbpgmrsDBParameterGroups :: [DBParameterGroup]
      -- ^ A list of DBParameterGroup instances.
    , ddbpgmrsMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous request. If this
      -- parameter is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeDBParameterGroupsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeDBParameterGroupsResponse"
        :| ["DescribeDBParameterGroupsResult"]
