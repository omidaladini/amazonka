{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.DescribeOptionGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the available option groups. https://rds.amazonaws.com/
-- ?Action=DescribeOptionGroups &OptionGroupName=myoptiongroup &MaxRecords=100
-- 11.2 myoptiongroup oracle-se1 Test option group
-- 6088823d-84c8-11e1-a264-0b23c28bc344 https://rds.amazonaws.com/
-- ?Action=DescribeOptionGroups &MaxRecords=100 11.2 myoptiongroup oracle-se1
-- Test option group 11.2 default:oracle-se1-11-2 oracle-se1 Default option
-- group. e4b234d9-84d5-11e1-87a6-71059839a52b.
module Network.AWS.RDS.DescribeOptionGroups where

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

-- | Convenience method utilising default fields where applicable.
describeOptionGroups :: AWS (Either RDSError DescribeOptionGroupsResponse)
describeOptionGroups = undefined $ DescribeOptionGroups
    { dogmEngineName = Nothing
    , dogmMajorEngineVersion = Nothing
    , dogmMarker = Nothing
    , dogmMaxRecords = Nothing
    , dogmOptionGroupName = Nothing
    }

data DescribeOptionGroups = DescribeOptionGroups
    { dogmEngineName :: Maybe Text
      -- ^ Filters the list of option groups to only include groups associated with a
      -- specific database engine.
    , dogmMajorEngineVersion :: Maybe Text
      -- ^ Filters the list of option groups to only include groups associated with a
      -- specific database engine version. If specified, then EngineName must also
      -- be specified.
    , dogmMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous DescribeOptionGroups
      -- request. If this parameter is specified, the response includes only records
      -- beyond the marker, up to the value specified by MaxRecords.
    , dogmMaxRecords :: Maybe Int
      -- ^ The maximum number of records to include in the response. If more records
      -- exist than the specified MaxRecords value, a pagination token called a
      -- marker is included in the response so that the remaining results can be
      -- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
    , dogmOptionGroupName :: Maybe Text
      -- ^ The name of the option group to describe. Cannot be supplied together with
      -- EngineName or MajorEngineVersion.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeOptionGroups

instance AWSRequest DescribeOptionGroups where
    type Er DescribeOptionGroups = RDSError
    type Rs DescribeOptionGroups = DescribeOptionGroupsResponse
    request = getQuery service "DescribeOptionGroups"

instance AWSPager DescribeOptionGroups where
    next rq rs
        | Just x <- dogmrsMarker rs = Just $ rq { dogmMarker = Just x }
        | otherwise = Nothing

data DescribeOptionGroupsResponse = DescribeOptionGroupsResponse
    { dogmrsMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous request. If this
      -- parameter is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords.
    , dogmrsOptionGroupsList :: [OptionGroup]
      -- ^ List of option groups.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeOptionGroupsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeOptionGroupsResponse"
        :| ["DescribeOptionGroupsResult"]
