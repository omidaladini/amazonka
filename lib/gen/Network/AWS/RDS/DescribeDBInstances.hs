{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.DescribeDBInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about provisioned RDS instances. This API supports
-- pagination. https://rds.amazonaws.com/ ?Action=DescribeDBInstances
-- &Version=2013-05-15 &MaxRecords=100 &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2011-05-23T06%3A54%3A55.116Z
-- &AWSAccessKeyId= &Signature= 2011-05-23T06:50:00Z mysql 1 false
-- general-public-license available 5.1.50 3306
-- simcoprod01.cu7u2t4uz396.us-east-1.rds.amazonaws.com simcoprod01 in-sync
-- default.mysql5.1 active default 00:00-00:30 true sat:07:30-sat:08:00
-- us-east-1a 2011-05-23T06:06:43.110Z 10 default.mysql5.1 in-sync db.m1.large
-- master 9135fff3-8509-11e0-bd9b-a7b1ece36d51.
module Network.AWS.RDS.DescribeDBInstances where

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

data DescribeDBInstances = DescribeDBInstances
    { ddbinDBInstanceIdentifier :: Maybe Text
      -- ^ The user-supplied instance identifier. If this parameter is specified,
      -- information from only the specific DB instance is returned. This parameter
      -- isn't case sensitive. Constraints: Must contain from 1 to 63 alphanumeric
      -- characters or hyphens First character must be a letter Cannot end with a
      -- hyphen or contain two consecutive hyphens.
    , ddbinMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous DescribeDBInstances
      -- request. If this parameter is specified, the response includes only records
      -- beyond the marker, up to the value specified by MaxRecords .
    , ddbinMaxRecords :: Maybe Int
      -- ^ The maximum number of records to include in the response. If more records
      -- exist than the specified MaxRecords value, a pagination token called a
      -- marker is included in the response so that the remaining results may be
      -- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeDBInstances

instance AWSRequest DescribeDBInstances where
    type Er DescribeDBInstances = RDSError
    type Rs DescribeDBInstances = DescribeDBInstancesResponse
    request = getQuery service "DescribeDBInstances"

instance AWSPager DescribeDBInstances where
    next rq rs
        | Just x <- ddbinrsMarker rs = Just $ rq { ddbinMarker = Just x }
        | otherwise = Nothing

data DescribeDBInstancesResponse = DescribeDBInstancesResponse
    { ddbinrsDBInstances :: [DBInstance]
      -- ^ A list of DBInstance instances.
    , ddbinrsMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous request. If this
      -- parameter is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords .
    } deriving (Eq, Show, Generic)

instance FromXML DescribeDBInstancesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeDBInstancesResponse"
        :| ["DescribeDBInstancesResult"]
