{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.DescribeReservedDBInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about reserved DB instances for this account, or about
-- a specified reserved DB instance. https://rds.amazonaws.com/
-- ?Action=DescribeReservedDBInstances
-- &ReservedDBInstanceId=customerSpecifiedID &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2012-12-18T18%3A31%3A36.118Z
-- &AWSAccessKeyId= &Signature= Medium Utilization USD mysql
-- 649fd0c8-cf6d-47a0-bfa6-060f8e75e95f false active myreservationid 1
-- 2010-12-15T00:25:14.131Z 31536000 227.5 0.046 db.m1.small
-- c695119b-2961-11e1-bd06-6fe008f046c3.
module Network.AWS.RDS.DescribeReservedDBInstances where

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
describeReservedDBInstances :: AWS (Either RDSError DescribeReservedDBInstancesResponse)
describeReservedDBInstances = undefined $ DescribeReservedDBInstances
    { drdbimDBInstanceClass = Nothing
    , drdbimDuration = Nothing
    , drdbimMarker = Nothing
    , drdbimMaxRecords = Nothing
    , drdbimMultiAZ = Nothing
    , drdbimOfferingType = Nothing
    , drdbimProductDescription = Nothing
    , drdbimReservedDBInstanceId = Nothing
    , drdbimReservedDBInstancesOfferingId = Nothing
    }

data DescribeReservedDBInstances = DescribeReservedDBInstances
    { drdbimDBInstanceClass :: Maybe Text
      -- ^ The DB instance class filter value. Specify this parameter to show only
      -- those reservations matching the specified DB instances class.
    , drdbimDuration :: Maybe Text
      -- ^ The duration filter value, specified in years or seconds. Specify this
      -- parameter to show only reservations for this duration. Valid Values: 1 | 3
      -- | 31536000 | 94608000.
    , drdbimMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous request. If this
      -- parameter is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords.
    , drdbimMaxRecords :: Maybe Int
      -- ^ The maximum number of records to include in the response. If more than the
      -- MaxRecords value is available, a pagination token called a marker is
      -- included in the response so that the following results can be retrieved.
      -- Default: 100 Constraints: minimum 20, maximum 100.
    , drdbimMultiAZ :: Maybe Bool
      -- ^ The Multi-AZ filter value. Specify this parameter to show only those
      -- reservations matching the specified Multi-AZ parameter.
    , drdbimOfferingType :: Maybe Text
      -- ^ The offering type filter value. Specify this parameter to show only the
      -- available offerings matching the specified offering type. Valid Values:
      -- "Light Utilization" | "Medium Utilization" | "Heavy Utilization".
    , drdbimProductDescription :: Maybe Text
      -- ^ The product description filter value. Specify this parameter to show only
      -- those reservations matching the specified product description.
    , drdbimReservedDBInstanceId :: Maybe Text
      -- ^ The reserved DB instance identifier filter value. Specify this parameter to
      -- show only the reservation that matches the specified reservation ID.
    , drdbimReservedDBInstancesOfferingId :: Maybe Text
      -- ^ The offering identifier filter value. Specify this parameter to show only
      -- purchased reservations matching the specified offering identifier.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeReservedDBInstances

instance AWSRequest DescribeReservedDBInstances where
    type Er DescribeReservedDBInstances = RDSError
    type Rs DescribeReservedDBInstances = DescribeReservedDBInstancesResponse
    request = getQuery service "DescribeReservedDBInstances"

instance AWSPager DescribeReservedDBInstances where
    next rq rs
        | Just x <- drdbimrsMarker rs = Just $ rq { drdbimMarker = Just x }
        | otherwise = Nothing

data DescribeReservedDBInstancesResponse = DescribeReservedDBInstancesResponse
    { drdbimrsMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous request. If this
      -- parameter is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords.
    , drdbimrsReservedDBInstances :: [ReservedDBInstance]
      -- ^ A list of reserved DB instances.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeReservedDBInstancesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeReservedDBInstancesResponse"
        :| ["DescribeReservedDBInstancesResult"]
