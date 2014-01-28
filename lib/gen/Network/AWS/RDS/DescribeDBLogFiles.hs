{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.DescribeDBLogFiles
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of DB log files for the DB instance.
-- https://rds.amazonaws.com/ ?DBInstanceIdentifier=rrak-mysql &MaxRecords=100
-- &Version=2013-02-12 &Action=DescribeDBLogFiles &SignatureVersion=4
-- &SignatureMethod=HmacSHA256 &Timestamp=20130327T173621Z
-- &X-Amz-Algorithm=AWS4-HMAC-SHA256 &X-Amz-Date=20130327T173621Z
-- &X-Amz-SignedHeaders=Host &X-Amz-Expires=20130327T173621Z
-- &X-Amz-Credential= &X-Amz-Signature= 1364403600000
-- error/mysql-error-running.log 0 1364338800000
-- error/mysql-error-running.log.0 0 1364342400000
-- error/mysql-error-running.log.1 0 1364346000000
-- error/mysql-error-running.log.2 0 1364349600000
-- error/mysql-error-running.log.3 0 1364405700000 error/mysql-error.log 0
-- d70fb3b3-9704-11e2-a0db-871552e0ef19.
module Network.AWS.RDS.DescribeDBLogFiles where

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

data DescribeDBLogFiles = DescribeDBLogFiles
    { ddblfmDBInstanceIdentifier :: !Text
      -- ^ The customer-assigned name of the DB instance that contains the log files
      -- you want to list. Constraints: Must contain from 1 to 63 alphanumeric
      -- characters or hyphens First character must be a letter Cannot end with a
      -- hyphen or contain two consecutive hyphens.
    , ddblfmFileLastWritten :: Maybe Integer
      -- ^ Filters the available log files for files written since the specified date,
      -- in POSIX timestamp format.
    , ddblfmFileSize :: Maybe Integer
      -- ^ Filters the available log files for files larger than the specified size.
    , ddblfmFilenameContains :: Maybe Text
      -- ^ Filters the available log files for log file names that contain the
      -- specified string.
    , ddblfmMarker :: Maybe Text
      -- ^ The pagination token provided in the previous request. If this parameter is
      -- specified the response includes only records beyond the marker, up to
      -- MaxRecords.
    , ddblfmMaxRecords :: Maybe Int
      -- ^ The maximum number of records to include in the response. If more records
      -- exist than the specified MaxRecords value, a pagination token called a
      -- marker is included in the response so that the remaining results can be
      -- retrieved.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeDBLogFiles

instance AWSRequest DescribeDBLogFiles where
    type Er DescribeDBLogFiles = RDSError
    type Rs DescribeDBLogFiles = DescribeDBLogFilesResponse
    request = getQuery service "DescribeDBLogFiles"

data DescribeDBLogFilesResponse = DescribeDBLogFilesResponse
    { ddblfmrsDescribeDBLogFiles :: [DescribeDBLogFilesDetails]
      -- ^ The DB log files returned.
    , ddblfmrsMarker :: Maybe Text
      -- ^ An optional paging token.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeDBLogFilesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeDBLogFilesResponse"
        :| ["DescribeDBLogFilesResult"]
