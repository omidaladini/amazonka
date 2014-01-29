{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.DownloadDBLogFilePortion
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Downloads the last line of the specified log file.
-- https://rds.amazonaws.com/ ?DBInstanceIdentifier=rra-mysql &MaxRecords=100
-- &Version=2013-05-15 &Action=DescribeDBLogFiles &SignatureVersion=4
-- &SignatureMethod=HmacSHA256 &Timestamp=20130327T173621Z
-- &X-Amz-Algorithm=AWS4-HMAC-SHA256 &X-Amz-Date=20130327T173621Z
-- &X-Amz-SignedHeaders=Host &X-Amz-Expires=20130327T173621Z
-- &X-Amz-Credential= &X-Amz-Signature= 1364403600000
-- error/mysql-error-running.log 0 1364338800000
-- error/mysql-error-running.log.0 0 1364342400000
-- error/mysql-error-running.log.1 0 1364371200000
-- error/mysql-error-running.log.9 0 1364405700000 error/mysql-error.log 0
-- d70fb3b3-9704-11e2-a0db-871552e0ef19.
module Network.AWS.RDS.DownloadDBLogFilePortion where

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
downloadDBLogFilePortion :: Text
                         -> Text
                         -> AWS (Either RDSError DownloadDBLogFilePortionResponse)
downloadDBLogFilePortion p1 p2 = undefined $ DownloadDBLogFilePortion
    { ddblfpmDBInstanceIdentifier = p1
    , ddblfpmLogFileName = p2
    , ddblfpmMarker = Nothing
    , ddblfpmNumberOfLines = Nothing
    }

data DownloadDBLogFilePortion = DownloadDBLogFilePortion
    { ddblfpmDBInstanceIdentifier :: !Text
      -- ^ The customer-assigned name of the DB instance that contains the log files
      -- you want to list. Constraints: Must contain from 1 to 63 alphanumeric
      -- characters or hyphens First character must be a letter Cannot end with a
      -- hyphen or contain two consecutive hyphens.
    , ddblfpmLogFileName :: !Text
      -- ^ The name of the log file to be downloaded.
    , ddblfpmMarker :: Maybe Text
      -- ^ The pagination token provided in the previous request. If this parameter is
      -- specified the response includes only records beyond the marker, up to
      -- MaxRecords.
    , ddblfpmNumberOfLines :: Maybe Int
      -- ^ The number of lines remaining to be downloaded.
    } deriving (Eq, Show, Generic)

instance ToQuery DownloadDBLogFilePortion

instance AWSRequest DownloadDBLogFilePortion where
    type Er DownloadDBLogFilePortion = RDSError
    type Rs DownloadDBLogFilePortion = DownloadDBLogFilePortionResponse
    request = getQuery service "DownloadDBLogFilePortion"

data DownloadDBLogFilePortionResponse = DownloadDBLogFilePortionResponse
    { ddblfpmrsAdditionalDataPending :: Maybe Bool
      -- ^ Boolean value that if true, indicates there is more data to be downloaded.
    , ddblfpmrsLogFileData :: Maybe Text
      -- ^ Entries from the specified log file.
    , ddblfpmrsMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous
      -- DownloadDBLogFilePortion request.
    } deriving (Eq, Show, Generic)

instance FromXML DownloadDBLogFilePortionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DownloadDBLogFilePortionResponse"
        :| ["DownloadDBLogFilePortionResult"]
