{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.DownloadDBLogFilePortion
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
module Network.AWS.RDS.V2013_09_09.DownloadDBLogFilePortion
    (
    -- * Request
      DownloadDBLogFilePortion
    -- ** Request constructor
    , downloadDBLogFilePortion
    -- ** Request lenses
    , ddblfpmDBInstanceIdentifier
    , ddblfpmLogFileName
    , ddblfpmNumberOfLines
    , ddblfpmMarker

    -- * Response
    , DownloadDBLogFilePortionResponse
    -- ** Response lenses
    , ddblfpdAdditionalDataPending
    , ddblfpdLogFileData
    , ddblfpdMarker
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DownloadDBLogFilePortion' request.
downloadDBLogFilePortion :: Text -- ^ 'ddblfpmDBInstanceIdentifier'
                         -> Text -- ^ 'ddblfpmLogFileName'
                         -> DownloadDBLogFilePortion
downloadDBLogFilePortion p1 p2 = DownloadDBLogFilePortion
    { _ddblfpmDBInstanceIdentifier = p1
    , _ddblfpmLogFileName = p2
    , _ddblfpmNumberOfLines = Nothing
    , _ddblfpmMarker = Nothing
    }

data DownloadDBLogFilePortion = DownloadDBLogFilePortion
    { _ddblfpmDBInstanceIdentifier :: Text
      -- ^ The customer-assigned name of the DB instance that contains the
      -- log files you want to list. Constraints: Must contain from 1 to
      -- 63 alphanumeric characters or hyphens First character must be a
      -- letter Cannot end with a hyphen or contain two consecutive
      -- hyphens.
    , _ddblfpmLogFileName :: Text
      -- ^ The name of the log file to be downloaded.
    , _ddblfpmNumberOfLines :: Maybe Integer
      -- ^ The number of lines remaining to be downloaded.
    , _ddblfpmMarker :: Maybe Text
      -- ^ The pagination token provided in the previous request. If this
      -- parameter is specified the response includes only records beyond
      -- the marker, up to MaxRecords.
    } deriving (Show, Generic)

-- | The customer-assigned name of the DB instance that contains the log files
-- you want to list. Constraints: Must contain from 1 to 63 alphanumeric
-- characters or hyphens First character must be a letter Cannot end with a
-- hyphen or contain two consecutive hyphens.
ddblfpmDBInstanceIdentifier
    :: Functor f
    => (Text
    -> f (Text))
    -> DownloadDBLogFilePortion
    -> f DownloadDBLogFilePortion
ddblfpmDBInstanceIdentifier f x =
    (\y -> x { _ddblfpmDBInstanceIdentifier = y })
       <$> f (_ddblfpmDBInstanceIdentifier x)
{-# INLINE ddblfpmDBInstanceIdentifier #-}

-- | The name of the log file to be downloaded.
ddblfpmLogFileName
    :: Functor f
    => (Text
    -> f (Text))
    -> DownloadDBLogFilePortion
    -> f DownloadDBLogFilePortion
ddblfpmLogFileName f x =
    (\y -> x { _ddblfpmLogFileName = y })
       <$> f (_ddblfpmLogFileName x)
{-# INLINE ddblfpmLogFileName #-}

-- | The number of lines remaining to be downloaded.
ddblfpmNumberOfLines
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DownloadDBLogFilePortion
    -> f DownloadDBLogFilePortion
ddblfpmNumberOfLines f x =
    (\y -> x { _ddblfpmNumberOfLines = y })
       <$> f (_ddblfpmNumberOfLines x)
{-# INLINE ddblfpmNumberOfLines #-}

-- | The pagination token provided in the previous request. If this parameter is
-- specified the response includes only records beyond the marker, up to
-- MaxRecords.
ddblfpmMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DownloadDBLogFilePortion
    -> f DownloadDBLogFilePortion
ddblfpmMarker f x =
    (\y -> x { _ddblfpmMarker = y })
       <$> f (_ddblfpmMarker x)
{-# INLINE ddblfpmMarker #-}

instance ToQuery DownloadDBLogFilePortion where
    toQuery = genericQuery def

data DownloadDBLogFilePortionResponse = DownloadDBLogFilePortionResponse
    { _ddblfpdAdditionalDataPending :: Bool
      -- ^ Boolean value that if true, indicates there is more data to be
      -- downloaded.
    , _ddblfpdLogFileData :: ByteString
      -- ^ Entries from the specified log file.
    , _ddblfpdMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous
      -- DownloadDBLogFilePortion request.
    } deriving (Show, Generic)

-- | Boolean value that if true, indicates there is more data to be downloaded.
ddblfpdAdditionalDataPending
    :: Functor f
    => (Bool
    -> f (Bool))
    -> DownloadDBLogFilePortionResponse
    -> f DownloadDBLogFilePortionResponse
ddblfpdAdditionalDataPending f x =
    (\y -> x { _ddblfpdAdditionalDataPending = y })
       <$> f (_ddblfpdAdditionalDataPending x)
{-# INLINE ddblfpdAdditionalDataPending #-}

-- | Entries from the specified log file.
ddblfpdLogFileData
    :: Functor f
    => (ByteString
    -> f (ByteString))
    -> DownloadDBLogFilePortionResponse
    -> f DownloadDBLogFilePortionResponse
ddblfpdLogFileData f x =
    (\y -> x { _ddblfpdLogFileData = y })
       <$> f (_ddblfpdLogFileData x)
{-# INLINE ddblfpdLogFileData #-}

-- | An optional pagination token provided by a previous
-- DownloadDBLogFilePortion request.
ddblfpdMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DownloadDBLogFilePortionResponse
    -> f DownloadDBLogFilePortionResponse
ddblfpdMarker f x =
    (\y -> x { _ddblfpdMarker = y })
       <$> f (_ddblfpdMarker x)
{-# INLINE ddblfpdMarker #-}

instance FromXML DownloadDBLogFilePortionResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DownloadDBLogFilePortion where
    type Sv DownloadDBLogFilePortion = RDS
    type Rs DownloadDBLogFilePortion = DownloadDBLogFilePortionResponse

    request = post "DownloadDBLogFilePortion"
    response _ = xmlResponse

instance AWSPager DownloadDBLogFilePortion where
    next rq rs
        | not (_ddblfpdAdditionalDataPending rs) = Nothing
        | otherwise = Just $ rq
            { _ddblfpmMarker = _ddblfpdMarker rs
            }
