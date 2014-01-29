{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.DescribeEngineDefaultParameters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the default engine and system parameter information for the
-- specified database engine. https://rds.amazonaws.com/
-- ?Action=DescribeEngineDefaultParameters &DBParameterGroupFamily=mysql5.1
-- &Version=2013-05-15 &MaxRecords=100 &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2011-02-15T19%3A10%3A03.510Z
-- &AWSAccessKeyId= &Signature= bG93ZXJfY2FzZV90YWJsZV9uYW1lcw== mysql5.1
-- boolean engine-default false Controls whether user-defined functions that
-- have only an xxx symbol for the main function can be loaded static 0,1
-- allow-suspicious-udfs integer engine-default true Intended for use with
-- master-to-master replication, and can be used to control the operation of
-- AUTO_INCREMENT columns dynamic 1-65535 auto_increment_increment integer
-- engine-default true Determines the starting point for the AUTO_INCREMENT
-- column value dynamic 1-65535 auto_increment_offset
-- 6c1341eb-a124-11df-bf5c-973b09643c5d.
module Network.AWS.RDS.DescribeEngineDefaultParameters where

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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
describeEngineDefaultParameters :: Text
                                -> DescribeEngineDefaultParameters
describeEngineDefaultParameters p1 = undefined $ DescribeEngineDefaultParameters
    { dedpmDBParameterGroupFamily = p1
    , dedpmMarker = Nothing
    , dedpmMaxRecords = Nothing
    }

data DescribeEngineDefaultParameters = DescribeEngineDefaultParameters
    { dedpmDBParameterGroupFamily :: !Text
      -- ^ The name of the DB parameter group family.
    , dedpmMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous
      -- DescribeEngineDefaultParameters request. If this parameter is specified,
      -- the response includes only records beyond the marker, up to the value
      -- specified by MaxRecords.
    , dedpmMaxRecords :: Maybe Int
      -- ^ The maximum number of records to include in the response. If more records
      -- exist than the specified MaxRecords value, a pagination token called a
      -- marker is included in the response so that the remaining results may be
      -- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeEngineDefaultParameters

instance AWSRequest DescribeEngineDefaultParameters where
    type Er DescribeEngineDefaultParameters = RDSError
    type Rs DescribeEngineDefaultParameters = DescribeEngineDefaultParametersResponse
    request = getQuery service "DescribeEngineDefaultParameters"

instance AWSPager DescribeEngineDefaultParameters where
    next rq rs
        | Just _ <- marker = Just $ rq { dedpmMarker = marker }
        | otherwise        = Nothing
      where
        marker = join . fmap edMarker $ dedpmrsEngineDefaults rs

data DescribeEngineDefaultParametersResponse = DescribeEngineDefaultParametersResponse
    { dedpmrsEngineDefaults :: Maybe EngineDefaults
      -- ^ Contains the result of a successful invocation of the
      -- DescribeEngineDefaultParameters action.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeEngineDefaultParametersResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeEngineDefaultParametersResponse"
        :| ["DescribeEngineDefaultParametersResult"]
