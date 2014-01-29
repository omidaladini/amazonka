{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.DescribeOrderableDBInstanceOptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of orderable DB instance options for the specified engine.
-- https://rds.amazonaws.com/ ?Action=DescribeOrderableDBInstanceOptions
-- &Engine=mysql &MaxRecords=100 &Version=2013-05-15
-- &Timestamp=2011-05-23T07%3A49%3A17.749Z &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &AWSAccessKeyId= &Signature= true mysql
-- general-public-license true 5.1.45 db.m1.large us-east-1a yes us-east-1b no
-- us-east-1d yes true mysql general-public-license true 5.1.45 db.m1.small
-- us-east-1a yes us-east-1b yes us-east-1d yes true mysql
-- general-public-license true 5.1.45 db.m1.xlarge us-east-1a yes us-east-1b
-- yes us-east-1d yes true mysql general-public-license true 5.1.45
-- db.m2.2xlarge us-east-1a yes us-east-1b yes us-east-1d yes true mysql
-- general-public-license true 5.1.45 db.m2.4xlarge us-east-1a yes us-east-1b
-- no us-east-1d no 2a0406d7-8511-11e0-90aa-eb648410240d.
module Network.AWS.RDS.DescribeOrderableDBInstanceOptions where

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
describeOrderableDBInstanceOptions :: Text
                                   -> AWS (Either RDSError DescribeOrderableDBInstanceOptionsResponse)
describeOrderableDBInstanceOptions p1 = undefined $ DescribeOrderableDBInstanceOptions
    { dodbiomEngine = p1
    , dodbiomDBInstanceClass = Nothing
    , dodbiomEngineVersion = Nothing
    , dodbiomLicenseModel = Nothing
    , dodbiomMarker = Nothing
    , dodbiomMaxRecords = Nothing
    , dodbiomVpc = Nothing
    }

data DescribeOrderableDBInstanceOptions = DescribeOrderableDBInstanceOptions
    { dodbiomDBInstanceClass :: Maybe Text
      -- ^ The DB instance class filter value. Specify this parameter to show only the
      -- available offerings matching the specified DB instance class.
    , dodbiomEngine :: !Text
      -- ^ The name of the engine to retrieve DB instance options for.
    , dodbiomEngineVersion :: Maybe Text
      -- ^ The engine version filter value. Specify this parameter to show only the
      -- available offerings matching the specified engine version.
    , dodbiomLicenseModel :: Maybe Text
      -- ^ The license model filter value. Specify this parameter to show only the
      -- available offerings matching the specified license model.
    , dodbiomMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous
      -- DescribeOrderableDBInstanceOptions request. If this parameter is specified,
      -- the response includes only records beyond the marker, up to the value
      -- specified by MaxRecords .
    , dodbiomMaxRecords :: Maybe Int
      -- ^ The maximum number of records to include in the response. If more records
      -- exist than the specified MaxRecords value, a pagination token called a
      -- marker is included in the response so that the remaining results can be
      -- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
    , dodbiomVpc :: Maybe Bool
      -- ^ The VPC filter value. Specify this parameter to show only the available VPC
      -- or non-VPC offerings.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeOrderableDBInstanceOptions

instance AWSRequest DescribeOrderableDBInstanceOptions where
    type Er DescribeOrderableDBInstanceOptions = RDSError
    type Rs DescribeOrderableDBInstanceOptions = DescribeOrderableDBInstanceOptionsResponse
    request = getQuery service "DescribeOrderableDBInstanceOptions"

instance AWSPager DescribeOrderableDBInstanceOptions where
    next rq rs
        | Just x <- dodbiomrsMarker rs = Just $ rq { dodbiomMarker = Just x }
        | otherwise = Nothing

data DescribeOrderableDBInstanceOptionsResponse = DescribeOrderableDBInstanceOptionsResponse
    { dodbiomrsMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous
      -- OrderableDBInstanceOptions request. If this parameter is specified, the
      -- response includes only records beyond the marker, up to the value specified
      -- by MaxRecords .
    , dodbiomrsOrderableDBInstanceOptions :: [OrderableDBInstanceOption]
      -- ^ An OrderableDBInstanceOption structure containing information about
      -- orderable options for the DB instance.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeOrderableDBInstanceOptionsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeOrderableDBInstanceOptionsResponse"
        :| ["DescribeOrderableDBInstanceOptionsResult"]
