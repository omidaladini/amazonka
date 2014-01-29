{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.DescribeDBSecurityGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of DBSecurityGroup descriptions. If a DBSecurityGroupName is
-- specified, the list will contain only the descriptions of the specified DB
-- security group. https://rds.amazonaws.com/ ?Action=DescribeDBSecurityGroups
-- &Version=2013-05-15 &MaxRecords=100 &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2011-02-15T19%3A40%3A19.926Z
-- &AWSAccessKeyId= &Signature= authorized myec2securitygroup 054794666394
-- default 127.0.0.1/30 authorized 621567473609 default vpc-1ab2c3d4 My new
-- DBSecurityGroup 192.168.1.1/24 authorized 621567473609 mydbsecuritygroup
-- vpc-1ab2c3d5 My new DBSecurityGroup 621567473609 mydbsecuritygroup4
-- vpc-1ab2c3d6 bbdad154-bf42-11de-86a4-97241dfaadff.
module Network.AWS.RDS.DescribeDBSecurityGroups where

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
describeDBSecurityGroups :: AWS (Either RDSError DescribeDBSecurityGroupsResponse)
describeDBSecurityGroups = undefined $ DescribeDBSecurityGroups
    { ddbsgpDBSecurityGroupName = Nothing
    , ddbsgpMarker = Nothing
    , ddbsgpMaxRecords = Nothing
    }

data DescribeDBSecurityGroups = DescribeDBSecurityGroups
    { ddbsgpDBSecurityGroupName :: Maybe Text
      -- ^ The name of the DB security group to return details for.
    , ddbsgpMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous
      -- DescribeDBSecurityGroups request. If this parameter is specified, the
      -- response includes only records beyond the marker, up to the value specified
      -- by MaxRecords.
    , ddbsgpMaxRecords :: Maybe Int
      -- ^ The maximum number of records to include in the response. If more records
      -- exist than the specified MaxRecords value, a pagination token called a
      -- marker is included in the response so that the remaining results may be
      -- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeDBSecurityGroups

instance AWSRequest DescribeDBSecurityGroups where
    type Er DescribeDBSecurityGroups = RDSError
    type Rs DescribeDBSecurityGroups = DescribeDBSecurityGroupsResponse
    request = getQuery service "DescribeDBSecurityGroups"

instance AWSPager DescribeDBSecurityGroups where
    next rq rs
        | Just x <- ddbsgprsMarker rs = Just $ rq { ddbsgpMarker = Just x }
        | otherwise = Nothing

data DescribeDBSecurityGroupsResponse = DescribeDBSecurityGroupsResponse
    { ddbsgprsDBSecurityGroups :: [DBSecurityGroup]
      -- ^ A list of DBSecurityGroup instances.
    , ddbsgprsMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous request. If this
      -- parameter is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeDBSecurityGroupsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeDBSecurityGroupsResponse"
        :| ["DescribeDBSecurityGroupsResult"]
