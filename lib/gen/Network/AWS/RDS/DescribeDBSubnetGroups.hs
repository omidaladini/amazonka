{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.DescribeDBSubnetGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of DBSubnetGroup descriptions. If a DBSubnetGroupName is
-- specified, the list will contain only the descriptions of the specified
-- DBSubnetGroup. For an overview of CIDR ranges, go to the Wikipedia
-- Tutorial. https://rds.amazonaws.com/ ?Action=DescribeDBSubnetGroups
-- &Version=2013-05-15 &MaxRecords=100 &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2011-02-15T19%3A40%3A19.926Z
-- &AWSAccessKeyId= &Signature= 990524496922 Complete description subnet_grp1
-- Active subnet-7c5b4115 us-east-1c Active subnet-7b5b4112 us-east-1b Active
-- subnet-3ea6bd57 us-east-1d 990524496922 Complete description subnet_grp2
-- Active subnet-7c5b4115 us-east-1c Active subnet-7b5b4112 us-east-1b Active
-- subnet-3ea6bd57 us-east-1d 31d0faee-229b-11e1-81f1-df3a2a803dad.
module Network.AWS.RDS.DescribeDBSubnetGroups where

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
describeDBSubnetGroups :: AWS (Either RDSError DescribeDBSubnetGroupsResponse)
describeDBSubnetGroups = undefined $ DescribeDBSubnetGroups
    { ddbsgmDBSubnetGroupName = Nothing
    , ddbsgmMarker = Nothing
    , ddbsgmMaxRecords = Nothing
    }

data DescribeDBSubnetGroups = DescribeDBSubnetGroups
    { ddbsgmDBSubnetGroupName :: Maybe Text
      -- ^ The name of the DB subnet group to return details for.
    , ddbsgmMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous DescribeDBSubnetGroups
      -- request. If this parameter is specified, the response includes only records
      -- beyond the marker, up to the value specified by MaxRecords.
    , ddbsgmMaxRecords :: Maybe Int
      -- ^ The maximum number of records to include in the response. If more records
      -- exist than the specified MaxRecords value, a pagination token called a
      -- marker is included in the response so that the remaining results may be
      -- retrieved. Default: 100 Constraints: minimum 20, maximum 100.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeDBSubnetGroups

instance AWSRequest DescribeDBSubnetGroups where
    type Er DescribeDBSubnetGroups = RDSError
    type Rs DescribeDBSubnetGroups = DescribeDBSubnetGroupsResponse
    request = getQuery service "DescribeDBSubnetGroups"

instance AWSPager DescribeDBSubnetGroups where
    next rq rs
        | Just x <- ddbsgmrsMarker rs = Just $ rq { ddbsgmMarker = Just x }
        | otherwise = Nothing

data DescribeDBSubnetGroupsResponse = DescribeDBSubnetGroupsResponse
    { ddbsgmrsDBSubnetGroups :: [DBSubnetGroup]
      -- ^ A list of DBSubnetGroup instances.
    , ddbsgmrsMarker :: Maybe Text
      -- ^ An optional pagination token provided by a previous request. If this
      -- parameter is specified, the response includes only records beyond the
      -- marker, up to the value specified by MaxRecords.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeDBSubnetGroupsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeDBSubnetGroupsResponse"
        :| ["DescribeDBSubnetGroupsResult"]
