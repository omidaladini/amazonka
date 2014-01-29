{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DescribeClusterSecurityGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about Amazon Redshift security groups. If the name of a
-- security group is specified, the response will contain only information
-- about only that security group. For information about managing security
-- groups, go to Amazon Redshift Cluster Security Groups in the Amazon
-- Redshift Management Guide. https://redshift.us-east-1.amazonaws.com/
-- ?Action=DescribeClusterSecurityGroups &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T010237Z
-- &x-amz-signedheaders=content-type;host;x-amz-date 0.0.0.0/0 authorized
-- default default my security group securitygroup1
-- 947a8305-64f8-11e2-bec0-17624ad140dd.
module Network.AWS.Redshift.DescribeClusterSecurityGroups where

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

import Network.AWS.Redshift.Service
import Network.AWS.Redshift.Types

-- | Convenience method utilising default fields where applicable.
describeClusterSecurityGroups :: AWS (Either RedshiftError DescribeClusterSecurityGroupsResponse)
describeClusterSecurityGroups = undefined $ DescribeClusterSecurityGroups
    { dcsgnClusterSecurityGroupName = Nothing
    , dcsgnMarker = Nothing
    , dcsgnMaxRecords = Nothing
    }

data DescribeClusterSecurityGroups = DescribeClusterSecurityGroups
    { dcsgnClusterSecurityGroupName :: Maybe Text
      -- ^ The name of a cluster security group for which you are requesting details.
      -- You can specify either the Marker parameter or a ClusterSecurityGroupName
      -- parameter, but not both. Example: securitygroup1.
    , dcsgnMarker :: Maybe Text
      -- ^ An optional marker returned by a previous DescribeClusterSecurityGroups
      -- request to indicate the first security group that the current request will
      -- return. You can specify either the Marker parameter or a
      -- ClusterSecurityGroupName parameter, but not both.
    , dcsgnMaxRecords :: Maybe Int
      -- ^ The maximum number of records to be included in the response. If more
      -- records exist than the specified MaxRecords value, a marker is included in
      -- the response, which you can use in a subsequent
      -- DescribeClusterSecurityGroups request. Default: 100 Constraints: Value must
      -- be at least 20 and no more than 100.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeClusterSecurityGroups

instance AWSRequest DescribeClusterSecurityGroups where
    type Er DescribeClusterSecurityGroups = RedshiftError
    type Rs DescribeClusterSecurityGroups = DescribeClusterSecurityGroupsResponse
    request = getQuery service "DescribeClusterSecurityGroups"

data DescribeClusterSecurityGroupsResponse = DescribeClusterSecurityGroupsResponse
    { dcsgnrsClusterSecurityGroups :: [ClusterSecurityGroup]
      -- ^ A list of ClusterSecurityGroup instances.
    , dcsgnrsMarker :: Maybe Text
      -- ^ A marker at which to continue listing cluster security groups in a new
      -- request. The response returns a marker if there are more security groups to
      -- list than could be returned in the response.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeClusterSecurityGroupsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeClusterSecurityGroupsResponse"
        :| ["DescribeClusterSecurityGroupsResult"]
