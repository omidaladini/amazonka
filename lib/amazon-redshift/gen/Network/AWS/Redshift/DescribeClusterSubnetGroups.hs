{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DescribeClusterSubnetGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns one or more cluster subnet group objects, which contain metadata
-- about your cluster subnet groups. By default, this operation returns
-- information about all cluster subnet groups that are defined in you AWS
-- account. https://redshift.us-east-1.amazonaws.com/
-- ?Action=DescribeClusterSubnetGroups &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130130/us-east-1/redshift/aws4_request
-- &x-amz-date=20130130T153938Z
-- &x-amz-signedheaders=content-type;host;x-amz-date vpc-5d917a30 my subnet
-- group my-subnet-group Complete Active subnet-71c5091c us-east-1a Active
-- subnet-78de1215 us-east-1a 42024b68-6af3-11e2-a726-6368a468fa67.
module Network.AWS.Redshift.DescribeClusterSubnetGroups where

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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
describeClusterSubnetGroups :: DescribeClusterSubnetGroups
describeClusterSubnetGroups = DescribeClusterSubnetGroups
    { dcsgoClusterSubnetGroupName = Nothing
    , dcsgoMarker = Nothing
    , dcsgoMaxRecords = Nothing
    }

data DescribeClusterSubnetGroups = DescribeClusterSubnetGroups
    { dcsgoClusterSubnetGroupName :: Maybe Text
      -- ^ The name of the cluster subnet group for which information is requested.
    , dcsgoMarker :: Maybe Text
      -- ^ An optional marker returned by a previous DescribeClusterSubnetGroups
      -- request to indicate the first cluster subnet group that the current request
      -- will return.
    , dcsgoMaxRecords :: Maybe Int
      -- ^ The maximum number of cluster subnet group records to include in the
      -- response. If more records exist than the specified MaxRecords value, the
      -- response returns a marker that you can use in a subsequent
      -- DescribeClusterSubnetGroups request in order to retrieve the next set of
      -- cluster subnet group records. Default: 100 Constraints: Must be at least 20
      -- and no more than 100.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeClusterSubnetGroups

instance AWSRequest DescribeClusterSubnetGroups where
    type Er DescribeClusterSubnetGroups = RedshiftError
    type Rs DescribeClusterSubnetGroups = DescribeClusterSubnetGroupsResponse
    request = getQuery service "DescribeClusterSubnetGroups"

data DescribeClusterSubnetGroupsResponse = DescribeClusterSubnetGroupsResponse
    { dcsgorsClusterSubnetGroups :: [ClusterSubnetGroup]
      -- ^ A list of ClusterSubnetGroup instances.
    , dcsgorsMarker :: Maybe Text
      -- ^ A marker at which to continue listing cluster subnet groups in a new
      -- request. A marker is returned if there are more cluster subnet groups to
      -- list than were returned in the response.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeClusterSubnetGroupsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeClusterSubnetGroupsResponse"
        :| ["DescribeClusterSubnetGroupsResult"]
