{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DescribeClusterParameterGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of Amazon Redshift parameter groups, including parameter
-- groups you created and the default parameter group. For each parameter
-- group, the response includes the parameter group name, description, and
-- parameter group family name. You can optionally specify a name to retrieve
-- the description of a specific parameter group. For more information about
-- managing parameter groups, go to Amazon Redshift Parameter Groups in the
-- Amazon Redshift Management Guide. https://redshift.us-east-1.amazonaws.com/
-- ?Action=DescribeClusterParameterGroups &Version=2012-12-01
-- &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T004002Z
-- &x-amz-signedheaders=content-type;host;x-amz-date redshift-1.0 Default
-- parameter group for redshift-1.0 default.redshift-1.0 redshift-1.0
-- description my parameter group parametergroup1
-- 6d28788b-64f5-11e2-b343-393adc3f0a21.
module Network.AWS.Redshift.DescribeClusterParameterGroups where

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

data DescribeClusterParameterGroups = DescribeClusterParameterGroups
    { dcpgmMarker :: Maybe Text
      -- ^ An optional marker returned by a previous DescribeClusterParameterGroups
      -- request to indicate the first parameter group that the current request will
      -- return.
    , dcpgmMaxRecords :: Maybe Int
      -- ^ The maximum number of parameter group records to include in the response.
      -- If more records exist than the specified MaxRecords value, the response
      -- includes a marker that you can use in a subsequent
      -- DescribeClusterParameterGroups request to retrieve the next set of records.
      -- Default: 100 Constraints: Value must be at least 20 and no more than 100.
    , dcpgmParameterGroupName :: Maybe Text
      -- ^ The name of a specific parameter group for which to return details. By
      -- default, details about all parameter groups and the default parameter group
      -- are returned.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeClusterParameterGroups

instance AWSRequest DescribeClusterParameterGroups where
    type Er DescribeClusterParameterGroups = RedshiftError
    type Rs DescribeClusterParameterGroups = DescribeClusterParameterGroupsResponse
    request = getQuery service "DescribeClusterParameterGroups"

data DescribeClusterParameterGroupsResponse = DescribeClusterParameterGroupsResponse
    { dcpgmrsMarker :: Maybe Text
      -- ^ A marker at which to continue listing cluster parameter groups in a new
      -- request. The response returns a marker if there are more parameter groups
      -- to list than returned in the response.
    , dcpgmrsParameterGroups :: [ClusterParameterGroup]
      -- ^ A list of ClusterParameterGroup instances. Each instance describes one
      -- cluster parameter group.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeClusterParameterGroupsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeClusterParameterGroupsResponse"
        :| ["DescribeClusterParameterGroupsResult"]
