{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DescribeClusters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns properties of provisioned clusters including general cluster
-- properties, cluster database properties, maintenance and backup properties,
-- and security and access properties. This operation supports pagination. For
-- more information about managing clusters, go to Amazon Redshift Clusters in
-- the Amazon Redshift Management Guide . Describing All Clusters The
-- following example shows a request that describes all clusters.
-- https://redshift.us-east-1.amazonaws.com/ ?Action=DescribeClusters
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130123/us-east-1/redshift/aws4_request
-- &x-amz-date=20130123T000452Z
-- &x-amz-signedheaders=content-type;host;x-amz-date **** 1.0 creating 2 1
-- true false dev sun:10:30-sun:11:00 in-sync default.redshift-1.0 active
-- default us-east-1a dw.hs1.xlarge examplecluster true masteruser
-- 837d45d6-64f0-11e2-b07c-f7fbdd006c67.
module Network.AWS.Redshift.DescribeClusters where

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
describeClusters :: DescribeClusters
describeClusters = DescribeClusters
    { dcmClusterIdentifier = Nothing
    , dcmMarker = Nothing
    , dcmMaxRecords = Nothing
    }

data DescribeClusters = DescribeClusters
    { dcmClusterIdentifier :: Maybe Text
      -- ^ The unique identifier of a cluster whose properties you are requesting.
      -- This parameter isn't case sensitive. The default is that all clusters
      -- defined for an account are returned.
    , dcmMarker :: Maybe Text
      -- ^ An optional marker returned by a previous DescribeClusters request to
      -- indicate the first cluster that the current DescribeClusters request will
      -- return. You can specify either a Marker parameter or a ClusterIdentifier
      -- parameter in a DescribeClusters request, but not both.
    , dcmMaxRecords :: Maybe Int
      -- ^ The maximum number of records that the response can include. If more
      -- records exist than the specified MaxRecords value, a marker is included in
      -- the response that can be used in a new DescribeClusters request to continue
      -- listing results. Default: 100 Constraints: Value must be at least 20 and no
      -- more than 100.
    } deriving (Eq, Show, Generic)

instance ToQuery DescribeClusters

instance AWSRequest DescribeClusters where
    type Er DescribeClusters = RedshiftError
    type Rs DescribeClusters = DescribeClustersResponse
    request = getQuery service "DescribeClusters"

data DescribeClustersResponse = DescribeClustersResponse
    { dcmrsClusters :: [Cluster]
      -- ^ A list of Cluster objects, where each object describes one cluster.
    , dcmrsMarker :: Maybe Text
      -- ^ A marker at which to continue listing clusters in a new request. A marker
      -- is returned if there are more clusters to list than were returned in the
      -- response.
    } deriving (Eq, Show, Generic)

instance FromXML DescribeClustersResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromNestedRoot
         $ "DescribeClustersResponse"
        :| ["DescribeClustersResult"]
