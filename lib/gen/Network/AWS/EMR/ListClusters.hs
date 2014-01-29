{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EMR.ListClusters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Provides the status of all clusters visible to this AWS account. Allows you
-- to filter the list of clusters based on certain criteria; for example,
-- filtering by cluster creation date and time or by status. This call returns
-- a maximum of 50 clusters per call, but returns a marker to track the paging
-- of the cluster list across multiple ListClusters calls.
module Network.AWS.EMR.ListClusters where

import           Control.Monad
import           Data.Aeson
import           Data.ByteString      (ByteString)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Time            (UTCTime)
import           GHC.Generics         (Generic)
import           Network.AWS.Internal hiding (Endpoint, Region, AvailabilityZone)

import Network.AWS.EMR.Service
import Network.AWS.EMR.Types

-- | Convenience method utilising default fields where applicable.
listClusters :: AWS (Either EMRError ListClustersResponse)
listClusters = undefined $ ListClusters
    { lciClusterStates = []
    , lciCreatedAfter = Nothing
    , lciCreatedBefore = Nothing
    , lciMarker = Nothing
    }

data ListClusters = ListClusters
    { lciClusterStates :: [ClusterState]
      -- ^ The cluster state filters to apply when listing clusters.
    , lciCreatedAfter :: Maybe UTCTime
      -- ^ The creation date and time beginning value filter for listing clusters.
    , lciCreatedBefore :: Maybe UTCTime
      -- ^ The creation date and time end value filter for listing clusters.
    , lciMarker :: Maybe Text
      -- ^ The pagination token is a random string indicating whether there are more
      -- results to fetch. Provide the pagination token from earlier API calls to
      -- retrieve the next page of results. When the value is null, all results have
      -- been returned.
    } deriving (Eq, Show, Generic)

instance ToJSON ListClusters

instance AWSRequest ListClusters where
    type Er ListClusters = EMRError
    type Rs ListClusters = ListClustersResponse
    request  = getJSON service
    response = responseJSON

data ListClustersResponse = ListClustersResponse
    { lcirsClusters :: [ClusterSummary]
      -- ^ The list of clusters for the account based on the given filters.
    , lcirsMarker :: Maybe Text
      -- ^ The pagination token is a random string indicating whether there are more
      -- results to fetch. Use the pagination token in later API calls to retrieve
      -- the next page of results. When the value is null, all results have been
      -- returned.
    } deriving (Eq, Show, Generic)

instance FromJSON ListClustersResponse
