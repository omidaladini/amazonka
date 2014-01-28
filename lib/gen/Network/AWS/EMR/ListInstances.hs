{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EMR.ListInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Provides information about the cluster instances that Amazon EMR provisions
-- on behalf of a user when it creates the cluster. For example, this
-- operation indicates when the EC2 instances reach the Ready state, when
-- instances become available to Amazon EMR to use for jobs, and the IP
-- addresses for cluster instances, etc.
module Network.AWS.EMR.ListInstances where

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

data ListInstances = ListInstances
    { liiClusterId :: Maybe Text
      -- ^ The identifier of the cluster for which to list the instances.
    , liiInstanceGroupId :: Maybe Text
      -- ^ The identifier of the instance group for which to list the instances.
    , liiInstanceGroupTypes :: [InstanceGroupType]
      -- ^ The type of instance group for which to list the instances.
    , liiMarker :: Maybe Text
      -- ^ The pagination token is a random string indicating whether there are more
      -- results to fetch. Provide the pagination token from earlier API calls to
      -- retrieve the next page of results. When the value is null, all results have
      -- been returned.
    } deriving (Eq, Show, Generic)

instance ToJSON ListInstances

instance AWSRequest ListInstances where
    type Er ListInstances = EMRError
    type Rs ListInstances = ListInstancesResponse
    request  = getJSON service
    response = responseJSON

data ListInstancesResponse = ListInstancesResponse
    { liirsInstances :: [Instance]
      -- ^ The list of instances for the cluster and given filters.
    , liirsMarker :: Maybe Text
      -- ^ The pagination token is a random string indicating whether there are more
      -- results to fetch. Use the pagination token in later API calls to retrieve
      -- the next page of results. When the value is null, all results have been
      -- returned.
    } deriving (Eq, Show, Generic)

instance FromJSON ListInstancesResponse
