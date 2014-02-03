{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EMR.ListInstanceGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Provides all available details about the instance groups in a cluster.
module Network.AWS.EMR.ListInstanceGroups where

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

-- | Convenience method utilising default fields to construct
-- the minimum required request.
listInstanceGroups :: ListInstanceGroups
listInstanceGroups = ListInstanceGroups
    { ligiClusterId = Nothing
    , ligiMarker = Nothing
    }

data ListInstanceGroups = ListInstanceGroups
    { ligiClusterId :: Maybe Text
      -- ^ The identifier of the cluster for which to list the instance groups.
    , ligiMarker :: Maybe Text
      -- ^ The pagination token is a random string indicating whether there are more
      -- results to fetch. Provide the pagination token from earlier API calls to
      -- retrieve the next page of results. When the value is null, all results have
      -- been returned.
    } deriving (Eq, Show, Generic)

instance ToJSON ListInstanceGroups where
    toJSON = genericToJSON jsonOptions

instance AWSRequest ListInstanceGroups where
    type Er ListInstanceGroups = EMRError
    type Rs ListInstanceGroups = ListInstanceGroupsResponse
    request  = getJSON service
    response = responseJSON

data ListInstanceGroupsResponse = ListInstanceGroupsResponse
    { ligirsInstanceGroups :: [InstanceGroup]
      -- ^ The list of instance groups for the cluster and given filters.
    , ligirsMarker :: Maybe Text
      -- ^ The pagination token is a random string indicating whether there are more
      -- results to fetch. Use the pagination token in later API calls to retrieve
      -- the next page of results. When the value is null, all results have been
      -- returned.
    } deriving (Eq, Show, Generic)

instance FromJSON ListInstanceGroupsResponse where
    fromJSON = genericFromJSON jsonOptions

