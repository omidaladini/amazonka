{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EMR.ListBootstrapActions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Provides information about the bootstrap actions associated with a cluster.
module Network.AWS.EMR.ListBootstrapActions where

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
listBootstrapActions :: ListBootstrapActions
listBootstrapActions = ListBootstrapActions
    { lbaiClusterId = Nothing
    , lbaiMarker = Nothing
    }

data ListBootstrapActions = ListBootstrapActions
    { lbaiClusterId :: Maybe Text
      -- ^ The cluster identifier for the bootstrap actions to list.
    , lbaiMarker :: Maybe Text
      -- ^ The pagination token is a random string indicating whether there are more
      -- results to fetch. Provide the pagination token from earlier API calls to
      -- retrieve the next page of results. When the value is null, all results have
      -- been returned.
    } deriving (Eq, Show, Generic)

instance ToJSON ListBootstrapActions

instance AWSRequest ListBootstrapActions where
    type Er ListBootstrapActions = EMRError
    type Rs ListBootstrapActions = ListBootstrapActionsResponse
    request  = getJSON service
    response = responseJSON

data ListBootstrapActionsResponse = ListBootstrapActionsResponse
    { lbairsBootstrapActions :: [Command]
      -- ^ The bootstrap actions associated with the cluster.
    , lbairsMarker :: Maybe Text
      -- ^ The pagination token is a random string indicating whether there are more
      -- results to fetch. Use the pagination token in later API calls to retrieve
      -- the next page of results. When the value is null, all results have been
      -- returned.
    } deriving (Eq, Show, Generic)

instance FromJSON ListBootstrapActionsResponse
