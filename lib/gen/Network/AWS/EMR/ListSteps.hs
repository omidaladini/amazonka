{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EMR.ListSteps
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Provides a list of steps for the cluster.
module Network.AWS.EMR.ListSteps where

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
listSteps :: AWS (Either EMRError ListStepsResponse)
listSteps = undefined $ ListSteps
    { lsiClusterId = Nothing
    , lsiMarker = Nothing
    , lsiStepStates = []
    }

data ListSteps = ListSteps
    { lsiClusterId :: Maybe Text
      -- ^ The identifier of the cluster for which to list the steps.
    , lsiMarker :: Maybe Text
      -- ^ The pagination token is a random string indicating whether there are more
      -- results to fetch. Provide the pagination token from earlier API calls to
      -- retrieve the next page of results. When the value is null, all results have
      -- been returned.
    , lsiStepStates :: [StepState]
      -- ^ The filter to limit the step list based on certain states.
    } deriving (Eq, Show, Generic)

instance ToJSON ListSteps

instance AWSRequest ListSteps where
    type Er ListSteps = EMRError
    type Rs ListSteps = ListStepsResponse
    request  = getJSON service
    response = responseJSON

data ListStepsResponse = ListStepsResponse
    { lsirsMarker :: Maybe Text
      -- ^ The pagination token is a random string indicating whether there are more
      -- results to fetch. Use the pagination token in later API calls to retrieve
      -- the next page of results. When the value is null, all results have been
      -- returned.
    , lsirsSteps :: [StepSummary]
      -- ^ The filtered list of steps for the cluster.
    } deriving (Eq, Show, Generic)

instance FromJSON ListStepsResponse
