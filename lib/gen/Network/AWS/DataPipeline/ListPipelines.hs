{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline.ListPipelines
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of pipeline identifiers for all active pipelines.
-- Identifiers are returned only for pipelines you have permission to access.
-- POST / HTTP/1.1 Content-Type: application/x-amz-json-1.1 X-Amz-Target:
-- DataPipeline.ListPipelines Content-Length: 14 Host:
-- datapipeline.us-east-1.amazonaws.com X-Amz-Date: Mon, 12 Nov 2012 17:49:52
-- GMT Authorization: AuthParams {} Status: x-amzn-RequestId:
-- b3104dc5-0734-11e2-af6f-6bc7a6be60d9 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 39 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {"PipelineIdList": [ {"id": "df-08785951KAKJEXAMPLE", "name":
-- "MyPipeline"}, {"id": "df-08662578ISYEXAMPLE", "name": "MySecondPipeline"}
-- ] }.
module Network.AWS.DataPipeline.ListPipelines where

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

import Network.AWS.DataPipeline.Service
import Network.AWS.DataPipeline.Types

-- | Convenience method utilising default fields where applicable.
listPipelines :: AWS (Either DataPipelineError ListPipelinesResponse)
listPipelines = undefined $ ListPipelines
    { lpiMarker = Nothing
    }

data ListPipelines = ListPipelines
    { lpiMarker :: Maybe Text
      -- ^ The starting point for the results to be returned. The first time you call
      -- ListPipelines, this value should be empty. As long as the action returns
      -- HasMoreResults as True, you can call ListPipelines again and pass the
      -- marker value from the response to retrieve the next set of results.
    } deriving (Eq, Show, Generic)

instance ToJSON ListPipelines

instance AWSRequest ListPipelines where
    type Er ListPipelines = DataPipelineError
    type Rs ListPipelines = ListPipelinesResponse
    request  = getJSON service
    response = responseJSON

data ListPipelinesResponse = ListPipelinesResponse
    { lpirsHasMoreResults :: Maybe Bool
      -- ^ If True, there are more results that can be obtained by a subsequent call
      -- to ListPipelines.
    , lpirsMarker :: Maybe Text
      -- ^ If not null, indicates the starting point for the set of pipeline
      -- identifiers that the next call to ListPipelines will retrieve. If null,
      -- there are no more pipeline identifiers.
    , lpirsPipelineIdList :: [PipelineIdName]
      -- ^ A list of all the pipeline identifiers that your account has permission to
      -- access. If you require additional information about the pipelines, you can
      -- use these identifiers to call DescribePipelines and GetPipelineDefinition.
    } deriving (Eq, Show, Generic)

instance FromJSON ListPipelinesResponse
