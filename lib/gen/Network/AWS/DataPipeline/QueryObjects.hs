{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline.QueryObjects
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Queries a pipeline for the names of objects that match a specified set of
-- conditions. The objects returned by QueryObjects are paginated and then
-- filtered by the value you set for query. This means the action may return
-- an empty result set with a value set for marker. If HasMoreResults is set
-- to True, you should continue to call QueryObjects, passing in the returned
-- value for marker, until HasMoreResults returns False. POST / HTTP/1.1
-- Content-Type: application/x-amz-json-1.1 X-Amz-Target:
-- DataPipeline.QueryObjects Content-Length: 123 Host:
-- datapipeline.us-east-1.amazonaws.com X-Amz-Date: Mon, 12 Nov 2012 17:49:52
-- GMT Authorization: AuthParams {"pipelineId": "df-06372391ZG65EXAMPLE",
-- "query": {"selectors": [ ] }, "sphere": "PO", "marker": "", "limit": 10}
-- x-amzn-RequestId: 14d704c1-0775-11e2-af6f-6bc7a6be60d9 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 72 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {"hasMoreResults": false, "ids":
-- ["@SayHello_1_2012-09-25T17:00:00"] }.
module Network.AWS.DataPipeline.QueryObjects where

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
queryObjects :: Text
             -> Text
             -> AWS (Either DataPipelineError QueryObjectsResponse)
queryObjects p1 p2 = undefined $ QueryObjects
    { qoiPipelineId = p1
    , qoiSphere = p2
    , qoiLimit = Nothing
    , qoiMarker = Nothing
    , qoiQuery = Nothing
    }

data QueryObjects = QueryObjects
    { qoiLimit :: Maybe Int
      -- ^ Specifies the maximum number of object names that QueryObjects will return
      -- in a single call. The default value is 100.
    , qoiMarker :: Maybe Text
      -- ^ The starting point for the results to be returned. The first time you call
      -- QueryObjects, this value should be empty. As long as the action returns
      -- HasMoreResults as True, you can call QueryObjects again and pass the marker
      -- value from the response to retrieve the next set of results.
    , qoiPipelineId :: !Text
      -- ^ Identifier of the pipeline to be queried for object names.
    , qoiQuery :: Maybe Query
      -- ^ Query that defines the objects to be returned. The Query object can contain
      -- a maximum of ten selectors. The conditions in the query are limited to
      -- top-level String fields in the object. These filters can be applied to
      -- components, instances, and attempts.
    , qoiSphere :: !Text
      -- ^ Specifies whether the query applies to components or instances. Allowable
      -- values: COMPONENT, INSTANCE, ATTEMPT.
    } deriving (Eq, Show, Generic)

instance ToJSON QueryObjects

instance AWSRequest QueryObjects where
    type Er QueryObjects = DataPipelineError
    type Rs QueryObjects = QueryObjectsResponse
    request  = getJSON service
    response = responseJSON

data QueryObjectsResponse = QueryObjectsResponse
    { qoirsHasMoreResults :: Maybe Bool
      -- ^ If True, there are more results that can be obtained by a subsequent call
      -- to QueryObjects.
    , qoirsIds :: [Text]
      -- ^ A list of identifiers that match the query selectors.
    , qoirsMarker :: Maybe Text
      -- ^ The starting point for the results to be returned. As long as the action
      -- returns HasMoreResults as True, you can call QueryObjects again and pass
      -- the marker value from the response to retrieve the next set of results.
    } deriving (Eq, Show, Generic)

instance FromJSON QueryObjectsResponse
