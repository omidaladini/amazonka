{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline.DescribeObjects
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the object definitions for a set of objects associated with the
-- pipeline. Object definitions are composed of a set of fields that define
-- the properties of the object. POST / HTTP/1.1 Content-Type:
-- application/x-amz-json-1.1 X-Amz-Target: DataPipeline.DescribeObjects
-- Content-Length: 98 Host: datapipeline.us-east-1.amazonaws.com X-Amz-Date:
-- Mon, 12 Nov 2012 17:49:52 GMT Authorization: AuthParams {"pipelineId":
-- "df-06372391ZG65EXAMPLE", "objectIds": ["Schedule"], "evaluateExpressions":
-- true} x-amzn-RequestId: 4c18ea5d-0777-11e2-8a14-21bb8a1f50ef Content-Type:
-- application/x-amz-json-1.1 Content-Length: 1488 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {"hasMoreResults": false, "pipelineObjects": [ {"fields": [
-- {"key": "startDateTime", "stringValue": "2012-12-12T00:00:00"}, {"key":
-- "parent", "refValue": "Default"}, {"key": "@sphere", "stringValue":
-- "COMPONENT"}, {"key": "type", "stringValue": "Schedule"}, {"key": "period",
-- "stringValue": "1 hour"}, {"key": "endDateTime", "stringValue":
-- "2012-12-21T18:00:00"}, {"key": "@version", "stringValue": "1"}, {"key":
-- "@status", "stringValue": "PENDING"}, {"key": "@pipelineId", "stringValue":
-- "df-06372391ZG65EXAMPLE"} ], "id": "Schedule", "name": "Schedule"} ] }.
module Network.AWS.DataPipeline.DescribeObjects where

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

data DescribeObjects = DescribeObjects
    { doiEvaluateExpressions :: Maybe Bool
      -- ^ Indicates whether any expressions in the object should be evaluated when
      -- the object descriptions are returned.
    , doiMarker :: Maybe Text
      -- ^ The starting point for the results to be returned. The first time you call
      -- DescribeObjects, this value should be empty. As long as the action returns
      -- HasMoreResults as True, you can call DescribeObjects again and pass the
      -- marker value from the response to retrieve the next set of results.
    , doiObjectIds :: [Text]
      -- ^ Identifiers of the pipeline objects that contain the definitions to be
      -- described. You can pass as many as 25 identifiers in a single call to
      -- DescribeObjects.
    , doiPipelineId :: !Text
      -- ^ Identifier of the pipeline that contains the object definitions.
    } deriving (Eq, Show, Generic)

instance ToJSON DescribeObjects

instance AWSRequest DescribeObjects where
    type Er DescribeObjects = DataPipelineError
    type Rs DescribeObjects = DescribeObjectsResponse
    request  = getJSON service
    response = responseJSON

data DescribeObjectsResponse = DescribeObjectsResponse
    { doirsHasMoreResults :: Maybe Bool
      -- ^ If True, there are more pages of results to return.
    , doirsMarker :: Maybe Text
      -- ^ The starting point for the next page of results. To view the next page of
      -- results, call DescribeObjects again with this marker value.
    , doirsPipelineObjects :: [PipelineObject]
      -- ^ An array of object definitions that are returned by the call to
      -- DescribeObjects.
    } deriving (Eq, Show, Generic)

instance FromJSON DescribeObjectsResponse
