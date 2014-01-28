{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline.GetPipelineDefinition
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the definition of the specified pipeline. You can call
-- GetPipelineDefinition to retrieve the pipeline definition you provided
-- using PutPipelineDefinition. POST / HTTP/1.1 Content-Type:
-- application/x-amz-json-1.1 X-Amz-Target: DataPipeline.GetPipelineDefinition
-- Content-Length: 40 Host: datapipeline.us-east-1.amazonaws.com X-Amz-Date:
-- Mon, 12 Nov 2012 17:49:52 GMT Authorization: AuthParams {"pipelineId":
-- "df-06372391ZG65EXAMPLE"} x-amzn-RequestId:
-- e28309e5-0776-11e2-8a14-21bb8a1f50ef Content-Type:
-- application/x-amz-json-1.1 Content-Length: 890 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {"pipelineObjects": [ {"fields": [ {"key": "workerGroup",
-- "stringValue": "workerGroup"} ], "id": "Default", "name": "Default"},
-- {"fields": [ {"key": "startDateTime", "stringValue":
-- "2012-09-25T17:00:00"}, {"key": "type", "stringValue": "Schedule"}, {"key":
-- "period", "stringValue": "1 hour"}, {"key": "endDateTime", "stringValue":
-- "2012-09-25T18:00:00"} ], "id": "Schedule", "name": "Schedule"}, {"fields":
-- [ {"key": "schedule", "refValue": "Schedule"}, {"key": "command",
-- "stringValue": "echo hello"}, {"key": "parent", "refValue": "Default"},
-- {"key": "type", "stringValue": "ShellCommandActivity"} ], "id": "SayHello",
-- "name": "SayHello"} ] }.
module Network.AWS.DataPipeline.GetPipelineDefinition where

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

data GetPipelineDefinition = GetPipelineDefinition
    { gpdipipelineId :: !Text
      -- ^ The identifier of the pipeline.
    , gpdiversion :: Maybe Text
      -- ^ The version of the pipeline definition to retrieve. This parameter accepts
      -- the values latest (default) and active. Where latest indicates the last
      -- definition saved to the pipeline and active indicates the last definition
      -- of the pipeline that was activated.
    } deriving (Eq, Show, Generic)

instance ToJSON GetPipelineDefinition

instance AWSRequest GetPipelineDefinition where
    type Er GetPipelineDefinition = DataPipelineError
    type Rs GetPipelineDefinition = GetPipelineDefinitionResponse
    request  = getJSON service
    response = responseJSON

data GetPipelineDefinitionResponse = GetPipelineDefinitionResponse
    { gpdirspipelineObjects :: [PipelineObject]
      -- ^ An array of objects defined in the pipeline.
    } deriving (Eq, Show, Generic)

instance FromJSON GetPipelineDefinitionResponse
