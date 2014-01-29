{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DataPipeline.PutPipelineDefinition
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds tasks, schedules, and preconditions that control the behavior of the
-- pipeline. You can use PutPipelineDefinition to populate a new pipeline.
-- PutPipelineDefinition also validates the configuration as it adds it to the
-- pipeline. Changes to the pipeline are saved unless one of the following
-- three validation errors exists in the pipeline. An object is missing a name
-- or identifier field. A string or reference field is empty. The number of
-- objects in the pipeline exceeds the maximum allowed objects. Pipeline
-- object definitions are passed to the PutPipelineDefinition action and
-- returned by the GetPipelineDefinition action. Example 1 This example sets
-- an valid pipeline configuration and returns success. POST / HTTP/1.1
-- Content-Type: application/x-amz-json-1.1 X-Amz-Target:
-- DataPipeline.PutPipelineDefinition Content-Length: 914 Host:
-- datapipeline.us-east-1.amazonaws.com X-Amz-Date: Mon, 12 Nov 2012 17:49:52
-- GMT Authorization: AuthParams {"pipelineId": "df-0937003356ZJEXAMPLE",
-- "pipelineObjects": [ {"id": "Default", "name": "Default", "fields": [
-- {"key": "workerGroup", "stringValue": "workerGroup"} ] }, {"id":
-- "Schedule", "name": "Schedule", "fields": [ {"key": "startDateTime",
-- "stringValue": "2012-12-12T00:00:00"}, {"key": "type", "stringValue":
-- "Schedule"}, {"key": "period", "stringValue": "1 hour"}, {"key":
-- "endDateTime", "stringValue": "2012-12-21T18:00:00"} ] }, {"id":
-- "SayHello", "name": "SayHello", "fields": [ {"key": "type", "stringValue":
-- "ShellCommandActivity"}, {"key": "command", "stringValue": "echo hello"},
-- {"key": "parent", "refValue": "Default"}, {"key": "schedule", "refValue":
-- "Schedule"} ] } ] } HTTP/1.1 200 x-amzn-RequestId:
-- f74afc14-0754-11e2-af6f-6bc7a6be60d9 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 18 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {"errored": false} Example 2 This example sets an invalid
-- pipeline configuration (the value for workerGroup is an empty string) and
-- returns an error message. POST / HTTP/1.1 Content-Type:
-- application/x-amz-json-1.1 X-Amz-Target: DataPipeline.PutPipelineDefinition
-- Content-Length: 903 Host: datapipeline.us-east-1.amazonaws.com X-Amz-Date:
-- Mon, 12 Nov 2012 17:49:52 GMT Authorization: AuthParams {"pipelineId":
-- "df-06372391ZG65EXAMPLE", "pipelineObjects": [ {"id": "Default", "name":
-- "Default", "fields": [ {"key": "workerGroup", "stringValue": ""} ] },
-- {"id": "Schedule", "name": "Schedule", "fields": [ {"key": "startDateTime",
-- "stringValue": "2012-09-25T17:00:00"}, {"key": "type", "stringValue":
-- "Schedule"}, {"key": "period", "stringValue": "1 hour"}, {"key":
-- "endDateTime", "stringValue": "2012-09-25T18:00:00"} ] }, {"id":
-- "SayHello", "name": "SayHello", "fields": [ {"key": "type", "stringValue":
-- "ShellCommandActivity"}, {"key": "command", "stringValue": "echo hello"},
-- {"key": "parent", "refValue": "Default"}, {"key": "schedule", "refValue":
-- "Schedule"} ] } ] } HTTP/1.1 200 x-amzn-RequestId:
-- f74afc14-0754-11e2-af6f-6bc7a6be60d9 Content-Type:
-- application/x-amz-json-1.1 Content-Length: 18 Date: Mon, 12 Nov 2012
-- 17:50:53 GMT {"__type":
-- "com.amazon.setl.webservice#InvalidRequestException", "message": "Pipeline
-- definition has errors: Could not save the pipeline definition due to FATAL
-- errors: [com.amazon.setl.webservice.ValidationError@108d7ea9] Please call
-- Validate to validate your pipeline"}.
module Network.AWS.DataPipeline.PutPipelineDefinition where

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
putPipelineDefinition :: Text
                      -> [PipelineObject]
                      -> AWS (Either DataPipelineError PutPipelineDefinitionResponse)
putPipelineDefinition p1 p2 = undefined $ PutPipelineDefinition
    { ppdiPipelineId = p1
    , ppdiPipelineObjects = p2
    }

data PutPipelineDefinition = PutPipelineDefinition
    { ppdiPipelineId :: !Text
      -- ^ The identifier of the pipeline to be configured.
    , ppdiPipelineObjects :: [PipelineObject]
      -- ^ The objects that define the pipeline. These will overwrite the existing
      -- pipeline definition.
    } deriving (Eq, Show, Generic)

instance ToJSON PutPipelineDefinition

instance AWSRequest PutPipelineDefinition where
    type Er PutPipelineDefinition = DataPipelineError
    type Rs PutPipelineDefinition = PutPipelineDefinitionResponse
    request  = getJSON service
    response = responseJSON

data PutPipelineDefinitionResponse = PutPipelineDefinitionResponse
    { ppdirsErrored :: !Bool
      -- ^ If True, there were validation errors. If errored is True, the pipeline
      -- definition is stored but cannot be activated until you correct the pipeline
      -- and call PutPipelineDefinition to commit the corrected pipeline.
    , ppdirsValidationErrors :: [ValidationError]
      -- ^ A list of the validation errors that are associated with the objects
      -- defined in pipelineObjects.
    , ppdirsValidationWarnings :: [ValidationWarning]
      -- ^ A list of the validation warnings that are associated with the objects
      -- defined in pipelineObjects.
    } deriving (Eq, Show, Generic)

instance FromJSON PutPipelineDefinitionResponse
